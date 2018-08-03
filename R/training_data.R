
#' Functions to record and manipulate training data
#'
#' @param cache_timeout time (in minutes) to cache results before hitting the
#'   API again
#' @param path path to file, typically a file created by
#'   \code{\link{record_hue_state}} or \code{\link{record_weather}}
#' @param limit sets a cap on the number of files to be processed at a time
#'
#' @return Returns `invisible(TRUE)` upon success.
#'
#' @name training_data

#' @rdname training_data
#' @export
record_hue_state <- function() {
    state <- PhilipsHue::get_state()
    path <- file.path(
        getOption('zeitgeber')$hue_storage_path,
        sprintf(
            '%s_%s_state.yaml',
            format(Sys.time(), '%Y%m%d%H%M%S'),
            digest::digest(state, algo = 'crc32')
        )
    )

    write(yaml::as.yaml(state), path)

    return(invisible(TRUE))
}

#' @rdname training_data
#' @export
record_weather <- function(cache_timeout = 15) {

    last_update_time <- dir(getOption('zeitgeber')$darksky_storage_path)
    last_update_time <- sub('^(\\d{14}).*$', '\\1', last_update_time)
    last_update_time <- as.POSIXct(last_update_time, format = '%Y%m%d%H%M%S')
    last_update_time <- if (length(last_update_time) == 0) {NA} else {max(last_update_time)}

    needs_update <- (
        is.na(last_update_time) ||
        difftime(Sys.time(), last_update_time, units = 'mins') > cache_timeout
    )

    if (needs_update) {
        weather <- DarkSky::get_forecast()
        path <- file.path(
            getOption('zeitgeber')$darksky_storage_path,
            sprintf(
                '%s_%s_weather.yaml',
                format(Sys.time(), '%Y%m%d%H%M%S'),
                digest::digest(weather, algo = 'crc32')
            )
        )

        write(yaml::as.yaml(weather), path)
    }

    return(invisible(TRUE))
}

#' @rdname training_data
#' @export
record_data <- function(cache_timeout = 15) {
    return(invisible(all(record_hue_state(), record_weather(cache_timeout))))
}

#' @rdname training_data
#' @export
parse_state <- function(path) {
    x <- suppressWarnings(yaml::yaml.load_file(path))

    # Create room_name/light_id lookup table
    rooms <- purrr::keep(x$groups, ~ .$type %in% 'Room')
    rooms <- dplyr::bind_rows(purrr::map(
        rooms,
        ~ dplyr::data_frame(
            room_name = .$name,
            light_id = .$lights
        )
    ))

    if (nrow(rooms) == 0) {return(NULL)}

    # Get light states
    lights <- dplyr::bind_rows(purrr::map2(
        names(x$lights),
        x$lights,
        ~ dplyr::data_frame(
            room_name = rooms$room_name[match(.x, rooms$light_id)],
            light_unique_id = .y$uniqueid,
            light_id = .x,
            light_name = .y$name,
            reachable = .y$state$reachable,
            on = .y$state$on,
            bri = .y$state$bri,
            ct = .y$state$ct,
        )
    ))

    if (nrow(rooms) == 0) {return(NULL)}

    # Get scenes, filter out multi-room scenes
    scenes <- dplyr::bind_rows(purrr::map(
        x$scenes,
        function(scene) {
            y <- purrr::map(scene$lightstates, as.data.frame)
            y <- purrr::map2(names(y), y, ~ dplyr::mutate(.y, light_id = .x))
            y <- dplyr::bind_rows(y)
            y <- dplyr::mutate(y, scene_name = scene$name)
            return(dplyr::tbl_df(y))
        }
    ))

    if (nrow(scenes) > 0) {
        scenes <- dplyr::left_join(scenes, rooms, by = 'light_id')
        scenes <- split(scenes, scenes$scene_name)
        scenes <- purrr::keep(scenes, ~ length(unique(.$room_name)) == 1)
        scenes <- dplyr::bind_rows(scenes)
    }

    if (nrow(scenes) > 0) {
        # Deduce whether current state matches a scene
        scene_matches <- dplyr::left_join(
            scenes, lights,
            by = c('room_name', 'light_id'),
            suffix = c('.scene', '.actual')
        )
        scene_matches <- scene_matches[
            scene_matches$reachable %in% TRUE &
            scene_matches$on.scene %in% TRUE &
            scene_matches$on.actual %in% TRUE,
        ]

        scene_matches <- split(
            scene_matches,
            with(scene_matches, list(room_name, scene_name)),
            drop = TRUE
        )
        scene_matches <- dplyr::bind_rows(purrr::map(scene_matches, function(x) {
            bri.diff <- x$bri.actual - x$bri.scene
            ct.diff <- x$ct.actual - x$ct.scene
            x$scene_rmse <- sqrt(mean(c(bri.diff^2, ct.diff^2), na.rm = TRUE))

            return(x)
        }))
    }

    if (exists('scene_matches') && nrow(scene_matches) > 0) {
        scene_matches <- split(scene_matches, scene_matches$room_name)
        scene_matches <- dplyr::bind_rows(purrr::map(scene_matches, function(x) {
            x[x$scene_rmse == min(x$scene_rmse), ]
        }))

        scene_matches <- scene_matches[, c('room_name', 'light_id', 'scene_name', 'scene_rmse')]

        y <- dplyr::left_join(lights, scene_matches, by = c('room_name', 'light_id'))
    } else {
        y <- lights
        y$scene_name = NA
        y$scene_rmse = NA
    }

    # Prepare return table
    y$datetime <- as.POSIXct(x$config$localtime, tz = x$config$timezone, format = '%Y-%m-%dT%H:%M:%S')
    y <- y[, c(
        'datetime',
        'room_name', 'light_unique_id', 'light_id', 'light_name',
        'reachable', 'on', 'bri', 'ct',
        'scene_name', 'scene_rmse'
    )]

    # Fin!
    return(y)
}

#' @rdname training_data
#' @export
parse_weather <- function(path) {
    x <- suppressWarnings(yaml::yaml.load_file(path))

    dplyr::data_frame(
        datetime = as.POSIXct(x$currently$time, origin = '1970-01-01'),
        visibility = as.numeric(x$currently$visibility),
        cloud_cover = as.numeric(x$currently$cloudCover)
    )
}

#' @rdname training_data
#' @export
archive_data <- function(limit = 100) {

    # state
    message('Archiving state...')

    files <- dir(
        getOption('zeitgeber')$hue_storage_path,
        pattern = '^(\\d{14})_\\w+_state.yaml$',
        full.names = TRUE
    )

    if (limit > 0 && length(files) > limit) {files <- utils::head(files, limit)}

    rds_file <- file.path(getOption('zeitgeber')$hue_storage_path, 'state.rds')
    zip_file <- file.path(getOption('zeitgeber')$hue_storage_path, 'archive.zip')

    state <- dplyr::bind_rows(purrr::map(files, parse_state))

    if (file.exists(rds_file)) {
        archive <- readRDS(rds_file)
        archive <- dplyr::mutate_if(archive, is.factor, as.character)
        state <- dplyr::bind_rows(archive, state)
        rm(archive)
    }

    state <- dplyr::mutate_if(state, is.character, as.factor)

    saveRDS(state, rds_file)
    utils::zip(zip_file, files, flags = '-r9XmD')
    rm(files, rds_file, zip_file, state)

    # weather
    message('Archiving weather...')

    files <- dir(
        getOption('zeitgeber')$darksky_storage_path,
        pattern = '^(\\d{14})_\\w+_weather.yaml$',
        full.names = TRUE
    )

    if (limit > 0 && length(files) > limit) {files <- utils::head(files, limit)}

    rds_file <- file.path(getOption('zeitgeber')$darksky_storage_path, 'weather.rds')
    zip_file <- file.path(getOption('zeitgeber')$darksky_storage_path, 'archive.zip')

    weather <- dplyr::bind_rows(purrr::map(files, parse_weather))

    if (file.exists(rds_file)) {
        archive <- readRDS(rds_file)
        archive <- dplyr::mutate_if(archive, is.factor, as.character)
        weather <- dplyr::bind_rows(archive, weather)
        rm(archive)
    }

    weather <- dplyr::mutate_if(weather, is.character, as.factor)

    saveRDS(weather, rds_file)
    utils::zip(zip_file, files, flags = '-r9XmD')
    rm(files, rds_file, zip_file, weather)

    # fin!
    return(invisible(TRUE))
}

#' @rdname training_data
#' @export
training_data <- function() {

    # Load source data
    state_path <- file.path(getOption('zeitgeber')$hue_storage_path, 'state.rds')
    weather_path <- file.path(getOption('zeitgeber')$darksky_storage_path, 'weather.rds')

    if (!file.exists(state_path) | !file.exists(weather_path)) {
        stop('Create training data with `record_data()`, then call `archive_data()`')
    }

    state <- readRDS(state_path)
    weather <- readRDS(weather_path)

    rm(state_path, weather_path)

    # Initialize return dataset
    y <- state

    # Update room, role, and group
    re <- '^(.+)/(Ambient|Accent|Task)/(.+)$'
    lights <- PhilipsHue::get_lights()
    lights <- dplyr::bind_rows(purrr::map(lights, ~
        dplyr::data_frame(light_unique_id = .$uniqueid, name = .$name)))
    lights$room <- sub(re, '\\1', lights$name)
    lights$role <- sub(re, '\\2', lights$name)
    lights$group <- sub(' ?\\d+', '', sub(re, '\\3', lights$name))
    lights$room <- factor(lights$room, levels = names(sort(table(lights$room), decreasing = TRUE)))
    lights$role <- factor(lights$role, levels = c('Ambient', 'Accent', 'Task'))
    lights$group <- factor(lights$group, levels = names(sort(table(lights$group), decreasing = TRUE)))
    lights$light_unique_id <- factor(lights$light_unique_id, levels = levels(y$light_unique_id))
    lights$name <- factor(lights$name)
    lights <- lights[, c('room', 'role', 'group', 'name', 'light_unique_id')]
    y <- dplyr::right_join(
        lights,
        y[, setdiff(names(y), c('room_name', 'light_name'))],
        by = 'light_unique_id'
    )
    rm(re, lights)

    # Flag rooms set to default scene; exclude rooms set to non-default scene
    y$default_scene <- (
        grepl('default', y$scene_name, ignore.case = TRUE) %in% TRUE &
        (y$scene_rmse <= 17) %in% TRUE
    )
    y <- y[is.na(y$scene_name) | grepl('default', y$scene_name, ignore.case = TRUE), ]

    # Filter down to lights that were on; deduplicate
    y <- y[y$reachable %in% TRUE & y$on %in% TRUE, ]
    y <- dplyr::group_by_at(y, c('room', 'role', 'group', 'name', 'datetime'))
    y <- dplyr::summarise_at(y, c('bri', 'ct', 'default_scene'), mean, na.rm = TRUE)
    y <- dplyr::ungroup(y)
    y <- y[stats::complete.cases(y), ]
    y$default_scene <- as.logical(y$default_scene)
    y$bri[y$bri < 1] <- 1; y$bri[y$bri > 254] <- 254
    y$ct[y$ct < 153] <- 153; y$ct[y$ct > 500] <- 500

    # Add zeitgeber and weather features
    z <- zeitgeber(unique(y$datetime))
    z <- z[, setdiff(names(z), c('time_of_day', 'weekend'))]
    index <- purrr::map_int(z$datetime, ~ which.min(abs(. - weather$datetime)))
    y <- dplyr::right_join(
        dplyr::bind_cols(z, weather[index, c('visibility', 'cloud_cover')]),
        y, by = 'datetime'
    )
    rm(z, index)

    # Calculate time in state, by room
    y <- y[order(y$datetime, y$room, y$role, y$group, y$name), ]
    y <- split(y, list(y$room, y$datetime), drop = TRUE)
    y <- dplyr::bind_rows(purrr::map(y, function(x) {
        x$hash <- digest::digest(cbind(x$bri, x$ct))
        return(x)
    }))
    y <- split(y, list(cumsum(as.numeric((y$hash == dplyr::lag(y$hash)) %in% c(NA, FALSE)))))
    y <- dplyr::bind_rows(purrr::map(y, function(x) {
        x$time_in_state <- as.numeric(difftime(x$datetime, min(x$datetime), units = 'mins'))
        return(x)
    }))
    y <- y[, setdiff(names(y), 'hash')]

    # Weight observations
    y$weight <- (

        # Decay all observations (half-life: 4 years)
        (0.5 ^ (as.numeric(difftime(max(y$datetime), y$datetime, units = 'days')) / (4 * 365.2422))) *

        # Further decay observations that match default scene (half-life: 4 weeks)
        (0.5 ^ (y$default_scene * as.numeric(difftime(Sys.time(), y$datetime, units = 'weeks')) / (5))) *

        # Further decay sustained observations (half-life: 30 minutes)
        (0.5 ^ (y$time_in_state / (30)))
    )
    y <- y[, setdiff(names(y), c('default_scene', 'time_in_state'))]

    # Fin!
    return(y)
}
