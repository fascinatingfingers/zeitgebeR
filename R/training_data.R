
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
