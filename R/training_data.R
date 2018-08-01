
#' Functions to record and manipulate training data
#'
#' @param cache_timeout time (in minutes) to cache results before hitting the
#'   API again
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
