
#' Create Table of Time Cues
#'
#' Zeitgeber, Geman for *time-giver*, are external cues that help
#' synchronize an organism's biolgical rhythms to Earth's day/night cycles and
#' seasons. This eponymous function takes a vector of date/time values (plus
#' latitude/longitude values retrieved from the session options; see Details)
#' and returns a [dplyr::data_frame()] of zeitgeber features.
#'
#' This function retrieves location information saved in
#' `options()$DarkSky`. Before calling this function, set your credentials
#' and other configuration details with a call to `set_configuration`.
#'
#' @param time vector of date/time values
#' @param lat latitude (in decimal degrees). Positive north; negative south.
#' @param lon longitude (in decimal degrees). Positive east; negative west.
#'
#' @return Returns a data_frame with the following diurnal features:
#'   \itemize{
#'     \item `datetime`
#'     \item `solar_elevation` (in degrees)
#'     \item `solar_azimuth` (in degrees)
#'     \item `solar_azimuth_x` east--west movement, scaled \[-1, 1\]
#'     \item `solar_azimuth_y` north--south movement, scaled \[-1, 1\]
#'     \item `civil_daylight` `TRUE` when the solar elevation is
#'       greater than -6°
#'     \item `nautical_daylight` `TRUE` when the solar elevation is
#'       greater than -12°
#'     \item `astronomical_daylight` `TRUE` when the solar elevation
#'       is greater than -18°
#'     \item `working_hours` `TRUE` from 9a--5p; 10a--6p on weekends
#'     \item `awake_hours` `TRUE` from 8a--12a; 9a--1a on weekends
#'   }
#'
#' @encoding UTF-8
#'
#' @export
zeitgeber <- function(time = Sys.time(), lat = getOption('DarkSky')$lat, lon = getOption('DarkSky')$lon) {

    if (is.null(lat) || is.null(lon)) {
        stop('Configure your location by calling `DarkSky::set_dark_sky_credentials`')
    }

    scale_period <- function(x, min, max) {
        m <- mean(c(min, max)); s <- (max - min) / 4
        stats::dnorm(x, m, s) / stats::dnorm(stats::qnorm(0.5, m, s), m, s)
    }

    time_of_day <- lubridate::hour(time) + lubridate::minute(time) / 60
    weekend <- lubridate::wday(time, label = TRUE) %in% c('Sat', 'Sun') # TODO parameterize

    solar_position <- maptools::solarpos(matrix(c(lon, lat), nrow = 1), time)
    solar_elevation <- solar_position[, 2]
    solar_azimuth <- solar_position[, 1]

    dplyr::data_frame(
        datetime = time,
        solar_elevation = solar_elevation,
        solar_azimuth = solar_azimuth,
        solar_azimuth_x = cos(solar_azimuth),
        solar_azimuth_y = sin(solar_azimuth),
        civil_daylight = solar_elevation > -6,
        nautical_daylight = solar_elevation > -12,
        astronomical_daylight = solar_elevation > -18,
        working_hours = scale_period(time_of_day - weekend, 09, 18), # TODO parameterize
        awake_hours = scale_period(time_of_day - weekend, 08, 24) # TODO parameterize
    )
}
