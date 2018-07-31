
#' Create Table of Time Cues
#'
#' @param time time
#' @param lat latitude
#' @param lon longitude
#'
#' @return Returns a dataframe with `length(x)` rows and columns of diurnal
#'   features
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
        time_of_day = time_of_day,
        solar_elevation = solar_elevation,
        solar_azimuth = solar_azimuth,
        solar_azimuth_x = cos(solar_azimuth),
        solar_azimuth_y = sin(solar_azimuth),
        civil_daylight = solar_elevation > -6,
        nautical_daylight = solar_elevation > -12,
        astronomical_daylight = solar_elevation > -18,
        weekend = weekend,
        working_hours = scale_period(time_of_day - weekend, 09, 18), # TODO parameterize
        awake_hours = scale_period(time_of_day - weekend, 08, 24) # TODO parameterize
    )
}
