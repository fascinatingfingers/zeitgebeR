
#' Fit Brightness and Color Temperature Models
#'
#' @param dta training dataset, like that returned by
#'   \code{\link{training_data}}
#'
#' @return Returns a named list with two models, one for brightness and one for
#'   color temperature.
#'
#' @export
fit_models <- function(dta = training_data()) {
    dta$bri <- (dta$bri + 1) / (254 + 1)
    dta$ct <- (dta$ct - 153) / (500 - 153)

    bri <- stats::glm(
        bri ~ room + role + group +
            splines::bs(solar_elevation, df = 3) +
            solar_azimuth_x + solar_azimuth_y +
            astronomical_daylight + nautical_daylight + civil_daylight +
            awake_hours + working_hours +
            visibility + cloud_cover
        ,
        family = stats::quasibinomial,
        data = dta,
        weights = dta$weight
    )

    ct <- stats::glm(
        ct ~ room + role + group +
            splines::bs(solar_elevation, df = 3) +
            solar_azimuth_x + solar_azimuth_y +
            astronomical_daylight + nautical_daylight + civil_daylight +
            awake_hours + working_hours +
            visibility + cloud_cover
        ,
        family = stats::quasibinomial,
        data = dta,
        weights = dta$weight
    )

    return(list(bri = bri, ct = ct))
}
