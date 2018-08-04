
#' Fit Brightness and Color Temperature Models
#'
#' After recording data and preparing a dataset with \code{\link{training_data}},
#' the \code{fit_models} function will fit two generalized linear models: one
#' to predict brightness for the given features, and the other to predict color
#' temperature.
#'
#' Note that quasibinomial models produce estimates in the range [0, 1], but
#' brightness and color temperature are in the ranges [1, 254] and [153, 500],
#' respectively. Scale/unscale functions are provided to help convert to and
#' from the model scale.
#'
#' @param dta training dataset, like that returned by
#'   \code{\link{training_data}}
#' @param bri the brightness value to scale or unscale
#' @param ct the color temperature value to scale or unscale
#'
#' @return Returns a named list with two models, one for brightness and one for
#'   color temperature.
#'
#' @export
fit_models <- function(dta = training_data()) {
    dta$bri <- scale_bri(dta$bri)
    dta$ct <- scale_ct(dta$ct)

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

#' @rdname fit_models
#' @export
scale_bri <- function(bri) {(bri - 1) / (254 - 1)}

#' @rdname fit_models
#' @export
unscale_bri <- function(bri) {bri * (254 - 1) + 1}

#' @rdname fit_models
#' @export
scale_ct <- function(ct) {(ct - 153) / (500 - 153)}

#' @rdname fit_models
#' @export
unscale_ct <- function(ct) {ct * (500 - 153) + 153}
