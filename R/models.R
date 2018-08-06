
#' Fit Brightness and Color Temperature Models
#'
#' After recording data and preparing a dataset with \code{\link{training_data}},
#' the \code{fit_models} function will fit two generalized linear models: one
#' to predict brightness for the given features, and the other to predict color
#' temperature.
#'
#' @param dta training dataset, like that returned by
#'   \code{\link{training_data}}
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

#' Predict the ideal state
#'
#' @param models the list of models to predict from, like those returned by
#'   \code{\link{fit_models}}
#' @param state the current Hue state, as returned by
#'   \code{\link{parse_hue_state}}
#' @param weather current weather, as returned by \code{\link{parse_weather}}
#'
#' @return Returns a \code{\link[dplyr]{data_frame}} of light information
#'   including \code{bri} and \code{ct} values predicted from the supplied
#'   models.
#'
#' @export
predict_from_models <- function(models, state, weather) {

    # Initialize return dataset
    re <- '^(.+)/(Ambient|Accent|Task)/(.+)$'
    y <- dplyr::data_frame(
        datetime = unique(weather$datetime),
        room = factor(
            sub(re, '\\1', state$light_name),
            levels = levels(models$bri$data$room)
        ),
        role = factor(
            sub(re, '\\2', state$light_name),
            levels = levels(models$bri$data$role)
        ),
        group = factor(
            sub(' ?\\d+', '', sub(re, '\\3', state$light_name)),
            levels = levels(models$bri$data$group)
        ),
        light_name = factor(
            state$light_name,
            levels = levels(models$bri$data$name)
        ),
        light_id = state$light_id
    )
    rm(re)

    # Add zeitgeber and weather features
    y <- dplyr::right_join(
        dplyr::bind_cols(
            zeitgeber(unique(y$datetime)),
            weather[, c('visibility', 'cloud_cover')]
        ),
        y,
        by = 'datetime'
    )

    # Add bri & ct predictions
    y$bri <- as.numeric(suppressWarnings(
        stats::predict(models$bri, type = 'response', newdata = y)
    ))
    y$ct <- as.numeric(suppressWarnings(
        stats::predict(models$ct, type = 'response', newdata = y)
    ))

    # Unscale
    y$bri <- unscale_bri(y$bri)
    y$ct <- unscale_ct(y$ct)

    # Fin
    return(y)
}

#' Scale and unscale brightness and color temperature
#'
#' The quasibinomial models created by \code{\link{fit_models}} produce
#' estimates in the range [0, 1], but brightness and color temperature are in
#' the ranges [1, 254] and [153, 500], respectively. The following functions are
#' provided to help convert to and from the model scale.
#'
#' @param bri the brightness value to scale or unscale
#' @param ct the color temperature value to scale or unscale
#'
#' @return Returns scaled (i.e. [0, 1]) values or unscaled (brightness in
#'   [1, 254]; color temperature in [153, 500]) values.
#'
#' @name scale

#' @rdname scale
#' @export
scale_bri <- function(bri) {
    y <- (bri - 1) / (254 - 1)
    y[(y < 0) %in% TRUE] <- 0
    y[(y > 1) %in% TRUE] <- 1
    return(y)
}

#' @rdname scale
#' @export
unscale_bri <- function(bri) {
    y <- bri * (254 - 1) + 1
    y[(y < 1) %in% TRUE] <- 1
    y[(y > 254) %in% TRUE] <- 254
    return(y)
}

#' @rdname scale
#' @export
scale_ct <- function(ct) {
    y <- (ct - 153) / (500 - 153)
    y[(y < 0) %in% TRUE] <- 0
    y[(y > 1) %in% TRUE] <- 1
    return(y)
}

#' @rdname scale
#' @export
unscale_ct <- function(ct) {
    y <- ct * (500 - 153) + 153
    y[(y < 153) %in% TRUE] <- 153
    y[(y > 500) %in% TRUE] <- 500
    return(y)
}
