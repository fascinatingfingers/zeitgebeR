
#' Configure Environment
#'
#' These functions are used specifically for their side effects -- namely to
#' set (or reset) authentication secrets and configuration details stored in
#' \code{options()}.
#'
#' @param hue_bridge_ip the IP address of your Hue Bridge
#' @param hue_username the username with access to your Hue Bridge
#' @param darksky_key Dark Sky secret key
#' @param lat latitude (in decimal degrees). Positive north; negative south.
#' @param lon longitude (in decimal degrees). Positive east; negative west.
#' @param hue_storage_path path to directory where Hue training data are stored
#' @param darksky_storage_path path to directory where Dark Sky weather data are
#'   stored
#'
#' @return Returns \code{TRUE} (invisibly) if options were successfully set or
#'   reset.
#'
#' @seealso \code{\link[PhilipsHue]{set_bridge_credentials}}
#' @seealso \code{\link[DarkSky]{set_dark_sky_credentials}}
#'
#' @export
set_configuration <- function(
    hue_bridge_ip, hue_username, darksky_key, lat, lon,
    hue_storage_path, darksky_storage_path
) {
    if (length(hue_storage_path) != 1L || !is.character(hue_storage_path)) {
        stop('Please specify `hue_storage_path` as a single character value')
    }

    if (length(darksky_storage_path) != 1L || !is.character(darksky_storage_path)) {
        stop('Please specify `darksky_storage_path` as a single character value')
    }

    if (!dir.exists(hue_storage_path)) {dir.create(hue_storage_path, recursive = TRUE)}
    if (!dir.exists(darksky_storage_path)) {dir.create(darksky_storage_path, recursive = TRUE)}

    PhilipsHue::set_bridge_credentials(hue_bridge_ip, hue_username)
    DarkSky::set_dark_sky_credentials(darksky_key, lat, lon)

    options(zeitgebeR = list(
        hue_storage_path = hue_storage_path,
        darksky_storage_path = darksky_storage_path
    ))

    return(invisible(TRUE))
}

#' @rdname set_configuration
#' @export
reset_configuration <- function() {
    PhilipsHue::reset_bridge_credentials()
    DarkSky::reset_dark_sky_credentials()
    options(zeitgebeR = list())
}
