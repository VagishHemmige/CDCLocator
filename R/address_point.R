# -------------------------------------------------------------------------
# Address translation: latitude / longitude -> ZIP code
# -------------------------------------------------------------------------

#' Translate latitude and longitude to a ZIP code
#'
#' Uses the CDC Locator API Point Translator endpoint to convert geographic
#' coordinates into a ZIP code.
#'
#' @param latitude Numeric latitude.
#' @param longitude Numeric longitude.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return A tibble with city, state, zipcode, latitude, and longitude.
#' @export
cdc_address_point <- function(latitude,
                              longitude,
                              client_id = NULL,
                              client_token = NULL) {

  # ---- argument validation ------------------------------------------------
  if (!is.numeric(latitude) || length(latitude) != 1) {
    cli::cli_abort("{.arg latitude} must be a single numeric value.")
  }

  if (!is.numeric(longitude) || length(longitude) != 1) {
    cli::cli_abort("{.arg longitude} must be a single numeric value.")
  }

  # ---- build + perform request --------------------------------------------
  req <- .cdc_locator_request(
    endpoint = "address/point",
    query = list(
      latitude  = latitude,
      longitude = longitude
    ),
    client_id = client_id,
    client_token = client_token
  )

  body <- .perform_cdc_locator_request(req)

  # ---- normalize response -------------------------------------------------
  tibble::tibble(
    city      = body$city,
    state     = body$state,
    zipcode   = body$zipcode,
    latitude  = body$point$latitude,
    longitude = body$point$longitude
  )
}
