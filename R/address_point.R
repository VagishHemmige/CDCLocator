# -------------------------------------------------------------------------
# Address translation: latitude / longitude -> ZIP code
# -------------------------------------------------------------------------

#' Translate latitude and longitude to a ZIP code
#'
#' Uses the CDC Locator API Point Translator endpoint to convert geographic
#' coordinates into a ZIP code.
#'
#' @param latitude Latitude in decimal degrees, from -90 to 90.
#' @param longitude Longitude in decimal degrees, from -180 to 180. Places in
#'   the US have negative longitudes.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return A tibble with city, state, zipcode, latitude, and longitude.
#' @export
cdc_address_point <- function(
  latitude,
  longitude,
  client_id = NULL,
  client_token = NULL
) {
  # ---- argument validation ------------------------------------------------
  .check_number(latitude, "latitude", min = -90, max = 90, call = environment())
  .check_number(
    longitude,
    "longitude",
    min = -180,
    max = 180,
    call = environment()
  )

  # ---- build + perform request --------------------------------------------
  req <- .cdc_locator_request(
    endpoint = "address/point",
    query = list(
      latitude = latitude,
      longitude = longitude
    ),
    client_id = client_id,
    client_token = client_token
  )

  body <- .perform_cdc_locator_request(req)

  # ---- normalize response -------------------------------------------------
  tibble::tibble(
    city = body$city,
    state = body$state,
    zipcode = body$zipcode,
    latitude = body$point$latitude,
    longitude = body$point$longitude
  )
}
