# -------------------------------------------------------------------------
# Address translation: city / state -> ZIP code
# -------------------------------------------------------------------------

#' Translate city and state to a ZIP code
#'
#' Uses the CDC Locator API Place Translator endpoint to convert a city and
#' state into a ZIP code.
#'
#' @param city Character scalar. City name.
#' @param state Character scalar. State name or postal abbreviation.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return A tibble with city, state, zipcode, latitude, and longitude.
#' @export
cdc_address_place <- function(city,
                              state,
                              client_id = NULL,
                              client_token = NULL) {

  # ---- argument validation ------------------------------------------------
  if (!is.character(city) || length(city) != 1 || !nzchar(city)) {
    cli::cli_abort("{.arg city} must be a non-empty character scalar.")
  }

  if (!is.character(state) || length(state) != 1 || !nzchar(state)) {
    cli::cli_abort("{.arg state} must be a non-empty character scalar.")
  }

  # ---- build + perform request --------------------------------------------
  req <- .cdc_locator_request(
    endpoint = "address/place",
    query = list(
      city  = city,
      state = state
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
