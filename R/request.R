# -------------------------------------------------------------------------
# Internal request helper for CDC Locator API
# -------------------------------------------------------------------------

#' Build an httr2 request for the CDC Locator API
#'
#' This is the single entry point for all API requests in the package.
#'
#' @param endpoint Character scalar. API endpoint path (e.g. "providers").
#' @param query Named list of query parameters.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return An httr2 request object.
#' @keywords internal
.cdc_locator_request <- function(endpoint,
                                 query = list(),
                                 client_id = NULL,
                                 client_token = NULL) {

  # Defensive checks (cheap and helpful)
  stopifnot(
    is.character(endpoint),
    length(endpoint) == 1
  )

  if (!is.list(query)) {
    cli::cli_abort("{.arg query} must be a named list.")
  }

  httr2::request("https://locator-api.hiv.gov") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(!!!.build_auth_headers(
      client_id = client_id,
      client_token = client_token
    )) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_timeout(30)
}

#' Perform a CDC Locator API request and return parsed JSON
#'
#' Centralizes response checking and JSON parsing.
#'
#' @param req An httr2 request object.
#'
#' @return Parsed JSON content (simplified).
#' @keywords
