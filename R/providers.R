# -------------------------------------------------------------------------
# Provider lookup by ZIP code
# -------------------------------------------------------------------------

#' Find CDC HIV-related service providers by ZIP code
#'
#' Queries the CDC Locator API to return service providers within a given
#' radius of a ZIP code.
#'
#' @param zipcode Character scalar. ZIP code.
#' @param radius Integer search radius in miles (1–50, default: 10).
#' @param services Optional character vector of service enums.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return A tibble of service providers.
#' @export
cdc_providers <- function(zipcode,
                          radius = 10,
                          services = NULL,
                          client_id = NULL,
                          client_token = NULL) {

  # ---- argument validation ------------------------------------------------
  if (!is.character(zipcode) || length(zipcode) != 1 || !nzchar(zipcode)) {
    cli::cli_abort("{.arg zipcode} must be a non-empty character scalar.")
  }

  if (!is.numeric(radius) || length(radius) != 1) {
    cli::cli_abort("{.arg radius} must be a single numeric value.")
  }

  radius <- as.integer(radius)

  if (radius < 1 || radius > 50) {
    cli::cli_abort("{.arg radius} must be between 1 and 50 miles.")
  }

  .validate_cdc_locator_services(services)

  # ---- build query --------------------------------------------------------
  query <- list(
    zipcode = zipcode,
    radius  = radius,
    services = if (!is.null(services)) paste(services, collapse = ",")
  ) |>
    purrr::compact()

  # ---- build + perform request --------------------------------------------
  req <- .cdc_locator_request(
    endpoint = "providers",
    query = query,
    client_id = client_id,
    client_token = client_token
  )

  body <- .perform_cdc_locator_request(req)

  # ---- normalize response -------------------------------------------------
  tibble::as_tibble(body)
}
