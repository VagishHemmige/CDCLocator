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
#' @param services Character vector of services to search for, matched in
#'   any case: `"hivtesting"`, `"stdtesting"`, `"selftesting"`,
#'   `"preptesting"`, `"pep"`, `"condom"`, `"hepc"`, `"doxypep"`,
#'   `"mailintesting"`, `"ryanwhite"`, `"clinics"`, `"substanceabuse"`,
#'   `"mentalhealth"` or `"housing"`. `NULL`, the default, searches for all
#'   of them.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return A tibble of service providers.
#' @export
cdc_providers <- function(
  zipcode,
  radius = 10,
  services = NULL,
  client_id = NULL,
  client_token = NULL
) {
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

  services <- .validate_cdc_locator_services(services)

  # ---- build query --------------------------------------------------------
  # The API requires radius and services (observed 2026-11-16), so both are
  # always sent; NULL services means every service.
  query <- list(
    zipcode = zipcode,
    radius = radius,
    services = paste(services, collapse = ",")
  )

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
