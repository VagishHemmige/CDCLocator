# -------------------------------------------------------------------------
# Provider lookup by ZIP code
# -------------------------------------------------------------------------

#' Find CDC HIV-related service providers by ZIP code
#'
#' Queries the CDC Locator API to return service providers within a given
#' radius of a ZIP code.
#'
#' The API returns providers offering any of the requested services, but
#' returned no providers when all 14 services were requested at once (observed
#' 2026-11-16). So services are requested at most three at a time, and the
#' results are combined: the default, every service, takes five requests.
#' Providers returned by more than one request are kept once, by `id`.
#'
#' @param zipcode Character scalar. ZIP code.
#' @param radius Integer search radius in miles (1–50, default: 10).
#' @param services Character vector of services to search for, matched in
#'   any case: `"hivtesting"`, `"stdtesting"`, `"selftesting"`,
#'   `"preptesting"`, `"pep"`, `"condom"`, `"hepc"`, `"doxypep"`,
#'   `"mailintesting"`, `"ryanwhite"`, `"clinics"`, `"substanceabuse"`,
#'   `"mentalhealth"` or `"housing"`. `NULL`, the default, searches for all
#'   of them. More than three services take more than one request.
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

  # ---- one request per batch of up to three services -----------------------
  batches <- split(
    services,
    ceiling(seq_along(services) / .cdc_locator_batch_size)
  )
  caller <- environment()
  results <- lapply(batches, \(batch) {
    .cdc_providers_request(
      zipcode,
      radius,
      batch,
      client_id,
      client_token,
      call = caller
    )
  })

  # An empty batch has a different `attributes` type from a populated one, so
  # bind only populated batches; if none are, return the empty schema.
  populated <- Filter(\(x) nrow(x) > 0, results)
  if (length(populated) == 0) {
    return(results[[1]])
  }
  out <- dplyr::bind_rows(unname(populated))
  out[!duplicated(out$id), ]
}

# The most services sent in one request. All 14 at once returned no providers;
# up to three returned the union of their single-service results.
.cdc_locator_batch_size <- 3

.cdc_providers_request <- function(
  zipcode,
  radius,
  services,
  client_id,
  client_token,
  call
) {
  # The API requires radius and services (observed 2026-11-16).
  query <- list(
    zipcode = zipcode,
    radius = radius,
    services = paste(services, collapse = ",")
  )

  req <- .cdc_locator_request(
    endpoint = "providers",
    query = query,
    client_id = client_id,
    client_token = client_token
  )

  body <- .perform_cdc_locator_request(req, call = call)

  # No matching providers comes back as an empty JSON array, which parses to
  # an unnamed empty list. Providers parse to a data frame. Anything else
  # (null, an object) is not a provider list and must not look like "none".
  if (is.list(body) && length(body) == 0 && is.null(names(body))) {
    return(tibble::tibble(
      type = character(),
      id = character(),
      attributes = list()
    ))
  }

  if (!is.data.frame(body)) {
    cli::cli_abort(
      c(
        "The CDC Locator API returned an unexpected response.",
        "i" = "Expected a JSON array of providers."
      ),
      class = "cdclocator_unexpected_response",
      call = call
    )
  }

  # Providers are combined across batches by `id`, so every record needs one.
  ids <- body$id
  if (is.null(ids) || any(is.na(ids)) || !all(nzchar(as.character(ids)))) {
    cli::cli_abort(
      c(
        "The CDC Locator API returned an unexpected response.",
        "i" = "Every provider should have a non-empty {.field id}."
      ),
      class = "cdclocator_unexpected_response",
      call = call
    )
  }

  tibble::as_tibble(body)
}
