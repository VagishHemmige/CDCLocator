# -------------------------------------------------------------------------
# Internal request helpers for CDC Locator API
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
#' @noRd
.cdc_locator_request <- function(
  endpoint,
  query = list(),
  client_id = NULL,
  client_token = NULL
) {
  # Defensive checks (cheap and helpful)
  stopifnot(
    is.character(endpoint),
    length(endpoint) == 1
  )

  if (!is.list(query)) {
    cli::cli_abort("{.arg query} must be a named list.")
  }

  auth <- .build_auth_headers(
    client_id = client_id,
    client_token = client_token
  )

  httr2::request("https://locator-api.hiv.gov") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(!!!auth, .redact = names(auth)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(.cdc_locator_user_agent()) |>
    httr2::req_timeout(30) |>
    # 429 and 503 only; the API reports bad input as 500, which must not retry.
    httr2::req_retry(max_tries = 3) |>
    # The default provider search makes five requests; space them out.
    httr2::req_throttle(rate = 2, realm = "locator-api.hiv.gov") |>
    httr2::req_error(body = \(resp) .cdc_locator_error_body(resp, auth))
}

.cdc_locator_user_agent <- function() {
  paste0(
    "CDCLocator/",
    utils::packageVersion("CDCLocator"),
    " (https://github.com/VagishHemmige/CDCLocator)"
  )
}

# Extra lines for an HTTP error: the API's own message, with credentials
# removed, and a hint for the usual causes of a 403.
.cdc_locator_error_body <- function(resp, auth) {
  type <- httr2::resp_content_type(resp) %||% ""
  text <- tryCatch(httr2::resp_body_string(resp), error = \(e) "")

  message <- NULL
  if (!grepl("html", type, fixed = TRUE) && nzchar(trimws(text))) {
    message <- .scrub_credentials(text, auth)
    message <- gsub("[[:space:]]+", " ", trimws(message))
    if (nchar(message) > 300) {
      message <- paste0(substr(message, 1, 300), "...")
    }
    message <- c("i" = paste0("The API said: ", message))
  }

  hint <- NULL
  if (httr2::resp_status(resp) == 403) {
    hint <- c(
      "i" = "The API refused this request. The usual causes are:",
      "*" = "the client ID or token is wrong or no longer active, or",
      "*" = paste(
        "the request came from a network the API blocks: outside the US,",
        "or a cloud server or VPN (for example GitHub Actions runners on",
        "Microsoft Azure)."
      )
    )
  }

  c(message, hint)
}

# Removes credential values from text before it is shown. Done on the full
# text, before any truncation, so no partial value can survive.
.scrub_credentials <- function(text, auth) {
  for (value in auth[nzchar(auth)]) {
    text <- gsub(value, "<REDACTED>", text, fixed = TRUE)
  }
  text
}

#' Perform a CDC Locator API request and return parsed JSON
#'
#' Centralizes response checking and JSON parsing.
#'
#' @param req An httr2 request object.
#' @param call The frame named in errors: the user-facing function.
#'
#' @return Parsed JSON content (simplified).
#' @noRd
.perform_cdc_locator_request <- function(req, call = parent.frame()) {
  resp <- httr2::req_perform(req, error_call = call)

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
