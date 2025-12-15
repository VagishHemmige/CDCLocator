# -------------------------------------------------------------------------
# Authentication helpers for CDC Locator API
# -------------------------------------------------------------------------

#' Build authentication headers for the CDC Locator API
#'
#' Credentials are discovered in the following order:
#' 1. Explicit arguments
#' 2. Environment variables:
#'    - CDC_LOCATOR_CLIENT_ID
#'    - CDC_LOCATOR_CLIENT_TOKEN
#'
#' @param client_id CDC client ID (optional)
#' @param client_token CDC client token (optional)
#'
#' @return Named character vector suitable for httr2::req_headers()
#' @keywords internal
.build_auth_headers <- function(client_id = NULL, client_token = NULL) {

  client_id    <- client_id    %||% Sys.getenv("CDC_LOCATOR_CLIENT_ID")
  client_token <- client_token %||% Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN")

  if (!nzchar(client_id) || !nzchar(client_token)) {
    cli::cli_abort(c(
      "CDC Locator API credentials not found.",
      "i" = "Set credentials using {.code cdc_locator_set_credentials()}",
      "i" = "or define environment variables:",
      " " = "{.envvar CDC_LOCATOR_CLIENT_ID}",
      " " = "{.envvar CDC_LOCATOR_CLIENT_TOKEN}"
    ))
  }

  c(
    "client_id"    = client_id,
    "client_token" = client_token
  )
}

#' Check whether CDC Locator credentials are available
#'
#' @return Logical scalar
#' @keywords internal
.has_cdc_locator_credentials <- function() {
  nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_ID")) &&
    nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN"))
}

#' Set CDC Locator API credentials
#'
#' Stores credentials in the user's .Renviron file. You only need to run
#' this once per machine. Restart R after setting credentials.
#'
#' @param client_id CDC client ID
#' @param client_token CDC client token
#'
#' @export
cdc_locator_set_credentials <- function(client_id, client_token) {

  if (!nzchar(client_id) || !nzchar(client_token)) {
    cli::cli_abort("Both {.arg client_id} and {.arg client_token} must be provided.")
  }

  usethis::edit_r_environ()

  cli::cli_alert_success(c(
    "Add the following lines to your {.file .Renviron}:",
    "",
    "CDC_LOCATOR_CLIENT_ID={client_id}",
    "CDC_LOCATOR_CLIENT_TOKEN={client_token}",
    "",
    "Then restart R."
  ))
}
