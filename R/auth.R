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
#' @noRd
.build_auth_headers <- function(client_id = NULL, client_token = NULL) {
  client_id <- client_id %||% Sys.getenv("CDC_LOCATOR_CLIENT_ID")
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
    "client_id" = client_id,
    "client_token" = client_token
  )
}

#' Check whether CDC Locator credentials are available
#'
#' @return Logical scalar
#' @noRd
.has_cdc_locator_credentials <- function() {
  nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_ID")) &&
    nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN"))
}

#' Set CDC Locator API credentials for this session
#'
#' Sets the `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` environment
#' variables for the current R session, so the other functions can find them.
#' The values are never printed, and no file is written.
#'
#' To keep the credentials between sessions, add both variables to your
#' `.Renviron` file instead, for example with `usethis::edit_r_environ()`, and
#' restart R:
#'
#' ```
#' CDC_LOCATOR_CLIENT_ID=your-client-id
#' CDC_LOCATOR_CLIENT_TOKEN=your-client-token
#' ```
#'
#' Avoid typing the token into a script you might share. Calling this function
#' with values read from a password manager, or with
#' `rstudioapi::askForPassword()`, keeps them out of your code.
#'
#' @param client_id Your Locator API client ID, a single string.
#' @param client_token Your Locator API client token, a single string.
#'
#' @return `NULL`, invisibly. Called for its side effect of setting the two
#'   environment variables.
#' @export
#' @examples
#' \dontrun{
#' cdc_locator_set_credentials(
#'   client_id = "your-client-id",
#'   client_token = "your-client-token"
#' )
#' }
cdc_locator_set_credentials <- function(client_id, client_token) {
  call <- environment()
  for (arg in c("client_id", "client_token")) {
    if (eval(call("missing", as.name(arg)))) {
      cli::cli_abort("{.arg {arg}} is missing.", call = call)
    }
    .check_credential(get(arg), arg, call)
  }

  Sys.setenv(
    CDC_LOCATOR_CLIENT_ID = client_id,
    CDC_LOCATOR_CLIENT_TOKEN = client_token
  )

  cli::cli_inform(c(
    "v" = "Locator API credentials set for this R session.",
    "i" = paste(
      "To keep them, add {.envvar CDC_LOCATOR_CLIENT_ID} and",
      "{.envvar CDC_LOCATOR_CLIENT_TOKEN} to your {.file .Renviron}",
      "(for example with {.code usethis::edit_r_environ()}), then restart R."
    )
  ))

  invisible(NULL)
}

# A credential must be one non-empty, non-missing string. Errors name the
# argument but never show its value.
.check_credential <- function(x, arg, call) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single non-empty string.",
      call = call
    )
  }
  invisible(x)
}
