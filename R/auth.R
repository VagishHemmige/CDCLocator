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

#' Set CDC Locator API credentials
#'
#' Sets the `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` environment
#' variables for the current R session, so the other functions can find them,
#' and, if you ask, saves them in your `.Renviron` file for later sessions.
#' The values are never printed.
#'
#' With `install = TRUE`, the function first asks whether to save the
#' credentials, naming the file, and writes only after you answer yes. It
#' writes to the file named by the `R_ENVIRON_USER` environment variable if
#' that is set, and otherwise to `.Renviron` in your home folder. Any existing
#' `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` lines are replaced,
#' and every other line is kept. A new file is made readable only by you,
#' where the operating system allows. Because it must ask, `install = TRUE`
#' only works in an interactive session.
#'
#' R reads only one user `.Renviron` when it starts. If the folder you start R
#' in (in RStudio, the project's folder) has its own `.Renviron`, R reads that
#' file instead of the one in your home folder, so credentials saved there
#' don't apply in that project. The function warns you when this is the case.
#'
#' Avoid typing the token into a script you might share. Calling this function
#' with values read from a password manager, or with
#' `rstudioapi::askForPassword()`, keeps them out of your code.
#'
#' @param client_id Your Locator API client ID, a single string.
#' @param client_token Your Locator API client token, a single string.
#' @param install `FALSE`, the default, sets the credentials for this session
#'   only and writes no file. `TRUE` also saves them in your `.Renviron` file,
#'   after asking.
#'
#' @return `NULL`, invisibly. Called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' # For this session only
#' cdc_locator_set_credentials(
#'   client_id = "your-client-id",
#'   client_token = "your-client-token"
#' )
#'
#' # Also save them for future sessions, after confirming
#' cdc_locator_set_credentials(
#'   client_id = "your-client-id",
#'   client_token = "your-client-token",
#'   install = TRUE
#' )
#' }
cdc_locator_set_credentials <- function(
  client_id,
  client_token,
  install = FALSE
) {
  call <- environment()
  for (arg in c("client_id", "client_token")) {
    if (eval(call("missing", as.name(arg)))) {
      cli::cli_abort("{.arg {arg}} is missing.", call = call)
    }
    .check_credential(get(arg), arg, call)
  }
  if (!is.logical(install) || length(install) != 1 || is.na(install)) {
    cli::cli_abort(
      "{.arg install} must be {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }

  # Everything that could stop a save is checked before anything changes.
  if (install) {
    for (arg in c("client_id", "client_token")) {
      .check_renviron_value(get(arg), arg, call)
    }
    if (!.is_interactive()) {
      cli::cli_abort(
        c(
          "{.code install = TRUE} needs an interactive session, so you can confirm it.",
          "i" = "Nothing was set or saved."
        ),
        call = call
      )
    }
  }

  Sys.setenv(
    CDC_LOCATOR_CLIENT_ID = client_id,
    CDC_LOCATOR_CLIENT_TOKEN = client_token
  )

  if (!install) {
    cli::cli_inform(c(
      "v" = "Locator API credentials set for this R session.",
      "i" = paste(
        "To keep them for future sessions, run this again with",
        "{.code install = TRUE}, or add {.envvar CDC_LOCATOR_CLIENT_ID} and",
        "{.envvar CDC_LOCATOR_CLIENT_TOKEN} to your {.file .Renviron}",
        "yourself and restart R."
      )
    ))
    return(invisible(NULL))
  }

  path <- .renviron_path()
  answer <- .ask_yes_no(paste0(
    "Save your Locator API credentials to ",
    path,
    "? ",
    "This adds two lines, or replaces the ones already there."
  ))
  if (!isTRUE(answer)) {
    cli::cli_inform(c(
      "v" = "Locator API credentials set for this R session.",
      "i" = "Not saved to {.file {path}}."
    ))
    return(invisible(NULL))
  }

  .write_renviron(path, client_id, client_token)
  cli::cli_inform(c(
    "v" = "Locator API credentials set for this R session and saved to {.file {path}}.",
    "i" = "Saved credentials are read each time R starts."
  ))
  .warn_local_renviron(path)

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

# `.Renviron` treats spaces, quotes, `#` and `$` specially, so only plain
# values are written. Others are refused without showing them.
.check_renviron_value <- function(x, arg, call) {
  if (!grepl("^[A-Za-z0-9._~+/=:-]+$", x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has characters that can't be saved safely in {.file .Renviron}.",
        "i" = "Only letters, digits and {.code . _ ~ + / = : -} can be saved this way.",
        "i" = "Use {.code install = FALSE} and add it to {.file .Renviron} yourself, in quotes.",
        "i" = "Nothing was set or saved."
      ),
      call = call
    )
  }
  invisible(x)
}

# The user .Renviron R reads at startup when no project file is present:
# R_ENVIRON_USER if set, otherwise the one in the home folder.
.renviron_path <- function() {
  path <- Sys.getenv("R_ENVIRON_USER")
  if (!nzchar(path)) {
    path <- file.path(Sys.getenv("HOME", path.expand("~")), ".Renviron")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# Replaces any existing credential lines with the new ones and keeps every
# other line. A new file is made readable only by its owner.
.write_renviron <- function(path, client_id, client_token) {
  is_new <- !file.exists(path)
  lines <- if (is_new) character() else readLines(path, warn = FALSE)
  pattern <- "^\\s*(CDC_LOCATOR_CLIENT_ID|CDC_LOCATOR_CLIENT_TOKEN)\\s*="
  lines <- lines[!grepl(pattern, lines)]
  lines <- c(
    lines,
    paste0("CDC_LOCATOR_CLIENT_ID=", client_id),
    paste0("CDC_LOCATOR_CLIENT_TOKEN=", client_token)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  if (is_new) {
    Sys.chmod(path, mode = "0600")
  }
  invisible(path)
}

# R reads a .Renviron in the starting folder instead of the user file. Warn
# when the current folder has one that isn't the file just written.
.warn_local_renviron <- function(written) {
  local <- normalizePath(".Renviron", winslash = "/", mustWork = FALSE)
  if (
    file.exists(local) &&
      !identical(local, written) &&
      !nzchar(Sys.getenv("R_ENVIRON_USER"))
  ) {
    cli::cli_warn(c(
      "!" = "But this folder has its own {.file .Renviron}, which R reads instead.",
      "i" = "Add the credentials to {.file {local}} too, if you start R here."
    ))
  }
  invisible()
}

# Wrappers so tests can stand in for a person at the console.
.is_interactive <- function() {
  interactive()
}

.ask_yes_no <- function(question) {
  utils::askYesNo(question, default = FALSE)
}
