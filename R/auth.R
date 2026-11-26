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
#' that is set, and otherwise to `.Renviron` in the home folder R uses at
#' startup (on Windows, `R_USER` if set, then `HOME`). Any existing
#' `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` lines are replaced,
#' and every other line is kept. The file is made readable only by you before
#' the credentials are written to it; if that isn't possible, nothing is saved.
#' (On Windows, this means an access control list that gives only your account
#' access, set with the system's `icacls` tool and checked afterwards.)
#' Because it must ask, `install = TRUE` only works in an interactive session.
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

  .write_renviron(path, client_id, client_token, call)
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
# R_ENVIRON_USER if set, otherwise .Renviron in R's home folder, found the way
# R finds it (on Windows, R_USER before HOME).
.renviron_path <- function() {
  path <- Sys.getenv("R_ENVIRON_USER")
  if (!nzchar(path)) {
    path <- path.expand("~/.Renviron")
  }
  # Resolve the folder, which exists, rather than the file, which may not.
  folder <- dirname(path)
  if (dir.exists(folder)) {
    folder <- normalizePath(folder, winslash = "/")
  }
  file.path(folder, basename(path))
}

# Saves the credentials without ever leaving them in a file others can read.
# The new contents go into a temporary file in the same folder, which is made
# owner-only and checked before anything secret is written, and then replaces
# the old file. Any existing credential lines are replaced; every other line
# is kept. If any step fails, the old file is left as it was.
.write_renviron <- function(path, client_id, client_token, call) {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  pattern <- "^\\s*(CDC_LOCATOR_CLIENT_ID|CDC_LOCATOR_CLIENT_TOKEN)\\s*="
  lines <- c(
    lines[!grepl(pattern, lines)],
    paste0("CDC_LOCATOR_CLIENT_ID=", client_id),
    paste0("CDC_LOCATOR_CLIENT_TOKEN=", client_token)
  )

  folder <- dirname(path)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(".Renviron-", tmpdir = folder)
  on.exit(unlink(temp), add = TRUE)

  not_saved <- c(
    "i" = "The credentials are set for this session, but nothing was saved.",
    "i" = "Add them to {.file {path}} yourself, and make it readable only by you."
  )
  if (!.create_private_file(temp) || !.protect_file(temp)) {
    cli::cli_abort(
      c(
        "Couldn't make a file in {.file {folder}} readable only by you.",
        not_saved
      ),
      call = call
    )
  }

  # A warning counts as a failure too: a full disk can show up only as a
  # warning when the file is closed, leaving it incomplete.
  written <- tryCatch(
    {
      .write_lines(lines, temp)
      TRUE
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  if (!written) {
    cli::cli_abort(
      c("Couldn't write a new copy of {.file {path}}.", not_saved),
      call = call
    )
  }
  replaced <- tryCatch(.replace_file(temp, path), error = function(e) FALSE)
  if (!replaced) {
    cli::cli_abort(
      c("Couldn't replace {.file {path}}.", not_saved),
      call = call
    )
  }
  invisible(path)
}

# Creates an empty file that only its owner can read, under a umask of 077.
# The umask is restored however the create call ends, even when a warning is
# turned into an error. Returns FALSE instead of stopping when it fails.
.create_private_file <- function(path) {
  old_mask <- Sys.umask("077")
  on.exit(Sys.umask(old_mask), add = TRUE)
  tryCatch(
    file.create(path, showWarnings = FALSE),
    error = function(e) FALSE
  )
}

# Wrappers so tests can make writing or replacing fail.
.write_lines <- function(lines, path) {
  writeLines(lines, path)
}

.replace_file <- function(from, to) {
  file.rename(from, to)
}

# Makes a file readable and writable by its owner only, and checks that it
# worked.
.protect_file <- function(path) {
  if (.Platform$OS.type == "windows") {
    return(.protect_file_windows(path))
  }
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  identical(format(file.mode(path)), "600")
}

# On Windows, file modes don't decide who can read a file; its access control
# list does, and a new file inherits its folder's list, which may let others
# read it (for example when R_ENVIRON_USER names a shared folder). So the
# inherited entries are removed and only the current user, named by security
# identifier (SID), is given access. The list is then read back and checked.
# Any failure returns FALSE, so nothing is saved.
.protect_file_windows <- function(path) {
  system32 <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32")

  whoami <- .run_command(
    file.path(system32, "whoami.exe"),
    c("/user", "/fo", "csv", "/nh")
  )
  sid <- if (whoami$status == 0) .parse_whoami_sid(whoami$output) else NA
  if (is.na(sid)) {
    return(FALSE)
  }

  restrict <- .run_command(
    file.path(system32, "icacls.exe"),
    c(
      shQuote(path, type = "cmd"),
      "/inheritance:r",
      "/grant:r",
      paste0("*", sid, ":F")
    )
  )
  if (restrict$status != 0) {
    return(FALSE)
  }

  read_back <- .run_command(
    file.path(system32, "WindowsPowerShell", "v1.0", "powershell.exe"),
    c(
      "-NoProfile",
      "-NonInteractive",
      "-Command",
      shQuote(
        paste0(
          "(Get-Acl -LiteralPath '",
          gsub("'", "''", path, fixed = TRUE),
          "').Sddl"
        ),
        type = "cmd"
      )
    )
  )
  read_back$status == 0 && .dacl_is_owner_only(read_back$output, sid)
}

# The SID in `whoami /user /fo csv /nh` output, a line like
# "machine\\name","S-1-5-21-...", or NA if there isn't exactly one.
.parse_whoami_sid <- function(output) {
  output <- output[nzchar(trimws(output))]
  if (length(output) != 1) {
    return(NA_character_)
  }
  fields <- scan(text = output, what = "", sep = ",", quiet = TRUE)
  sid <- fields[length(fields)]
  if (length(sid) == 1 && grepl("^S-1-[0-9]+(-[0-9]+)+$", sid)) {
    sid
  } else {
    NA_character_
  }
}

# Whether a security descriptor, in Security Descriptor Definition Language,
# has a protected list (no inherited entries) whose only entry gives `sid`
# full control: "D:P(A;;FA;;;<sid>)", possibly with other list flags. Anything
# else, including a well-known short name in place of the SID, is FALSE.
.dacl_is_owner_only <- function(sddl, sid) {
  sddl <- paste(trimws(sddl), collapse = "")
  dacl <- regmatches(sddl, regexec("D:([A-Z]*)((\\([^)]*\\))*)", sddl))[[1]]
  length(dacl) >= 3 &&
    grepl("P", dacl[2], fixed = TRUE) &&
    identical(dacl[3], paste0("(A;;FA;;;", sid, ")"))
}

# Runs a program and returns its exit status and standard output. A program
# that can't be started counts as a failure.
.run_command <- function(command, args) {
  output <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = FALSE)),
    error = function(e) structure(character(), status = 127L)
  )
  list(status = attr(output, "status") %||% 0L, output = output)
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
