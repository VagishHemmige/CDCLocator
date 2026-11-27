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
#' (On Windows, this means the file is created with an access control list that
#' gives only your account access, which is checked before anything is written.)
#' Because it must ask, `install = TRUE` only works in an interactive session.
#'
#' R reads only one user `.Renviron` when it starts. If the folder you start R
#' in (in RStudio, the project's folder) has its own `.Renviron`, R reads that
#' file instead of the one in your home folder, so credentials saved there
#' don't apply in that project. The function warns you when this is the case.
#'
#' With `install = TRUE`, if the `.Renviron` file is a symbolic link, nothing is
#' set or saved, because replacing the file would replace the link. Add the
#' lines to the file it points to yourself.
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
    .check_renviron_target(.renviron_path(), call)
  }

  Sys.setenv(
    CDC_LOCATOR_CLIENT_ID = client_id,
    CDC_LOCATOR_CLIENT_TOKEN = client_token
  )

  if (!install) {
    # Suggest saving only when the values could be saved.
    savable <- .is_renviron_safe(client_id) && .is_renviron_safe(client_token)
    keep <- if (savable) {
      paste(
        "To keep them for future sessions, run this again with",
        "{.code install = TRUE}, or add {.envvar CDC_LOCATOR_CLIENT_ID} and",
        "{.envvar CDC_LOCATOR_CLIENT_TOKEN} to your {.file .Renviron}",
        "yourself and restart R."
      )
    } else {
      "These values can't be saved in {.file .Renviron}, so set them again in each new session."
    }
    cli::cli_inform(c(
      "v" = "Locator API credentials set for this R session.",
      "i" = keep
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

# `.Renviron` treats spaces, quotes, `#` and `$` specially, and a line break
# would split the line, so only plain values are written. Others are refused
# without showing them, and the advice is session-only use: quoting by hand
# can't hold a line break, so it isn't suggested.
.check_renviron_value <- function(x, arg, call) {
  if (!.is_renviron_safe(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has characters that can't be saved safely in {.file .Renviron}.",
        "i" = "Only letters, digits and {.code . _ ~ + / = : -} can be saved this way.",
        "i" = "Use {.code install = FALSE} to set it for this session only.",
        "i" = "Nothing was set or saved."
      ),
      call = call
    )
  }
  invisible(x)
}

.is_renviron_safe <- function(x) {
  grepl("^[A-Za-z0-9._~+/=:-]+$", x)
}

# Saving replaces the file by renaming a new one over it, which would turn a
# symbolic link (say, to a dotfiles repository) into a regular file and leave
# the file it pointed to unchanged. So a link is refused, without asking.
.check_renviron_target <- function(path, call) {
  is_link <- .is_link(path)
  if (is.na(is_link)) {
    cli::cli_abort(
      c(
        "Couldn't check whether {.file {path}} is a link, so it wasn't updated.",
        "i" = "Use {.code install = FALSE}, or add the credentials to it yourself.",
        "i" = "Nothing was set or saved."
      ),
      call = call
    )
  }
  if (is_link) {
    cli::cli_abort(
      c(
        "{.file {path}} is a symbolic link, so it can't be updated safely.",
        "i" = "Use {.code install = FALSE}, or add the credentials to the file it points to yourself.",
        "i" = "Nothing was set or saved."
      ),
      call = call
    )
  }
  invisible(path)
}

# TRUE if `path` is a link (including a broken one), FALSE if it is a regular
# file or missing, NA if that can't be told.
.is_link <- function(path) {
  if (.Platform$OS.type == "windows") {
    return(.is_windows_link(path))
  }
  # Sys.readlink() gives "" for a regular file and NA for a missing one.
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}

# Sys.readlink() always gives "" on Windows, so ask Windows for the path's own
# attributes (not those of anything it points to): a reparse point is a
# symbolic link or junction. Only "not found" counts as missing; any other
# failure, such as access denied, gives NA.
.is_windows_link <- function(path) {
  script <- paste(
    "$ErrorActionPreference = 'Stop'",
    paste0(
      "try { $attributes = [IO.File]::GetAttributes($env:CDCLOCATOR_FILE) } ",
      "catch [IO.FileNotFoundException], [IO.DirectoryNotFoundException] ",
      "{ 'missing'; exit 0 }"
    ),
    paste0(
      "if ($attributes -band [IO.FileAttributes]::ReparsePoint) ",
      "{ 'link' } else { 'file' }"
    ),
    sep = "; "
  )
  .link_answer(.run_powershell(script, path))
}

# Turns the PowerShell answer into TRUE (a link), FALSE (a file or nothing),
# or NA (anything else, including a failed run).
.link_answer <- function(result) {
  if (result$status != 0 || length(result$output) != 1) {
    return(NA)
  }
  switch(result$output, link = TRUE, file = FALSE, missing = FALSE, NA)
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

  not_saved <- c(
    "i" = "The credentials are set for this session, but nothing was saved.",
    "i" = "Add them to {.file {path}} yourself, and make it readable only by you."
  )
  not_private <- c(
    "Couldn't make a file in {.file {folder}} readable only by you.",
    not_saved
  )
  # The temporary file must be new, so no other process can have made it
  # first. On Windows, creating it fails if the name is taken. On Unix, it is
  # made inside a staging folder that only this call can have created, since
  # making a folder fails if the name is taken. Only what this call created is
  # removed afterwards: the file, then the staging folder once it is empty,
  # never anything else inside it.
  if (.Platform$OS.type == "windows") {
    temp <- .new_temp_path(folder)
  } else {
    staging <- .new_temp_path(folder)
    if (!.create_private_folder(staging)) {
      cli::cli_abort(not_private, call = call)
    }
    on.exit(.remove_empty_folder(staging), add = TRUE)
    temp <- file.path(staging, ".Renviron")
  }
  if (!.create_private_file(temp)) {
    cli::cli_abort(not_private, call = call)
  }
  # Runs before the folder's removal, so the folder is empty by then.
  on.exit(unlink(temp), add = TRUE, after = FALSE)
  if (!.protect_file(temp)) {
    cli::cli_abort(not_private, call = call)
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

# Unix only: creates a new folder that only its owner can use, asking for
# mode 0700 under a umask of 077. Returns FALSE if the name is already taken
# (by a file or a folder). A folder's default access list can override both,
# so if the new folder doesn't come out 0700 it is removed (it is still empty)
# and FALSE is returned.
.create_private_folder <- function(path) {
  old_mask <- Sys.umask("077")
  on.exit(Sys.umask(old_mask), add = TRUE)
  created <- tryCatch(
    dir.create(path, showWarnings = FALSE, mode = "0700"),
    error = function(e) FALSE
  )
  if (!created) {
    return(FALSE)
  }
  if (!identical(format(file.mode(path)), "700")) {
    .remove_empty_folder(path)
    return(FALSE)
  }
  TRUE
}

# Removes a folder only if it is empty; anything inside is left alone, and so
# is the folder. (unlink() never removes a folder unless asked to remove its
# contents too.)
.remove_empty_folder <- function(path) {
  empty <- length(list.files(path, all.files = TRUE, no.. = TRUE)) == 0
  if (dir.exists(path) && empty) {
    suppressWarnings(file.remove(path))
  }
  invisible()
}

# Creates an empty file that only its owner can read. Returns FALSE instead of
# stopping when it fails, including when a file of that name already exists.
.create_private_file <- function(path) {
  if (.Platform$OS.type == "windows") {
    return(.create_private_file_windows(path))
  }
  # Under a umask of 077, restored however the create call ends, even when a
  # warning is turned into an error.
  old_mask <- Sys.umask("077")
  on.exit(Sys.umask(old_mask), add = TRUE)
  tryCatch(
    !file.exists(path) && file.create(path, showWarnings = FALSE),
    error = function(e) FALSE
  )
}

# Wrappers so tests can choose the temporary name, or make writing or
# replacing fail.
.new_temp_path <- function(folder) {
  tempfile(".Renviron-", tmpdir = folder)
}

.write_lines <- function(lines, path) {
  writeLines(lines, path)
}

.replace_file <- function(from, to) {
  file.rename(from, to)
}

# Makes a file readable and writable by its owner only, and checks that it
# worked. On Windows the file was created that way, so this only checks.
.protect_file <- function(path) {
  if (.Platform$OS.type == "windows") {
    return(.protect_file_windows(path))
  }
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  identical(format(file.mode(path)), "600")
}

# On Windows, file modes don't decide who can read a file; its access control
# list does, and a new file normally inherits its folder's list, which may let
# others read it (for example when R_ENVIRON_USER names a shared folder).
# Restricting the file after creating it would leave a moment when someone
# could open it and keep that access. So the file is created with its final
# list already in place: no inherited entries, and full control for the
# current user only. It must not already exist. The path is passed to
# PowerShell as an environment variable, never inside the command, so no
# character in it can change the command.
.create_private_file_windows <- function(path) {
  script <- paste(
    "$ErrorActionPreference = 'Stop'",
    "$user = [Security.Principal.WindowsIdentity]::GetCurrent().User",
    "$security = New-Object Security.AccessControl.FileSecurity",
    "$security.SetAccessRuleProtection($true, $false)",
    paste0(
      "$security.AddAccessRule((New-Object ",
      "Security.AccessControl.FileSystemAccessRule($user, 'FullControl', 'Allow')))"
    ),
    paste0(
      "$stream = New-Object IO.FileStream($env:CDCLOCATOR_FILE, ",
      "[IO.FileMode]::CreateNew, ",
      "[Security.AccessControl.FileSystemRights]::FullControl, ",
      "[IO.FileShare]::None, 4096, [IO.FileOptions]::None, $security)"
    ),
    "$stream.Close()",
    "'created'",
    sep = "; "
  )
  result <- .run_powershell(script, path)
  result$status == 0 && identical(result$output, "created")
}

# Reads a file's access control list back and checks that it is protected (no
# inherited entries) and gives full control to the current user only. The
# rules are listed with their SIDs, because the text form of the list (SDDL)
# names some accounts by short codes instead, such as LA for the built-in
# Administrator.
.protect_file_windows <- function(path) {
  script <- paste(
    "$ErrorActionPreference = 'Stop'",
    "$acl = Get-Acl -LiteralPath $env:CDCLOCATOR_FILE",
    "[Security.Principal.WindowsIdentity]::GetCurrent().User.Value",
    "'protected=' + $acl.AreAccessRulesProtected",
    paste0(
      "foreach ($rule in $acl.GetAccessRules($true, $true, ",
      "[Security.Principal.SecurityIdentifier])) { 'rule=' + ",
      "$rule.IdentityReference.Value + ';' + $rule.AccessControlType + ';' + ",
      "$rule.FileSystemRights + ';' + $rule.IsInherited }"
    ),
    sep = "; "
  )
  result <- .run_powershell(script, path)
  result$status == 0 && .acl_is_owner_only(result$output)
}

# Whether the lines .protect_file_windows() prints describe a protected list
# with one rule: allow full control, not inherited, for the current user,
# whose SID is the first line.
.acl_is_owner_only <- function(lines) {
  if (length(lines) != 3) {
    return(FALSE)
  }
  sid <- lines[1]
  grepl("^S-1-[0-9]+(-[0-9]+)+$", sid) &&
    identical(lines[2], "protected=True") &&
    identical(lines[3], paste0("rule=", sid, ";Allow;FullControl;False"))
}

# A full path in Windows form for PowerShell. Only the folder is normalized:
# normalizing the whole path would follow a link to its target, and the link
# check needs the link itself.
.native_path <- function(path) {
  folder <- normalizePath(dirname(path), winslash = "\\", mustWork = FALSE)
  paste0(folder, "\\", basename(path))
}

# Runs a fixed PowerShell script with `path` in the CDCLOCATOR_FILE
# environment variable, and returns its exit status and the non-empty lines it
# printed. PowerShell that can't be started counts as a failure.
.run_powershell <- function(script, path) {
  old <- Sys.getenv("CDCLOCATOR_FILE", unset = NA)
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("CDCLOCATOR_FILE")
    } else {
      Sys.setenv(CDCLOCATOR_FILE = old)
    },
    add = TRUE
  )
  Sys.setenv(CDCLOCATOR_FILE = .native_path(path))
  powershell <- file.path(
    Sys.getenv("SystemRoot", "C:/Windows"),
    "System32",
    "WindowsPowerShell",
    "v1.0",
    "powershell.exe"
  )
  output <- tryCatch(
    suppressWarnings(system2(
      powershell,
      c(
        "-NoProfile",
        "-NonInteractive",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        shQuote(script, type = "cmd")
      ),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) structure(character(), status = 127L)
  )
  list(
    status = attr(output, "status") %||% 0L,
    output = trimws(output[nzchar(trimws(output))])
  )
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
