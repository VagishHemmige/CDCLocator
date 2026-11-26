test_that(".build_auth_headers() errors helpfully without credentials", {
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  expect_snapshot(.build_auth_headers(), error = TRUE)
})

test_that(".build_auth_headers() reads environment variables", {
  local_fake_credentials()
  expect_equal(
    .build_auth_headers(),
    c(client_id = "fake-client-id", client_token = "fake-client-token")
  )
})

test_that(".build_auth_headers() prefers arguments over environment variables", {
  local_fake_credentials()
  expect_equal(
    .build_auth_headers(client_id = "arg-id", client_token = "arg-token"),
    c(client_id = "arg-id", client_token = "arg-token")
  )
})

test_that(".has_cdc_locator_credentials() needs both variables", {
  withr::local_envvar(
    CDC_LOCATOR_CLIENT_ID = "fake-client-id",
    CDC_LOCATOR_CLIENT_TOKEN = NA
  )
  expect_false(.has_cdc_locator_credentials())

  local_fake_credentials()
  expect_true(.has_cdc_locator_credentials())
})

# Runs `code` with a temporary home folder and user .Renviron, so a test can
# check that nothing is written to the real ones.
local_temp_home <- function(env = parent.frame()) {
  # Normalize the folder first and build every path from it, so the paths
  # match the package's own (on macOS /var is /private/var; on Windows short
  # and long names differ).
  home <- normalizePath(
    withr::local_tempdir(.local_envir = env),
    winslash = "/"
  )
  withr::local_envvar(
    HOME = home,
    R_ENVIRON_USER = file.path(home, ".Renviron"),
    .local_envir = env
  )
  home
}

test_that("cdc_locator_set_credentials() sets them for this session only", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)

  suppressMessages(cdc_locator_set_credentials(
    "fake-client-id",
    "fake-client-token"
  ))

  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "fake-client-id")
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN"), "fake-client-token")
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), character())
})

test_that("cdc_locator_set_credentials() never shows the values", {
  local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)

  shown <- c(
    utils::capture.output(
      messages <- capture_messages(
        cdc_locator_set_credentials("fake-client-id", "fake-client-token")
      )
    ),
    messages
  )
  expect_no_match(shown, "fake-client")
  expect_match(shown, "CDC_LOCATOR_CLIENT_TOKEN", all = FALSE)
})

test_that("cdc_locator_set_credentials() explains how to keep them", {
  local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  expect_snapshot(cdc_locator_set_credentials(
    "fake-client-id",
    "fake-client-token"
  ))
})

test_that("cdc_locator_set_credentials() rejects missing or malformed values", {
  local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)

  expect_snapshot(
    cdc_locator_set_credentials("", "fake-client-token"),
    error = TRUE
  )
  expect_snapshot(
    cdc_locator_set_credentials("fake-client-id", NA),
    error = TRUE
  )
  expect_snapshot(
    cdc_locator_set_credentials(c("a", "b"), "fake-client-token"),
    error = TRUE
  )
  expect_snapshot(cdc_locator_set_credentials("fake-client-id"), error = TRUE)
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "")
})

# Pretends the session is interactive and answers the confirmation with
# `answer`. Returns an environment whose `question` holds what was asked.
local_confirm <- function(answer, env = parent.frame()) {
  seen <- new.env()
  local_mocked_bindings(
    .is_interactive = function() TRUE,
    .ask_yes_no = function(question) {
      seen$question <- question
      answer
    },
    .env = env
  )
  seen
}

test_that("install = TRUE needs an interactive session and changes nothing without one", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  local_mocked_bindings(.is_interactive = function() FALSE)

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE
  )
  expect_false(file.exists(file.path(home, ".Renviron")))
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "")
})

test_that("install = TRUE asks first and writes nothing if the answer is no", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  asked <- local_confirm(FALSE)

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_match(asked$question, file.path(home, ".Renviron"), fixed = TRUE)
  expect_no_match(asked$question, "fake-client")
  expect_false(file.exists(file.path(home, ".Renviron")))
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "fake-client-id")
})

test_that("install = TRUE writes both lines after a yes, without showing them", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  local_confirm(TRUE)

  messages <- capture_messages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  )
  path <- file.path(home, ".Renviron")

  expect_equal(
    readLines(path),
    c(
      "CDC_LOCATOR_CLIENT_ID=fake-client-id",
      "CDC_LOCATOR_CLIENT_TOKEN=fake-client-token"
    )
  )
  expect_no_match(messages, "fake-client")
  expect_match(messages, "Saved", all = FALSE)
  # Windows is checked by its own test, through the access control list.
  skip_on_os("windows")
  expect_equal(format(file.mode(path)), "600")
})

test_that("install = TRUE replaces old credential lines and keeps everything else", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  # No newline at the end, as some editors leave it.
  writeChar(
    paste(
      "OTHER_KEY=keep-me",
      "CDC_LOCATOR_CLIENT_ID=old-id",
      " CDC_LOCATOR_CLIENT_TOKEN = old-token",
      "# a comment",
      "LAST=also-kept",
      sep = "\n"
    ),
    path,
    eos = NULL
  )
  local_confirm(TRUE)

  suppressMessages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  )

  expect_equal(
    readLines(path),
    c(
      "OTHER_KEY=keep-me",
      "# a comment",
      "LAST=also-kept",
      "CDC_LOCATOR_CLIENT_ID=fake-client-id",
      "CDC_LOCATOR_CLIENT_TOKEN=fake-client-token"
    )
  )
})

test_that("install = TRUE uses R_ENVIRON_USER when it names a file", {
  home <- local_temp_home()
  other <- file.path(home, "elsewhere.Renviron")
  withr::local_envvar(
    R_ENVIRON_USER = other,
    CDC_LOCATOR_CLIENT_ID = NA,
    CDC_LOCATOR_CLIENT_TOKEN = NA
  )
  local_confirm(TRUE)

  suppressMessages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  )

  expect_true(file.exists(other))
  expect_false(file.exists(file.path(home, ".Renviron")))
})

test_that("install = TRUE warns when the current folder has its own .Renviron", {
  home <- local_temp_home()
  # With R_ENVIRON_USER unset, R reads a .Renviron in the starting folder
  # instead of the home one.
  withr::local_envvar(
    R_ENVIRON_USER = NA,
    CDC_LOCATOR_CLIENT_ID = NA,
    CDC_LOCATOR_CLIENT_TOKEN = NA
  )
  project <- withr::local_tempdir()
  writeLines("SOMETHING=else", file.path(project, ".Renviron"))
  withr::local_dir(project)
  local_confirm(TRUE)
  # R caches the home folder, so point the default file at the test's home
  # rather than letting a test reach the real one.
  local_mocked_bindings(.renviron_path = function() {
    file.path(home, ".Renviron")
  })

  expect_warning(
    suppressMessages(
      cdc_locator_set_credentials(
        "fake-client-id",
        "fake-client-token",
        install = TRUE
      )
    ),
    "this folder has its own"
  )
  expect_equal(readLines(file.path(project, ".Renviron")), "SOMETHING=else")
  expect_length(readLines(file.path(home, ".Renviron")), 2)
})

test_that("install = TRUE doesn't warn when R_ENVIRON_USER chooses the file", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  project <- withr::local_tempdir()
  writeLines("SOMETHING=else", file.path(project, ".Renviron"))
  withr::local_dir(project)
  local_confirm(TRUE)

  expect_no_warning(suppressMessages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  ))
})

test_that("install = TRUE refuses values .Renviron can't hold, before writing", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  local_confirm(TRUE)

  expect_snapshot(
    cdc_locator_set_credentials("fake id", "fake-client-token", install = TRUE),
    error = TRUE
  )
  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake\ntoken",
      install = TRUE
    ),
    error = TRUE
  )
  expect_false(file.exists(file.path(home, ".Renviron")))
})

test_that("install must be TRUE or FALSE", {
  local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = "yes"
    ),
    error = TRUE
  )
})

test_that("install = TRUE restores the umask when the file can't be created", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  # A file where the folder should be, so creating the temporary file fails,
  # and warnings as errors, so the failure interrupts the create call.
  writeLines("not a folder", file.path(home, "blocker"))
  withr::local_envvar(R_ENVIRON_USER = file.path(home, "blocker", ".Renviron"))
  old_umask <- Sys.umask("022")
  withr::defer(Sys.umask(old_umask))
  local_confirm(TRUE)

  expect_snapshot(
    withr::with_options(
      list(warn = 2),
      cdc_locator_set_credentials(
        "fake-client-id",
        "fake-client-token",
        install = TRUE
      )
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), "blocker")
  # Windows keeps only part of the umask, so its value isn't compared there.
  skip_on_os("windows")
  expect_equal(format(Sys.umask()), "22")
})

test_that("install = TRUE keeps the old file if writing the new one fails", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  writeLines("OTHER_KEY=keep-me", path)
  local_confirm(TRUE)
  local_mocked_bindings(.write_lines = function(lines, path) {
    stop("disk full")
  })

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_equal(readLines(path), "OTHER_KEY=keep-me")
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("install = TRUE keeps the old file if closing the new one warns", {
  # A full disk can surface only when the file is closed, as a warning.
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  writeLines(c("OTHER_KEY=keep-me", "LAST=also-kept"), path)
  local_confirm(TRUE)
  local_mocked_bindings(.write_lines = function(lines, path) {
    writeLines(lines[1], path)
    warning("problem closing connection")
  })

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_equal(readLines(path), c("OTHER_KEY=keep-me", "LAST=also-kept"))
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("install = TRUE keeps the old file if replacing it fails", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  writeLines("OTHER_KEY=keep-me", path)
  local_confirm(TRUE)
  local_mocked_bindings(.replace_file = function(from, to) FALSE)

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_equal(readLines(path), "OTHER_KEY=keep-me")
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("install = TRUE makes an existing readable .Renviron owner-only", {
  skip_on_os("windows")
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  writeLines("OTHER_KEY=keep-me", path)
  Sys.chmod(path, mode = "0644")
  local_confirm(TRUE)

  suppressMessages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  )

  expect_equal(format(file.mode(path)), "600")
  expect_equal(readLines(path)[1], "OTHER_KEY=keep-me")
})

# On Windows: a temporary folder everyone can read, as a shared folder named by
# R_ENVIRON_USER might be. `name` is the folder's name inside the temporary
# folder.
local_everyone_reads_folder <- function(name = "shared", env = parent.frame()) {
  folder <- file.path(withr::local_tempdir(.local_envir = env), name)
  dir.create(folder)
  icacls <- file.path(Sys.getenv("SystemRoot"), "System32", "icacls.exe")
  status <- system2(
    icacls,
    c(shQuote(folder, type = "cmd"), "/grant", "*S-1-1-0:(OI)(CI)R"),
    stdout = FALSE
  )
  if (!identical(as.integer(status), 0L)) {
    skip("Couldn't let everyone read the test folder.")
  }
  normalizePath(folder, winslash = "/")
}

# On Windows: the current user's SID, whether a file's list is protected, and
# its rules as "SID;type;rights;inherited", read with PowerShell independently
# of the package's helpers.
read_acl <- function(path) {
  withr::local_envvar(CHECK_FILE = normalizePath(path, winslash = "\\"))
  script <- paste(
    "$acl = Get-Acl -LiteralPath $env:CHECK_FILE",
    "[Security.Principal.WindowsIdentity]::GetCurrent().User.Value",
    "$acl.AreAccessRulesProtected",
    paste0(
      "$acl.GetAccessRules($true, $true, ",
      "[Security.Principal.SecurityIdentifier]) | ForEach-Object { ",
      "$_.IdentityReference.Value + ';' + $_.AccessControlType + ';' + ",
      "$_.FileSystemRights + ';' + $_.IsInherited }"
    ),
    sep = "; "
  )
  out <- system2(
    "powershell",
    c("-NoProfile", "-Command", shQuote(script, type = "cmd")),
    stdout = TRUE
  )
  out <- trimws(out[nzchar(trimws(out))])
  list(sid = out[1], protected = out[2], rules = out[-(1:2)])
}

# Checks that read_acl() found one rule: full control for the current user.
expect_owner_only <- function(acl) {
  expect_match(acl$sid, "^S-1-[0-9]+(-[0-9]+)+$")
  expect_equal(acl$protected, "True")
  expect_equal(acl$rules, paste0(acl$sid, ";Allow;FullControl;False"))
}

test_that(".create_private_file() on Windows makes a file only its owner can open", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_on_cran()
  folder <- local_everyone_reads_folder()
  path <- file.path(folder, ".Renviron-test")

  expect_true(.create_private_file(path))
  expect_true(file.exists(path))
  expect_owner_only(read_acl(path))
  expect_true(.protect_file(path))
  # It never reuses a file that already exists.
  expect_false(.create_private_file(path))
})

test_that(".create_private_file() on Windows copes with quotes in the path", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_on_cran()
  folder <- local_everyone_reads_folder("O\u2019Brien's files")
  path <- file.path(folder, ".Renviron-test")

  expect_true(.create_private_file(path))
  expect_true(.protect_file(path))
})

test_that("install = TRUE on Windows gives only the current user access", {
  skip_on_os(c("mac", "linux", "solaris"))
  skip_on_cran()
  folder <- local_everyone_reads_folder()
  path <- file.path(folder, ".Renviron")
  withr::local_envvar(
    R_ENVIRON_USER = path,
    CDC_LOCATOR_CLIENT_ID = NA,
    CDC_LOCATOR_CLIENT_TOKEN = NA
  )
  writeLines("OTHER_KEY=keep-me", path)
  local_confirm(TRUE)

  suppressMessages(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    )
  )

  expect_owner_only(read_acl(path))
  expect_equal(readLines(path)[1], "OTHER_KEY=keep-me")
  expect_equal(list.files(folder, all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("the Windows helpers pass the path to PowerShell only as data", {
  scripts <- character()
  local_mocked_bindings(.run_powershell = function(script, path) {
    scripts <<- c(scripts, script)
    list(status = 0L, output = "created")
  })

  .create_private_file_windows("C:/Users/O\u2019Brien/.Renviron-1")
  .protect_file_windows("C:/Users/O\u2019Brien/.Renviron-1")

  expect_length(scripts, 2)
  expect_no_match(scripts, "Brien", fixed = TRUE)
  expect_match(scripts, "$env:CDCLOCATOR_FILE", fixed = TRUE)
  expect_match(scripts[1], "CreateNew", fixed = TRUE)
})

test_that(".protect_file_windows() needs the one owner-only rule", {
  sid <- "S-1-5-21-1-2-3-500"
  owner_only <- c(
    sid,
    "protected=True",
    paste0("rule=", sid, ";Allow;FullControl;False")
  )
  answer <- list(status = 0L, output = owner_only)
  local_mocked_bindings(.run_powershell = function(script, path) answer)
  expect_true(.protect_file_windows("C:/x"))

  answer <- list(status = 1L, output = owner_only)
  expect_false(.protect_file_windows("C:/x"))
})

test_that(".create_private_file_windows() fails when PowerShell does", {
  answer <- list(status = 1L, output = character())
  local_mocked_bindings(.run_powershell = function(script, path) answer)
  expect_false(.create_private_file_windows("C:/x"))

  answer <- list(status = 0L, output = "something else")
  expect_false(.create_private_file_windows("C:/x"))
})

test_that(".acl_is_owner_only() accepts only one protected owner rule", {
  sid <- "S-1-5-21-1-2-3-500"
  rule <- paste0("rule=", sid, ";Allow;FullControl;False")

  expect_true(.acl_is_owner_only(c(sid, "protected=True", rule)))
  # Not protected, so rules can still be inherited.
  expect_false(.acl_is_owner_only(c(sid, "protected=False", rule)))
  # Everyone can also read.
  everyone <- "rule=S-1-1-0;Allow;ReadAndExecute, Synchronize;False"
  expect_false(.acl_is_owner_only(c(sid, "protected=True", rule, everyone)))
  # Inherited, read-only, denied, another account, no rule, no SID.
  expect_false(.acl_is_owner_only(
    c(sid, "protected=True", paste0("rule=", sid, ";Allow;FullControl;True"))
  ))
  expect_false(.acl_is_owner_only(
    c(sid, "protected=True", paste0("rule=", sid, ";Allow;Read;False"))
  ))
  expect_false(.acl_is_owner_only(
    c(sid, "protected=True", paste0("rule=", sid, ";Deny;FullControl;False"))
  ))
  expect_false(.acl_is_owner_only(
    c(sid, "protected=True", "rule=S-1-5-21-9-9-9-1001;Allow;FullControl;False")
  ))
  expect_false(.acl_is_owner_only(c(sid, "protected=True")))
  expect_false(.acl_is_owner_only(
    c("LA", "protected=True", "rule=LA;Allow;FullControl;False")
  ))
})

test_that("install = TRUE saves nothing if the file can't be protected", {
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  path <- file.path(home, ".Renviron")
  writeLines("OTHER_KEY=keep-me", path)
  local_confirm(TRUE)
  local_mocked_bindings(.protect_file = function(path) FALSE)

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_equal(readLines(path), "OTHER_KEY=keep-me")
  expect_equal(list.files(home, all.files = TRUE, no.. = TRUE), ".Renviron")
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "fake-client-id")
})

test_that("the default .Renviron is the one R reads at startup", {
  # A fresh R process, so R works out its home folder from the variables set
  # here. On Windows, R_USER comes before HOME.
  skip_on_cran()
  r_user <- normalizePath(withr::local_tempdir(), winslash = "/")
  home <- normalizePath(withr::local_tempdir(), winslash = "/")
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      paste0(
        ".renviron_path <- ",
        paste(deparse(.renviron_path), collapse = "\n")
      ),
      "cat(.renviron_path())"
    ),
    script
  )
  withr::local_envvar(R_USER = r_user, HOME = home, R_ENVIRON_USER = NA)

  out <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE
  )

  expected <- if (.Platform$OS.type == "windows") r_user else home
  expect_equal(out, file.path(expected, ".Renviron"))
})
