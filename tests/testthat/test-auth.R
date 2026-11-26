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

test_that("install = TRUE refuses a .Renviron that is a symbolic link", {
  # Making links on Windows needs extra rights.
  skip_on_os("windows")
  home <- local_temp_home()
  withr::local_envvar(CDC_LOCATOR_CLIENT_ID = NA, CDC_LOCATOR_CLIENT_TOKEN = NA)
  target <- file.path(home, "dotfiles.Renviron")
  writeLines("OTHER_KEY=keep-me", target)
  path <- file.path(home, ".Renviron")
  file.symlink(target, path)
  seen <- local_confirm(TRUE)

  expect_snapshot(
    cdc_locator_set_credentials(
      "fake-client-id",
      "fake-client-token",
      install = TRUE
    ),
    error = TRUE,
    transform = \(x) gsub(home, "<home>", x, fixed = TRUE)
  )
  expect_null(seen$question)
  expect_equal(Sys.readlink(path), target)
  expect_equal(readLines(target), "OTHER_KEY=keep-me")
  expect_equal(Sys.getenv("CDC_LOCATOR_CLIENT_ID"), "")
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
