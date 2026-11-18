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
  home <- withr::local_tempdir(.local_envir = env)
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
