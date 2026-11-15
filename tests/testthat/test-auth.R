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
