test_that(".cdc_locator_request() builds the URL, query and auth headers", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers", query = list(zipcode = "02110"))

  expect_equal(req$url, "https://locator-api.hiv.gov/providers?zipcode=02110")
  expect_equal(req$headers$client_id, "fake-client-id")
  expect_equal(req$headers$client_token, "fake-client-token")
})

test_that(".cdc_locator_request() rejects a query that is not a list", {
  local_fake_credentials()
  expect_snapshot(
    .cdc_locator_request("providers", query = "zip"),
    error = TRUE
  )
})

test_that(".perform_cdc_locator_request() parses JSON", {
  local_fake_credentials()
  local_mock_locator(json_response(list(zipcode = "02110")))
  body <- .perform_cdc_locator_request(.cdc_locator_request("address/place"))
  expect_equal(body, list(zipcode = "02110"))
})

test_that(".perform_cdc_locator_request() turns HTTP errors into R errors", {
  local_fake_credentials()
  local_mock_locator(json_response(
    list(message = "Unauthorized"),
    status = 401L
  ))
  expect_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    class = "httr2_http_401"
  )
})

test_that(".cdc_locator_request() hides credentials when printed", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers")

  expect_setequal(
    attr(req$headers, "redact"),
    c("client_id", "client_token")
  )
  printed <- c(
    capture.output(print(req), type = "output"),
    capture.output(print(req), type = "message")
  )
  expect_match(printed, "client_token", all = FALSE)
  expect_no_match(printed, "fake-client")
})

test_that(".cdc_locator_request() sets a user agent, retries and pacing", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers")

  expect_match(req$options$useragent, "^CDCLocator/")
  expect_equal(req$policies$retry_max_tries, 3)
  # The throttle policy's name differs between httr2 versions.
  expect_match(names(req$policies), "^throttle", all = FALSE)
})

test_that("API error text is shown, without credentials", {
  local_fake_credentials()
  local_mock_locator(raw_json_response(
    "invalid entry: radius must be 50 or less",
    status = 500L
  ))
  expect_snapshot(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    error = TRUE
  )

  local_mock_locator(raw_json_response(
    "echo fake-client-id fake-client-token",
    status = 500L
  ))
  err <- tryCatch(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    error = \(e) e
  )
  expect_s3_class(err, "httr2_http_500")
  expect_no_match(conditionMessage(err), "fake-client")
  expect_no_match(paste(err$body, collapse = " "), "fake-client")
})

test_that("a 403 explains the two usual causes", {
  local_fake_credentials()
  local_mock_locator(raw_json_response(
    '{"Message":"User is not authorized to access this resource"}',
    status = 403L
  ))
  expect_snapshot(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    error = TRUE
  )

  local_mock_locator(httr2::response(
    status_code = 403L,
    headers = list(`Content-Type` = "text/html"),
    body = charToRaw("<html><body><h1>403 ERROR</h1>blocked</body></html>")
  ))
  err <- tryCatch(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    error = \(e) e
  )
  expect_s3_class(err, "httr2_http_403")
  expect_no_match(conditionMessage(err), "<html|<h1>")
  expect_match(conditionMessage(err), "cloud", fixed = TRUE)
})
