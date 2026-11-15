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
