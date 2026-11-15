test_that("cdc_address_point() validates its arguments", {
  local_fake_credentials()
  expect_snapshot(cdc_address_point("42", -71), error = TRUE)
  expect_snapshot(cdc_address_point(42, c(-71, -72)), error = TRUE)
})

test_that("cdc_address_point() sends coordinates and returns one row", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list(
    city = "Boston",
    state = "MA",
    zipcode = "02110",
    point = list(latitude = 42.35, longitude = -71.05)
  )))
  out <- cdc_address_point(42.35, -71.05)

  query <- httr2::url_parse(mock$req$url)$query
  expect_equal(query, list(latitude = "42.35", longitude = "-71.05"))
  expect_equal(out$zipcode, "02110")
})
