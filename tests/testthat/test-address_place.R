test_that("cdc_address_place() validates its arguments", {
  local_fake_credentials()
  expect_snapshot(cdc_address_place(1, "MA"), error = TRUE)
  expect_snapshot(cdc_address_place("Boston", ""), error = TRUE)
})

test_that("cdc_address_place() sends city and state and returns one row", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list(
    city = "Boston",
    state = "MA",
    zipcode = "02110",
    point = list(latitude = 42.35, longitude = -71.05)
  )))
  out <- cdc_address_place("Boston", "MA")

  url <- httr2::url_parse(mock$req$url)
  expect_equal(url$path, "/address/place")
  query <- url$query
  expect_equal(query, list(city = "Boston", state = "MA"))
  expect_equal(
    out,
    tibble::tibble(
      city = "Boston",
      state = "MA",
      zipcode = "02110",
      latitude = 42.35,
      longitude = -71.05
    )
  )
})
