test_that("cdc_providers() validates its arguments", {
  local_fake_credentials()
  expect_snapshot(cdc_providers(2110), error = TRUE)
  expect_snapshot(cdc_providers(""), error = TRUE)
  expect_snapshot(cdc_providers("02110", radius = "10"), error = TRUE)
  expect_snapshot(cdc_providers("02110", radius = 51), error = TRUE)
  expect_snapshot(cdc_providers("02110", services = "nope"), error = TRUE)
})

test_that("cdc_providers() sends zipcode, radius and services", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110", radius = 5, services = c("hivtesting", "pep"))

  query <- httr2::url_parse(mock$req$url)$query
  expect_equal(query$zipcode, "02110")
  expect_equal(query$radius, "5")
  expect_equal(query$services, "hivtesting,pep")
})

test_that("cdc_providers() leaves services out when it is NULL", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110")

  query <- httr2::url_parse(mock$req$url)$query
  expect_named(query, c("zipcode", "radius"))
  expect_equal(query$radius, "10")
})

test_that("cdc_providers() returns a tibble", {
  local_fake_credentials()
  local_mock_locator(json_response(list(
    list(id = "a1", type = "provider"),
    list(id = "b2", type = "provider")
  )))
  out <- cdc_providers("02110")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c("a1", "b2"))
})
