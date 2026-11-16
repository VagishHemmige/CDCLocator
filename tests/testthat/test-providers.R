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

test_that("cdc_providers() sends every service when services is NULL", {
  # The API now requires both radius and services (observed 2026-11-16).
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110")

  query <- httr2::url_parse(mock$req$url)$query
  expect_named(query, c("zipcode", "radius", "services"))
  expect_equal(query$radius, "10")
  expect_equal(
    strsplit(query$services, ",")[[1]],
    c(
      "hivtesting",
      "stdtesting",
      "selftesting",
      "preptesting",
      "pep",
      "condom",
      "hepc",
      "doxypep",
      "mailintesting",
      "ryanwhite",
      "clinics",
      "substanceabuse",
      "mentalhealth",
      "housing"
    )
  )
})

test_that("cdc_providers() matches services in any case and sends lowercase", {
  # The API returns nothing for `Hepc` and matches `hepc` (observed 2026-11-16).
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110", services = c("Hepc", "HIVtesting"))

  query <- httr2::url_parse(mock$req$url)$query
  expect_equal(query$services, "hepc,hivtesting")
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
