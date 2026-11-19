# Live HIV.gov Locator API checks. They run only with
# CDC_LOCATOR_LIVE_TESTS=true and real credentials in the environment, on the
# maintainer's machine or in live-tests.yaml. Keep requests few and small.

test_that("live tests run with the package's real request pacing", {
  skip_if_not_live()
  expect_null(getOption("cdclocator.min_gap"))
})

test_that("cdc_providers() finds providers with the default services", {
  # The default searches all 14 services in five requests.
  skip_if_not_live()
  out <- cdc_providers("10001", radius = 1)

  expect_s3_class(out, "tbl_df")
  expect_gt(nrow(out), 0)
  expect_false(anyDuplicated(out$id) > 0)
})

test_that("cdc_providers() searches two ZIP codes, each provider once", {
  # Two requests: one service, two ZIP codes a few miles apart.
  skip_if_not_live()
  out <- cdc_providers(
    c("10001", "10018"),
    radius = 1,
    services = "hivtesting",
    progress = FALSE
  )

  expect_gt(nrow(out), 0)
  expect_false(anyDuplicated(out$id) > 0)
  expect_in(out$search_zipcode, c("10001", "10018"))
  expect_null(attr(out, "failures"))
})

test_that("cdc_providers_tidy() flattens real provider rows", {
  skip_if_not_live()
  out <- cdc_providers_tidy(
    cdc_providers("10001", radius = 1, services = "hivtesting")
  )

  expect_gt(nrow(out), 0)
  expect_contains(
    names(out),
    c("provider_id", "name", "zipcode", "latitude", "languages", "distance")
  )
  expect_type(out$name, "character")
  expect_type(out$latitude, "double")
  expect_type(out$languages, "list")
})

test_that("cdc_address_place() and cdc_address_point() return a ZIP code", {
  skip_if_not_live()
  place <- cdc_address_place("New York", "NY")
  point <- cdc_address_point(40.7506, -73.9972)

  for (out in list(place, point)) {
    expect_equal(nrow(out), 1)
    expect_type(out$zipcode, "character")
    expect_false(is.na(out$zipcode))
    expect_match(out$zipcode, "^[0-9]{5}$")
  }
})
