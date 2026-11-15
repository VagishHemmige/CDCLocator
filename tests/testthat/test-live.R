# Live HIV.gov Locator API checks. They run only with
# CDC_LOCATOR_LIVE_TESTS=true and real credentials in the environment, on the
# maintainer's machine or in live-tests.yaml. Keep requests few and small.

test_that("cdc_providers() returns providers for a real ZIP code", {
  skip_if_not_live()
  out <- cdc_providers("10001", radius = 1)

  expect_s3_class(out, "tbl_df")
  expect_gt(nrow(out), 0)
})

test_that("cdc_providers_tidy() handles a real response", {
  skip_if_not_live()
  out <- cdc_providers_tidy(cdc_providers("10001", radius = 1))

  expect_contains(names(out), c("provider_id", "name", "zipcode", "latitude"))
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
