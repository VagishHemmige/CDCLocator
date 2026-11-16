test_that(".validate_cdc_locator_services() returns every service for NULL", {
  expect_equal(.validate_cdc_locator_services(NULL), .cdc_locator_services)
})

test_that(".validate_cdc_locator_services() returns valid values in lowercase", {
  expect_equal(
    .validate_cdc_locator_services(c("hivtesting", "Hepc", "PEP")),
    c("hivtesting", "hepc", "pep")
  )
})

test_that(".validate_cdc_locator_services() rejects bad input", {
  expect_snapshot(.validate_cdc_locator_services(1), error = TRUE)
  expect_snapshot(
    .validate_cdc_locator_services(c("hivtesting", "nope")),
    error = TRUE
  )
})
