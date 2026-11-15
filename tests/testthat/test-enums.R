test_that(".validate_cdc_locator_services() accepts NULL and valid values", {
  expect_true(.validate_cdc_locator_services(NULL))
  expect_true(.validate_cdc_locator_services(c("hivtesting", "pep")))
})

test_that(".validate_cdc_locator_services() rejects bad input", {
  expect_snapshot(.validate_cdc_locator_services(1), error = TRUE)
  expect_snapshot(
    .validate_cdc_locator_services(c("hivtesting", "nope")),
    error = TRUE
  )
})
