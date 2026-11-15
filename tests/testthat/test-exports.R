test_that("every user-facing function is exported", {
  expect_contains(
    getNamespaceExports("CDCLocator"),
    c(
      "cdc_address_place",
      "cdc_address_point",
      "cdc_locator_set_credentials",
      "cdc_providers",
      "cdc_providers_tidy"
    )
  )
})
