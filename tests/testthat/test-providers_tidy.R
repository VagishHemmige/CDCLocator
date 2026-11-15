# The nested shape below is inferred from cdc_providers_tidy() itself, not
# from a recorded API response.
nested_providers <- function() {
  tibble::tibble(
    id = "a1",
    type = "provider",
    attributes = list(list(
      name = "Example Clinic",
      address = list(
        street = "1 Main St",
        city = "Boston",
        state = "MA",
        zipcode = "02110"
      ),
      contact = list(phone = "555-0100"),
      point = list(latitude = 42.35, longitude = -71.05)
    ))
  )
}

test_that("cdc_providers_tidy() flattens and renames provider columns", {
  out <- cdc_providers_tidy(nested_providers())

  expect_equal(out$provider_id, "a1")
  expect_equal(out$provider_type, "provider")
  expect_equal(out$name, "Example Clinic")
  expect_equal(out$street, "1 Main St")
  expect_equal(out$zipcode, "02110")
  expect_equal(out$contact_phone, "555-0100")
  expect_equal(out$latitude, 42.35)
  expect_equal(out$longitude, -71.05)
})

test_that("cdc_providers_tidy() rejects input that is not a tibble", {
  expect_snapshot(cdc_providers_tidy(data.frame()), error = TRUE)
})
