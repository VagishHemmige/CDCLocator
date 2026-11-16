tidy_columns <- c(
  "provider_type",
  "provider_id",
  "name",
  "street",
  "city",
  "state",
  "zipcode",
  "country",
  "categories",
  "contact_others",
  "contact_telephone",
  "contact_website",
  "languages",
  "latitude",
  "longitude",
  "distance"
)

test_that("cdc_providers_tidy() flattens the API's provider shape", {
  local_fake_credentials()
  local_mock_locator(json_response(list(
    fake_provider("a1", languages = list("English")),
    fake_provider("b2", languages = list("English", "Spanish"))
  )))
  out <- cdc_providers_tidy(cdc_providers("20176"))

  expect_named(out, tidy_columns, ignore.order = TRUE)
  expect_equal(out$provider_id, c("a1", "b2"))
  expect_equal(out$name, c("Clinic a1", "Clinic b2"))
  expect_equal(out$zipcode, c("20176", "20176"))
  expect_equal(out$country, c("US", "US"))
  expect_equal(out$contact_telephone, c("555-0100", "555-0100"))
  expect_equal(out$latitude, c(39.1, 39.1))
  expect_equal(out$distance, c(1.2, 1.2))
  expect_equal(out$languages, list("English", c("English", "Spanish")))
})

test_that("cdc_providers_tidy() returns zero rows when no providers match", {
  local_fake_credentials()
  local_mock_locator(json_response(list()))
  out <- cdc_providers_tidy(cdc_providers("20176"))

  expect_equal(nrow(out), 0)
  expect_named(out, tidy_columns, ignore.order = TRUE)
})

test_that("cdc_providers_tidy() rejects input that is not a tibble", {
  expect_snapshot(cdc_providers_tidy(data.frame()), error = TRUE)
})
