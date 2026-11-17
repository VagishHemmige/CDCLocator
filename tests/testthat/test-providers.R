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

test_that("cdc_providers() searches every service in batches of three", {
  # One request with all 14 services returns no providers, while requests
  # with up to three return the union of their results (observed 2026-11-16).
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110")

  batches <- lapply(mock$reqs, req_services)
  expect_equal(lengths(batches), c(3, 3, 3, 3, 2))
  expect_equal(unlist(batches), .cdc_locator_services)
  for (req in mock$reqs) {
    query <- httr2::url_parse(req$url)$query
    expect_equal(query$zipcode, "02110")
    expect_equal(query$radius, "10")
  }
})

test_that("cdc_providers() sends up to three services in one request", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110", services = c("hivtesting", "pep", "hepc"))

  expect_length(mock$reqs, 1)
  expect_equal(req_services(mock$req), c("hivtesting", "pep", "hepc"))
})

test_that("cdc_providers() splits more than three services into batches", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110", services = c("hivtesting", "pep", "hepc", "condom"))

  expect_equal(
    lapply(mock$reqs, req_services),
    list(c("hivtesting", "pep", "hepc"), "condom")
  )
})

test_that("cdc_providers() combines batches and drops repeated providers", {
  local_fake_credentials()
  local_mock_locator(function(req) {
    if ("hivtesting" %in% req_services(req)) {
      json_response(list(fake_provider("a1"), fake_provider("b2")))
    } else {
      json_response(list(fake_provider("b2"), fake_provider("c3")))
    }
  })
  out <- cdc_providers(
    "02110",
    services = c("hivtesting", "pep", "hepc", "condom")
  )

  expect_equal(out$id, c("a1", "b2", "c3"))
})

test_that("cdc_providers() fails if any batch fails", {
  local_fake_credentials()
  local_mock_locator(function(req) {
    if ("condom" %in% req_services(req)) {
      json_response(list(message = "error"), status = 500L)
    } else {
      json_response(list(fake_provider("a1")))
    }
  })
  expect_error(
    cdc_providers("02110", services = c("hivtesting", "pep", "hepc", "condom")),
    class = "httr2_http_500"
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

test_that("cdc_providers() returns zero rows when no providers match", {
  local_fake_credentials()
  local_mock_locator(json_response(list()))
  out <- cdc_providers("20176")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c("type", "id", "attributes"))
})

test_that("cdc_providers() treats only an empty array as no matches", {
  local_fake_credentials()

  local_mock_locator(raw_json_response("[]"))
  expect_equal(nrow(cdc_providers("20176")), 0)

  local_mock_locator(raw_json_response("null"))
  expect_snapshot(cdc_providers("20176"), error = TRUE)

  local_mock_locator(raw_json_response("{}"))
  expect_snapshot(cdc_providers("20176"), error = TRUE)

  local_mock_locator(raw_json_response('{"Message": "unexpected"}'))
  expect_snapshot(cdc_providers("20176"), error = TRUE)
})
