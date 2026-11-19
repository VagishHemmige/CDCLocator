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

test_that("cdc_providers() combines empty and non-empty batches in any order", {
  local_fake_credentials()
  four <- c("hivtesting", "pep", "hepc", "condom")
  for (empty_first in c(TRUE, FALSE)) {
    local_mock_locator(function(req) {
      if (("hivtesting" %in% req_services(req)) == empty_first) {
        json_response(list())
      } else {
        json_response(list(fake_provider("a1")))
      }
    })
    out <- cdc_providers("02110", services = four)

    expect_equal(out$id, "a1")
    expect_equal(cdc_providers_tidy(out)$provider_id, "a1")
  }
})

test_that("cdc_providers() rejects providers without an id", {
  local_fake_credentials()
  no_id <- function(id) {
    provider <- fake_provider(id)
    provider$id <- NULL
    provider
  }

  local_mock_locator(json_response(list(no_id("a1"), no_id("b2"))))
  expect_error(
    cdc_providers("02110", services = "hivtesting"),
    class = "cdclocator_unexpected_response"
  )

  local_mock_locator(raw_json_response(
    '[{"type": "provider", "id": null}, {"type": "provider", "id": ""}]'
  ))
  expect_error(
    cdc_providers("02110", services = "hivtesting"),
    class = "cdclocator_unexpected_response"
  )
})

test_that("cdc_providers() rejects an empty services vector without a request", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  expect_snapshot(cdc_providers("02110", services = character()), error = TRUE)
  expect_length(mock$reqs, 0)
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
  expect_named(out, c("type", "id", "search_zipcode", "attributes"))
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

test_that("cdc_providers() errors name the function, not an internal helper", {
  local_fake_credentials()
  local_mock_locator(raw_json_response("radius is not provided", status = 500L))
  err <- catch_error(cdc_providers("02110", services = "hivtesting"))

  expect_s3_class(err, "httr2_http_500")
  expect_equal(as.character(err$call[[1]]), "cdc_providers")
})

test_that("cdc_providers() rejects missing, short or long ZIP codes before any request", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))

  expect_snapshot(cdc_providers(NA_character_), error = TRUE)
  expect_snapshot(cdc_providers("2110"), error = TRUE)
  expect_snapshot(cdc_providers("021101"), error = TRUE)
  expect_snapshot(cdc_providers("02110-1234"), error = TRUE)
  expect_length(mock$reqs, 0)
})

test_that("cdc_providers() explains that a numeric ZIP code loses leading zeros", {
  local_fake_credentials()
  expect_snapshot(cdc_providers(2110), error = TRUE)
})

test_that("cdc_providers() rejects a missing or fractional radius", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))

  expect_snapshot(cdc_providers("02110", radius = NA_real_), error = TRUE)
  expect_snapshot(cdc_providers("02110", radius = 10.5), error = TRUE)
  expect_snapshot(cdc_providers("02110", radius = Inf), error = TRUE)
  expect_snapshot(cdc_providers("02110", radius = 0), error = TRUE)
  expect_length(mock$reqs, 0)
})

test_that("cdc_providers() accepts a whole-number radius given as a double", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers("02110", radius = 25, services = "hivtesting")

  expect_equal(httr2::url_parse(mock$req$url)$query$radius, "25")
})

# ---- several ZIP codes ------------------------------------------------------

# The ZIP code a mocked request searched.
req_zipcode <- function(req) {
  httr2::url_parse(req$url)$query$zipcode
}

test_that("cdc_providers() records the searched ZIP code", {
  local_fake_credentials()
  local_mock_locator(json_response(list(fake_provider("a1"))))
  out <- cdc_providers("20176", services = "hivtesting")

  expect_equal(out$search_zipcode, "20176")
  expect_equal(cdc_providers_tidy(out)$search_zipcode, "20176")
})

test_that("cdc_providers() searches several ZIP codes in order", {
  local_fake_credentials()
  mock <- local_mock_locator(function(req) {
    json_response(list(fake_provider(paste0("id-", req_zipcode(req)))))
  })
  out <- cdc_providers(
    c("20176", "02110", "10001"),
    services = c("hivtesting", "pep", "hepc", "condom"),
    progress = FALSE
  )

  expect_equal(
    vapply(mock$reqs, req_zipcode, character(1)),
    rep(c("20176", "02110", "10001"), each = 2)
  )
  expect_equal(out$id, c("id-20176", "id-02110", "id-10001"))
  expect_equal(out$search_zipcode, c("20176", "02110", "10001"))
})

test_that("cdc_providers() searches a repeated ZIP code once", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))
  cdc_providers(
    c("20176", "02110", "20176"),
    services = "hivtesting",
    progress = FALSE
  )

  expect_equal(
    vapply(mock$reqs, req_zipcode, character(1)),
    c("20176", "02110")
  )
})

test_that("cdc_providers() checks every ZIP code before any request", {
  local_fake_credentials()
  mock <- local_mock_locator(json_response(list()))

  expect_snapshot(cdc_providers(c("20176", "2110", "abcde")), error = TRUE)
  expect_snapshot(cdc_providers(c("20176", NA)), error = TRUE)
  expect_snapshot(cdc_providers(c("20176", "")), error = TRUE)
  expect_snapshot(cdc_providers(character()), error = TRUE)
  expect_snapshot(cdc_providers(c(20176, 2110)), error = TRUE)
  expect_length(mock$reqs, 0)
})

test_that("cdc_providers() keeps each provider once, from the nearest search", {
  local_fake_credentials()
  local_mock_locator(function(req) {
    if (req_zipcode(req) == "20176") {
      json_response(list(
        fake_provider("a1", distance = 8),
        fake_provider("b2", distance = 1)
      ))
    } else {
      json_response(list(
        fake_provider("a1", distance = 2),
        fake_provider("c3", distance = 3)
      ))
    }
  })
  out <- cdc_providers(
    c("20176", "20147"),
    services = "hivtesting",
    progress = FALSE
  )
  tidy <- cdc_providers_tidy(out)

  expect_equal(tidy$provider_id, c("b2", "a1", "c3"))
  expect_equal(tidy$search_zipcode, c("20176", "20147", "20147"))
  expect_equal(tidy$distance, c(1, 2, 3))
})

test_that("cdc_providers(distinct = FALSE) keeps a row per finding ZIP code", {
  local_fake_credentials()
  local_mock_locator(json_response(list(fake_provider("a1"))))
  out <- cdc_providers(
    c("20176", "20147"),
    services = "hivtesting",
    distinct = FALSE,
    progress = FALSE
  )

  expect_equal(out$id, c("a1", "a1"))
  expect_equal(out$search_zipcode, c("20176", "20147"))
})

test_that("cdc_providers(distinct = FALSE) drops repeats within a ZIP code", {
  local_fake_credentials()
  local_mock_locator(json_response(list(fake_provider("a1"))))
  out <- cdc_providers(
    "20176",
    services = c("hivtesting", "pep", "hepc", "condom"),
    distinct = FALSE
  )

  expect_equal(out$id, "a1")
})

test_that("cdc_providers() skips a failing ZIP code among several", {
  local_fake_credentials()
  local_mock_locator(function(req) {
    if (req_zipcode(req) == "00000") {
      raw_json_response("No point found for address", status = 500L)
    } else {
      json_response(list(fake_provider(paste0("id-", req_zipcode(req)))))
    }
  })
  expect_snapshot(
    out <- cdc_providers(
      c("20176", "00000", "10001"),
      services = "hivtesting",
      progress = FALSE
    )
  )

  expect_equal(out$search_zipcode, c("20176", "10001"))
  failures <- attr(out, "failures")
  expect_equal(failures$zipcode, "00000")
  expect_equal(
    failures$message,
    "HTTP 500 Internal Server Error. The API said: No point found for address"
  )
})

test_that("cdc_providers() stops at a failure not tied to one ZIP code", {
  # Only the API's "No point found for address" is known to concern one ZIP
  # code; anything else may hit every ZIP code, so the search stops.
  local_fake_credentials()
  for (failure in list(
    raw_json_response("Bad gateway", status = 502L),
    raw_json_response("Something else went wrong", status = 500L)
  )) {
    mock <- local_mock_locator(function(req) {
      if (req_zipcode(req) == "20147") {
        failure
      } else {
        json_response(list(fake_provider("a1")))
      }
    })

    expect_error(
      cdc_providers(
        c("20176", "20147", "10001"),
        services = "hivtesting",
        progress = FALSE
      ),
      class = "httr2_http"
    )
    expect_equal(
      vapply(mock$reqs, req_zipcode, character(1)),
      c("20176", "20147")
    )
  }
})

test_that("cdc_providers() returns zero rows when every ZIP code fails", {
  local_fake_credentials()
  local_mock_locator(raw_json_response(
    "No point found for address",
    status = 500L
  ))
  out <- suppressWarnings(cdc_providers(
    c("00000", "00001"),
    services = "hivtesting",
    progress = FALSE
  ))

  expect_equal(nrow(out), 0)
  expect_named(out, c("type", "id", "search_zipcode", "attributes"))
  expect_equal(attr(out, "failures")$zipcode, c("00000", "00001"))
})

test_that("cdc_providers() has no failures attribute when nothing failed", {
  local_fake_credentials()
  local_mock_locator(json_response(list()))
  out <- cdc_providers(
    c("20176", "20147"),
    services = "hivtesting",
    progress = FALSE
  )

  expect_null(attr(out, "failures"))
})

test_that("cdc_providers() still errors when its only ZIP code fails", {
  local_fake_credentials()
  local_mock_locator(raw_json_response("No point found", status = 500L))

  expect_error(
    cdc_providers("00000", services = "hivtesting"),
    class = "httr2_http_500"
  )
})

test_that("cdc_providers() stops at a refused request instead of skipping", {
  # A rejected key or blocked network refuses every request the same way.
  local_fake_credentials()
  mock <- local_mock_locator(json_response(
    list(Message = "User is not authorized"),
    status = 403L
  ))

  expect_error(
    cdc_providers(
      c("20176", "20147", "10001"),
      services = "hivtesting",
      progress = FALSE
    ),
    class = "httr2_http_403"
  )
  expect_length(mock$reqs, 1)
})

test_that("cdc_providers() waits 0.5 s for one ZIP code and 1 s for several", {
  withr::local_options(
    cdclocator.min_gap = NULL,
    cdclocator.sweep_gap = NULL
  )
  expect_equal(.cdc_providers_min_gap(NULL, 1), 0.5)
  expect_equal(.cdc_providers_min_gap(NULL, 3), 1)
  expect_equal(.cdc_providers_min_gap(2, 1), 2)

  withr::local_options(cdclocator.min_gap = 3)
  expect_equal(.cdc_providers_min_gap(NULL, 3), 3)
})

test_that("cdc_providers() spaces requests by min_gap, then restores it", {
  local_fake_credentials()
  seen <- numeric()
  local_mock_locator(function(req) {
    seen <<- c(seen, getOption("cdclocator.min_gap"))
    json_response(list())
  })
  cdc_providers(
    c("20176", "20147"),
    services = "hivtesting",
    min_gap = 0.1,
    progress = FALSE
  )

  expect_equal(seen, c(0.1, 0.1))
  expect_equal(getOption("cdclocator.min_gap"), 0)
})

test_that("cdc_providers() rejects a min_gap below the package's minimum", {
  local_fake_credentials()
  withr::local_options(cdclocator.min_gap = NULL)
  expect_snapshot(cdc_providers("20176", min_gap = 0.1), error = TRUE)
  expect_snapshot(cdc_providers("20176", min_gap = "1"), error = TRUE)
})

test_that("cdc_providers() checks progress and distinct", {
  local_fake_credentials()
  expect_snapshot(cdc_providers("20176", progress = NA), error = TRUE)
  expect_snapshot(cdc_providers("20176", distinct = "yes"), error = TRUE)
})

test_that("cdc_providers() summarizes a search of several ZIP codes", {
  local_fake_credentials()
  local_mock_locator(function(req) {
    if (req_zipcode(req) == "00000") {
      raw_json_response("No point found for address", status = 500L)
    } else {
      json_response(list(
        fake_provider("shared"),
        fake_provider(req_zipcode(req))
      ))
    }
  })

  expect_snapshot(
    out <- cdc_providers(c("20176", "20147"), services = "hivtesting")
  )
  expect_snapshot(
    out <- cdc_providers(c("20176", "00000"), services = "hivtesting")
  )
  expect_snapshot(
    out <- cdc_providers(
      c("20176", "20147"),
      services = "hivtesting",
      distinct = FALSE
    )
  )
  expect_silent(cdc_providers(
    c("20176", "20147"),
    services = "hivtesting",
    progress = FALSE
  ))
  expect_silent(cdc_providers("20176", services = "hivtesting"))
})
