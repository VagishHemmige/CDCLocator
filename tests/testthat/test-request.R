test_that(".cdc_locator_request() builds the URL, query and auth headers", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers", query = list(zipcode = "02110"))

  expect_equal(req$url, "https://locator-api.hiv.gov/providers?zipcode=02110")
  expect_equal(req_header_value(req, "client_id"), "fake-client-id")
  expect_equal(req_header_value(req, "client_token"), "fake-client-token")
})

test_that(".cdc_locator_request() rejects a query that is not a list", {
  local_fake_credentials()
  expect_snapshot(
    .cdc_locator_request("providers", query = "zip"),
    error = TRUE
  )
})

test_that(".perform_cdc_locator_request() parses JSON", {
  local_fake_credentials()
  local_mock_locator(json_response(list(zipcode = "02110")))
  body <- .perform_cdc_locator_request(.cdc_locator_request("address/place"))
  expect_equal(body, list(zipcode = "02110"))
})

test_that(".perform_cdc_locator_request() turns HTTP errors into R errors", {
  local_fake_credentials()
  local_mock_locator(json_response(
    list(message = "Unauthorized"),
    status = 401L
  ))
  expect_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers")),
    class = "httr2_http_401"
  )
})

test_that(".cdc_locator_request() hides credentials when printed", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers")
  printed <- printed_text(req)

  expect_match(printed, "client_token", all = FALSE)
  expect_no_match(printed, "fake-client")
  expect_equal(req_header_value(req, "client_token"), "fake-client-token")
})

test_that(".cdc_locator_request() sets a user agent and retries", {
  local_fake_credentials()
  req <- .cdc_locator_request("providers")

  expect_match(req$options$useragent, "^CDCLocator/")
  expect_equal(req$policies$retry_max_tries, 3)
})

test_that("requests are sent at least half a second apart", {
  local_fake_credentials()
  local_mock_locator(json_response(list()))
  withr::local_options(cdclocator.min_gap = 0.5)
  req <- .cdc_locator_request("providers")

  .perform_cdc_locator_request(req)
  start <- Sys.time()
  for (i in 1:4) {
    .perform_cdc_locator_request(req)
  }
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  expect_gte(elapsed, 1.95)
})

test_that("retries and the next call are also paced", {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  local_fake_credentials()
  withr::local_options(cdclocator.min_gap = 0.5)

  # The first request gets a 503 asking for an immediate retry; the rest
  # succeed. The server records when each request arrives.
  app <- webfakes::new_app()
  app$locals$arrivals <- numeric()
  app$get("/providers", function(req, res) {
    req$app$locals$arrivals <- c(
      req$app$locals$arrivals,
      as.numeric(Sys.time())
    )
    if (length(req$app$locals$arrivals) == 1) {
      res$set_status(503L)$set_header("Retry-After", "0")$send_json(list())
    } else {
      res$send_json(list())
    }
  })
  app$get("/arrivals", function(req, res) {
    res$send_json(
      as.list(req$app$locals$arrivals),
      auto_unbox = TRUE,
      digits = NA
    )
  })
  server <- webfakes::local_app_process(app)

  req <- .cdc_locator_request("providers") |>
    httr2::req_url(server$url("/providers"))
  .perform_cdc_locator_request(req)
  .perform_cdc_locator_request(req)

  arrivals <- unlist(httr2::resp_body_json(httr2::req_perform(
    httr2::request(server$url("/arrivals"))
  )))
  expect_length(arrivals, 3)
  expect_gte(min(diff(arrivals)), 0.45)
})

test_that("mocked tests turn pacing off only while they run", {
  withr::local_options(cdclocator.min_gap = NULL)
  in_mocked_test <- function() {
    local_mock_locator(json_response(list()))
    getOption("cdclocator.min_gap")
  }

  expect_equal(in_mocked_test(), 0)
  expect_null(getOption("cdclocator.min_gap"))
})

test_that("API error text is shown, without credentials", {
  local_fake_credentials()
  local_mock_locator(raw_json_response(
    "invalid entry: radius must be 50 or less",
    status = 500L
  ))
  err <- catch_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers"))
  )
  expect_s3_class(err, "httr2_http_500")
  expect_match(
    conditionMessage(err),
    "The API said: invalid entry: radius must be 50 or less",
    fixed = TRUE
  )

  local_mock_locator(raw_json_response(
    "echo fake-client-id fake-client-token",
    status = 500L
  ))
  err <- catch_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers"))
  )
  expect_s3_class(err, "httr2_http_500")
  expect_no_match(conditionMessage(err), "fake-client")
})

test_that("credentials are scrubbed whole, even when they overlap", {
  auth <- c(client_id = "fake-key", client_token = "fake-key-long-secret")
  expect_equal(
    .scrub_credentials("a fake-key-long-secret b fake-key c", auth),
    "a <REDACTED> b <REDACTED> c"
  )

  auth <- c(client_id = "abcXY", client_token = "XYZ123")
  expect_equal(.scrub_credentials("--abcXYZ123--", auth), "--<REDACTED>--")
})

test_that("a credential at the truncation point never shows in part", {
  local_fake_credentials()
  token <- "fake-client-token"
  text <- paste0(strrep("x", 295), token, strrep("y", 50))
  local_mock_locator(raw_json_response(text, status = 500L))
  err <- catch_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers"))
  )

  # The text is cut after the marker is inserted, so the marker itself may be
  # cut short, but no part of the credential remains.
  expect_no_match(conditionMessage(err), "fake-c", fixed = TRUE)
  expect_match(conditionMessage(err), "xxx<REDA", fixed = TRUE)
})

test_that("a 403 explains the two usual causes", {
  local_fake_credentials()
  local_mock_locator(raw_json_response(
    '{"Message":"User is not authorized to access this resource"}',
    status = 403L
  ))
  err <- catch_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers"))
  )
  expect_s3_class(err, "httr2_http_403")
  expect_match(conditionMessage(err), "not authorized", fixed = TRUE)
  expect_match(conditionMessage(err), "wrong or no longer active", fixed = TRUE)
  expect_match(conditionMessage(err), "cloud server or VPN", fixed = TRUE)

  local_mock_locator(httr2::response(
    status_code = 403L,
    headers = list(`Content-Type` = "text/html"),
    body = charToRaw("<html><body><h1>403 ERROR</h1>blocked</body></html>")
  ))
  err <- catch_error(
    .perform_cdc_locator_request(.cdc_locator_request("providers"))
  )
  expect_s3_class(err, "httr2_http_403")
  expect_no_match(conditionMessage(err), "<html|<h1>")
  expect_match(conditionMessage(err), "cloud server or VPN", fixed = TRUE)
})
