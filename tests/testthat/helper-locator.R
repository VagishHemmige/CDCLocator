# Requests are paced half a second apart in normal use; not in tests, except
# the test of the pacing itself.
options(cdclocator.min_gap = 0)

# Sets obviously fake credentials for the calling test. Never use real ones.
local_fake_credentials <- function(env = parent.frame()) {
  withr::local_envvar(
    CDC_LOCATOR_CLIENT_ID = "fake-client-id",
    CDC_LOCATOR_CLIENT_TOKEN = "fake-client-token",
    .local_envir = env
  )
}

# Builds a JSON response like the Locator API's, for httr2 mocking.
json_response <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  )
}

# Builds a response from literal JSON text, for shapes toJSON() can't produce.
raw_json_response <- function(text, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(text)
  )
}

# Mocks every request in the calling test with `response` and returns an
# environment whose `req` holds the last request made.
local_mock_locator <- function(response, env = parent.frame()) {
  seen <- new.env()
  httr2::local_mocked_responses(
    function(req) {
      seen$req <- req
      response
    },
    env = env
  )
  seen
}

# Skips unless live tests were asked for and credentials are present.
skip_if_not_live <- function() {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("CDC_LOCATOR_LIVE_TESTS"), "true"),
    "Set CDC_LOCATOR_LIVE_TESTS=true to run live API tests."
  )
  skip_if_not(
    nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_ID")) &&
      nzchar(Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN")),
    "Live API tests need CDC_LOCATOR_CLIENT_ID and CDC_LOCATOR_CLIENT_TOKEN."
  )
}

# One provider in the shape the API returned on 2026-11-16 (values invented).
fake_provider <- function(
  id = "a1",
  languages = list("English"),
  website = "https://example.org"
) {
  list(
    type = "provider",
    id = id,
    attributes = list(
      name = paste("Clinic", id),
      address = list(
        city = "Leesburg",
        country = "US",
        state = "VA",
        street = "1 Main St",
        zipcode = "20176"
      ),
      categories = NULL,
      contact = list(others = NULL, telephone = "555-0100", website = website),
      languages = languages,
      point = list(latitude = 39.1, longitude = -77.5),
      distance = 1.2
    )
  )
}

# The value a request will send for a header. httr2 >= 1.1 stores redacted
# headers as references and reveals them with req_get_headers(); older
# versions store plain strings.
req_header_value <- function(req, name) {
  httr2_ns <- asNamespace("httr2")
  if (exists("req_get_headers", envir = httr2_ns)) {
    httr2::req_get_headers(req, redacted = "reveal")[[name]]
  } else {
    req$headers[[name]]
  }
}

# Everything printing an object writes, to standard output or as messages,
# without showing it.
printed_text <- function(x) {
  messages <- character()
  output <- utils::capture.output(withCallingHandlers(
    print(x),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  ))
  c(output, messages)
}

# The condition an expression signals.
catch_error <- function(expr) {
  tryCatch(expr, error = \(e) e)
}
