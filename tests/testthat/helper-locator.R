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

# Mocks every request in the calling test. `response` is an httr2 response,
# or a function of the request that returns one. Returns an environment whose
# `req` holds the last request and `reqs` every request, in order.
local_mock_locator <- function(response, env = parent.frame()) {
  seen <- new.env()
  seen$reqs <- list()
  httr2::local_mocked_responses(
    function(req) {
      seen$req <- req
      seen$reqs <- c(seen$reqs, list(req))
      if (is.function(response)) response(req) else response
    },
    env = env
  )
  seen
}

# The comma-separated services of a mocked request, as a character vector.
req_services <- function(req) {
  strsplit(httr2::url_parse(req$url)$query$services, ",")[[1]]
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
