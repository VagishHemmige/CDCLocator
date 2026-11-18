# -------------------------------------------------------------------------
# Internal request helpers for CDC Locator API
# -------------------------------------------------------------------------

#' Build an httr2 request for the CDC Locator API
#'
#' This is the single entry point for all API requests in the package.
#'
#' @param endpoint Character scalar. API endpoint path (e.g. "providers").
#' @param query Named list of query parameters.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#'
#' @return An httr2 request object.
#' @noRd
.cdc_locator_request <- function(
  endpoint,
  query = list(),
  client_id = NULL,
  client_token = NULL
) {
  # Defensive checks (cheap and helpful)
  stopifnot(
    is.character(endpoint),
    length(endpoint) == 1
  )

  if (!is.list(query)) {
    cli::cli_abort("{.arg query} must be a named list.")
  }

  auth <- .build_auth_headers(
    client_id = client_id,
    client_token = client_token
  )

  httr2::request("https://locator-api.hiv.gov") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(!!!auth, .redact = names(auth)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(.cdc_locator_user_agent()) |>
    httr2::req_timeout(30) |>
    # 429 and 503 only; the API reports bad input as 500, which must not retry.
    # Retries keep the same minimum gap as separate calls.
    httr2::req_retry(
      max_tries = 3,
      backoff = .cdc_locator_backoff,
      after = .cdc_locator_retry_after
    ) |>
    httr2::req_error(body = \(resp) .cdc_locator_error_body(resp, auth))
}

.cdc_locator_user_agent <- function() {
  paste0(
    "CDCLocator/",
    utils::packageVersion("CDCLocator"),
    " (https://github.com/VagishHemmige/CDCLocator)"
  )
}

# Extra lines for an HTTP error: the API's own message, with credentials
# removed, and a hint for the usual causes of a 403.
.cdc_locator_error_body <- function(resp, auth) {
  type <- httr2::resp_content_type(resp) %||% ""
  text <- tryCatch(httr2::resp_body_string(resp), error = \(e) "")

  message <- NULL
  if (!grepl("html", type, fixed = TRUE) && nzchar(trimws(text))) {
    message <- .scrub_credentials(text, auth)
    message <- gsub("[[:space:]]+", " ", trimws(message))
    if (nchar(message) > 300) {
      message <- paste0(substr(message, 1, 300), "...")
    }
    message <- c("i" = paste0("The API said: ", message))
  }

  hint <- NULL
  if (httr2::resp_status(resp) == 403) {
    hint <- c(
      "i" = "The API refused this request. The usual causes are:",
      "*" = "the client ID or token is wrong or no longer active, or",
      "*" = paste(
        "the request came from a network the API blocks: outside the US,",
        "or a cloud server or VPN (for example GitHub Actions runners on",
        "Microsoft Azure)."
      )
    )
  }

  c(message, hint)
}

# The minimum time between requests, in seconds.
.cdc_locator_min_gap <- function() {
  getOption("cdclocator.min_gap", 0.5)
}

# Seconds before a retry when the server gives no Retry-After: httr2's
# default exponential backoff, but never less than the minimum gap.
.cdc_locator_backoff <- function(i) {
  max(.cdc_locator_min_gap(), round(min(stats::runif(1, 1, 2^i), 60), 1))
}

# Seconds before a retry when the server gives Retry-After: what it asks
# for, but never less than the minimum gap. NA falls back to the backoff.
.cdc_locator_retry_after <- function(resp) {
  after <- httr2::resp_retry_after(resp)
  if (is.na(after)) NA else max(after, .cdc_locator_min_gap())
}

# When the last request was sent or finished, shared by all calls in the
# session.
.cdc_locator_state <- new.env(parent = emptyenv())

# Waits until at least `min_gap` seconds have passed since the previous
# request. The default provider search makes five requests, so this keeps it
# to about two per second. Done here rather than with httr2::req_throttle(),
# whose token bucket (httr2 >= 1.1) allows short bursts. Tests set the
# `cdclocator.min_gap` option to 0 to run quickly.
.cdc_locator_wait <- function(min_gap = .cdc_locator_min_gap()) {
  last <- .cdc_locator_state$last
  if (!is.null(last)) {
    wait <- min_gap - as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (wait > 0) {
      Sys.sleep(wait)
    }
  }
  .cdc_locator_state$last <- Sys.time()
  invisible()
}

# Removes credential values from text before it is shown. Every occurrence of
# every value is found in the original text first, and overlapping or touching
# matches are merged, so one value can't break a match of another (for example
# a client ID that is the start of the token). Done on the full text, before
# any truncation.
.scrub_credentials <- function(text, auth) {
  values <- unique(auth[nzchar(auth)])
  spans <- do.call(
    rbind,
    lapply(values, \(value) {
      starts <- gregexpr(value, text, fixed = TRUE)[[1]]
      starts <- starts[starts > 0]
      if (length(starts) == 0) {
        return(NULL)
      }
      cbind(start = starts, end = starts + nchar(value) - 1)
    })
  )
  if (is.null(spans)) {
    return(text)
  }

  spans <- spans[order(spans[, "start"]), , drop = FALSE]
  merged <- spans[1, , drop = FALSE]
  for (i in seq_len(nrow(spans))[-1]) {
    last <- nrow(merged)
    if (spans[i, "start"] <= merged[last, "end"] + 1) {
      merged[last, "end"] <- max(merged[last, "end"], spans[i, "end"])
    } else {
      merged <- rbind(merged, spans[i, ])
    }
  }

  # Replace from the end so earlier positions stay valid.
  for (i in rev(seq_len(nrow(merged)))) {
    text <- paste0(
      substr(text, 1, merged[i, "start"] - 1),
      "<REDACTED>",
      substr(text, merged[i, "end"] + 1, nchar(text))
    )
  }
  text
}

#' Perform a CDC Locator API request and return parsed JSON
#'
#' Centralizes response checking and JSON parsing.
#'
#' @param req An httr2 request object.
#' @param call The frame named in errors: the user-facing function.
#'
#' @return Parsed JSON content (simplified).
#' @noRd
.perform_cdc_locator_request <- function(req, call = parent.frame()) {
  .cdc_locator_wait()
  # Retries happen inside req_perform(), so time the next call from when this
  # one finished, whether it succeeded or failed.
  on.exit(.cdc_locator_state$last <- Sys.time(), add = TRUE)
  resp <- httr2::req_perform(req, error_call = call)

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
