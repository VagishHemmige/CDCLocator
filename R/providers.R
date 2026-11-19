# -------------------------------------------------------------------------
# Provider lookup by ZIP code
# -------------------------------------------------------------------------

#' Find CDC HIV-related service providers by ZIP code
#'
#' Queries the CDC Locator API to return service providers within a given
#' radius of one or more ZIP codes.
#'
#' The API returns providers offering any of the requested services, but
#' returned no providers when all 14 services were requested at once (observed
#' 2026-11-16). So services are requested at most three at a time, and the
#' results are combined: the default, every service, takes five requests per
#' ZIP code. Providers returned by more than one request for the same ZIP code
#' are kept once, by `id`.
#'
#' @section Several ZIP codes:
#' Give `zipcode` a vector to search around each ZIP code in turn. Every ZIP
#' code is checked before any request is sent, and a repeated ZIP code is
#' searched once. Nearby circles overlap, so with `distinct = TRUE` a provider
#' found from more than one ZIP code is kept once, from the search where it was
#' nearest; its `distance` and `search_zipcode` then belong together.
#'
#' With one ZIP code, an error stops the call. With several, a ZIP code whose
#' search fails (for example an unknown ZIP code, which the API reports as
#' HTTP 500 "No point found for address") is skipped, the search carries on,
#' and a warning lists the skipped ZIP codes at the end. They are also kept,
#' with the API's message, in `attr(result, "failures")`; read it straight
#' away, because most data-frame operations, including [cdc_providers_tidy()],
#' drop it. A refused request (HTTP 401 or 403), too many requests (429), the
#' service being unavailable (503), a network failure or an unexpected
#' response still stops the call, because it would affect every remaining ZIP
#' code.
#'
#' Requests are spaced at least `min_gap` seconds apart: 0.5 seconds for one
#' ZIP code and 1 second for several, unless you set it. A search of 30 ZIP
#' codes for every service is 150 requests, about two and a half minutes. In
#' an interactive session a progress bar shows how far the search has got.
#' Pressing Esc stops the search and discards what was found so far, so for a
#' long search it can help to search in smaller groups of ZIP codes.
#'
#' @param zipcode One or more five-digit ZIP codes as strings, such as
#'   `"02110"`. A number would lose leading zeros, so it is rejected.
#' @param radius Search radius in miles: a whole number from 1 to 50. The
#'   default is 10.
#' @param services Character vector of services to search for, matched in
#'   any case: `"hivtesting"`, `"stdtesting"`, `"selftesting"`,
#'   `"preptesting"`, `"pep"`, `"condom"`, `"hepc"`, `"doxypep"`,
#'   `"mailintesting"`, `"ryanwhite"`, `"clinics"`, `"substanceabuse"`,
#'   `"mentalhealth"` or `"housing"`. `NULL`, the default, searches for all
#'   of them. More than three services take more than one request.
#' @param client_id Optional CDC client ID override.
#' @param client_token Optional CDC client token override.
#' @param distinct With several ZIP codes, `TRUE` (the default) keeps each
#'   provider once, from the search where it was nearest. `FALSE` keeps one
#'   row for each ZIP code that found it.
#' @param min_gap The least time between requests, in seconds, from 0.5 to
#'   60. `NULL`, the default, means 0.5 for one ZIP code and 1 for several.
#' @param progress With several ZIP codes, whether to show a progress bar
#'   (only in interactive sessions) and a one-line summary at the end.
#'
#' @return A tibble of service providers, with columns `type`, `id`,
#'   `search_zipcode` (the ZIP code whose search found the provider) and
#'   `attributes`. Use [cdc_providers_tidy()] to flatten it.
#' @export
cdc_providers <- function(
  zipcode,
  radius = 10,
  services = NULL,
  client_id = NULL,
  client_token = NULL,
  distinct = TRUE,
  min_gap = NULL,
  progress = TRUE
) {
  # ---- argument validation ------------------------------------------------
  caller <- environment()
  .check_zipcodes(zipcode, "zipcode", caller)
  .check_number(
    radius,
    "radius",
    min = 1,
    max = 50,
    whole = TRUE,
    unit = " miles",
    call = caller
  )
  radius <- as.integer(radius)

  services <- .validate_cdc_locator_services(services)

  .check_flag(distinct, "distinct", caller)
  .check_flag(progress, "progress", caller)
  zipcode <- unique(zipcode)
  several <- length(zipcode) > 1
  if (!is.null(min_gap)) {
    .check_number(
      min_gap,
      "min_gap",
      min = min(.cdc_locator_min_gap(), 0.5),
      max = 60,
      unit = " seconds",
      call = caller
    )
  }

  # ---- pacing: applies to every request and retry in this call --------------
  gap <- .cdc_providers_min_gap(min_gap, length(zipcode))
  old <- options(cdclocator.min_gap = gap)
  on.exit(options(old), add = TRUE)

  if (!several) {
    return(.cdc_providers_zipcode(
      zipcode,
      radius,
      services,
      client_id,
      client_token,
      call = caller
    ))
  }

  # ---- several ZIP codes ----------------------------------------------------
  found <- 0L
  if (progress) {
    cli::cli_progress_bar(
      format = paste(
        "Searching ZIP codes {cli::pb_bar} {cli::pb_current}/{cli::pb_total}",
        "| {found} found | ETA {cli::pb_eta}"
      ),
      total = length(zipcode),
      .envir = caller
    )
  }

  results <- vector("list", length(zipcode))
  failures <- list()
  for (i in seq_along(zipcode)) {
    # `[<-` with list(), because `[[<-` with NULL would drop the element.
    results[i] <- list(tryCatch(
      .cdc_providers_zipcode(
        zipcode[[i]],
        radius,
        services,
        client_id,
        client_token,
        call = caller
      ),
      httr2_http = function(e) {
        if (!.cdc_providers_skippable(e)) {
          stop(e)
        }
        failures[[length(failures) + 1]] <<- tibble::tibble(
          zipcode = zipcode[[i]],
          message = .cdc_providers_failure_message(e)
        )
        NULL
      }
    ))
    found <- found + nrow(results[[i]] %||% .empty_providers())
    if (progress) {
      cli::cli_progress_update(.envir = caller)
    }
  }
  if (progress) {
    cli::cli_progress_done(.envir = caller)
  }

  out <- .bind_providers(results)
  if (distinct) {
    out <- .distinct_nearest(out)
  }

  if (progress) {
    n_zip <- length(zipcode)
    n_found <- nrow(out)
    n_failed <- length(failures)
    cli::cli_inform(c(
      "v" = paste0(
        "Searched {n_zip} ZIP code{?s}: {n_found} provider{?s}",
        if (n_failed > 0) "; {n_failed} ZIP code{?s} failed" else "",
        "."
      )
    ))
  }

  if (length(failures) > 0) {
    failures <- dplyr::bind_rows(failures)
    attr(out, "failures") <- failures
    skipped <- failures$zipcode
    cli::cli_warn(
      c(
        paste(
          "Skipped {length(skipped)} ZIP code{?s} whose search failed:",
          "{.val {skipped}}."
        ),
        "i" = "See {.code attr(result, \"failures\")} for the API's messages."
      ),
      class = "cdclocator_skipped_zipcodes"
    )
  }

  out
}

# The minimum gap for this call: what the caller asked for, or else the
# package's gap (0.5 seconds) for one ZIP code and at least 1 second for
# several. The `cdclocator.sweep_gap` option exists so mocked tests can run
# without pacing.
.cdc_providers_min_gap <- function(min_gap, n_zipcodes) {
  if (!is.null(min_gap)) {
    return(min_gap)
  }
  if (n_zipcodes > 1) {
    max(.cdc_locator_min_gap(), getOption("cdclocator.sweep_gap", 1))
  } else {
    .cdc_locator_min_gap()
  }
}

# Whether one ZIP code's failure can be skipped in a search of several.
# Refusals, rate limits and outages would hit every remaining ZIP code.
.cdc_providers_skippable <- function(e) {
  status <- e$status %||% httr2::resp_status(e$resp)
  !status %in% c(401L, 403L, 429L, 503L)
}

# A failure's message on one line, without cli formatting or bullets, for
# the failures table.
.cdc_providers_failure_message <- function(e) {
  lines <- strsplit(cli::ansi_strip(conditionMessage(e)), "\n")[[1]]
  # Lines after the first are cli bullets: one symbol, then a space.
  lines <- trimws(lines)
  lines[-1] <- sub("^\\S ", "", lines[-1])
  paste(lines[nzchar(lines)], collapse = " ")
}

.empty_providers <- function() {
  tibble::tibble(
    type = character(),
    id = character(),
    search_zipcode = character(),
    attributes = list()
  )
}

# Binds provider tibbles. An empty result has a different `attributes` type
# from a populated one, so only populated ones are bound; if none are, the
# result is the empty schema.
.bind_providers <- function(results) {
  populated <- Filter(\(x) !is.null(x) && nrow(x) > 0, results)
  if (length(populated) == 0) {
    return(.empty_providers())
  }
  dplyr::bind_rows(unname(populated))
}

# Keeps each provider once, from the row where it was nearest, in the order
# the rows were found. Ties and missing distances keep the first row.
.distinct_nearest <- function(out) {
  if (nrow(out) == 0) {
    return(out)
  }
  distance <- if (is.data.frame(out$attributes)) out$attributes$distance
  distance <- suppressWarnings(as.numeric(distance %||% rep(NA, nrow(out))))
  nearest_first <- order(distance, seq_len(nrow(out)), na.last = TRUE)
  keep <- nearest_first[!duplicated(out$id[nearest_first])]
  out[sort(keep), ]
}

# All providers near one ZIP code: one request per batch of up to three
# services, with repeats across batches kept once.
.cdc_providers_zipcode <- function(
  zipcode,
  radius,
  services,
  client_id,
  client_token,
  call
) {
  batches <- split(
    services,
    ceiling(seq_along(services) / .cdc_locator_batch_size)
  )
  results <- lapply(batches, \(batch) {
    .cdc_providers_request(
      zipcode,
      radius,
      batch,
      client_id,
      client_token,
      call = call
    )
  })

  out <- .bind_providers(results)
  out <- out[!duplicated(out$id), ]
  out$search_zipcode <- rep(zipcode, nrow(out))
  dplyr::relocate(out, "search_zipcode", .after = "id")
}

# The most services sent in one request. All 14 at once returned no providers;
# up to three returned the union of their single-service results.
.cdc_locator_batch_size <- 3

.cdc_providers_request <- function(
  zipcode,
  radius,
  services,
  client_id,
  client_token,
  call
) {
  # The API requires radius and services (observed 2026-11-16).
  query <- list(
    zipcode = zipcode,
    radius = radius,
    services = paste(services, collapse = ",")
  )

  req <- .cdc_locator_request(
    endpoint = "providers",
    query = query,
    client_id = client_id,
    client_token = client_token
  )

  body <- .perform_cdc_locator_request(req, call = call)

  # No matching providers comes back as an empty JSON array, which parses to
  # an unnamed empty list. Providers parse to a data frame. Anything else
  # (null, an object) is not a provider list and must not look like "none".
  if (is.list(body) && length(body) == 0 && is.null(names(body))) {
    return(tibble::tibble(
      type = character(),
      id = character(),
      attributes = list()
    ))
  }

  if (!is.data.frame(body)) {
    cli::cli_abort(
      c(
        "The CDC Locator API returned an unexpected response.",
        "i" = "Expected a JSON array of providers."
      ),
      class = "cdclocator_unexpected_response",
      call = call
    )
  }

  # Providers are combined across batches by `id`, so every record needs one.
  ids <- body$id
  if (is.null(ids) || any(is.na(ids)) || !all(nzchar(as.character(ids)))) {
    cli::cli_abort(
      c(
        "The CDC Locator API returned an unexpected response.",
        "i" = "Every provider should have a non-empty {.field id}."
      ),
      class = "cdclocator_unexpected_response",
      call = call
    )
  }

  tibble::as_tibble(body)
}
