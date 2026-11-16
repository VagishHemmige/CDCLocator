# One-off check of how the Locator API combines `services`. For each request it
# prints only the HTTP status and the number of providers returned: no response
# text, headers or field values, so nothing the API echoes can reach the log.
# Requests are spaced one second apart.

library(httr2)

id <- Sys.getenv("CDC_LOCATOR_CLIENT_ID")
token <- Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN")
if (!nzchar(id) || !nzchar(token)) {
  stop("CDC_LOCATOR_CLIENT_ID and CDC_LOCATOR_CLIENT_TOKEN must be set.")
}

services <- c(
  "hivtesting",
  "stdtesting",
  "selftesting",
  "preptesting",
  "pep",
  "condom",
  "hepc",
  "doxypep",
  "mailintesting",
  "ryanwhite",
  "clinics",
  "substanceabuse",
  "mentalhealth",
  "housing"
)

# Returns the provider IDs from one request, or a fixed label explaining why
# there are none. IDs stay in memory; only counts and comparisons are printed.
fetch_ids <- function(zipcode, radius, svc) {
  Sys.sleep(1)
  resp <- tryCatch(
    request("https://locator-api.hiv.gov") |>
      req_url_path_append("providers") |>
      req_url_query(
        zipcode = zipcode,
        radius = radius,
        services = paste(svc, collapse = ",")
      ) |>
      req_headers(
        client_id = id,
        client_token = token,
        .redact = c("client_id", "client_token")
      ) |>
      req_timeout(30) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform(),
    error = \(e) NULL
  )
  if (is.null(resp)) {
    return(list(status = "request failed", ids = NULL))
  }
  status <- as.character(resp_status(resp))
  if (status != "200") {
    return(list(status = status, ids = NULL))
  }
  body <- tryCatch(resp_body_json(resp), error = \(e) NULL)
  if (is.null(body) && !identical(trimws(resp_body_string(resp)), "null")) {
    return(list(status = status, ids = "malformed JSON"))
  }
  list(status = status, ids = provider_ids(body))
}

# A provider array is an unnamed list of objects that each have an `id`.
# Anything else gets a fixed label instead of a count.
provider_ids <- function(body) {
  if (!is.list(body) || !is.null(names(body))) {
    return("unexpected shape")
  }
  ok <- vapply(
    body,
    \(x) is.list(x) && !is.null(names(x)) && length(x$id) == 1,
    logical(1)
  )
  if (!all(ok)) {
    return("unexpected shape")
  }
  vapply(body, \(x) as.character(x$id), character(1))
}

is_ids <- function(x) {
  is.character(x) &&
    !identical(x, "unexpected shape") &&
    !identical(x, "malformed JSON")
}

describe_count <- function(r) {
  if (is.null(r$ids)) {
    ""
  } else if (is_ids(r$ids) || length(r$ids) == 0) {
    as.character(length(unique(r$ids)))
  } else {
    r$ids
  }
}

near <- list(zip = "20176", radius = 10)
singles <- lapply(services, \(s) fetch_ids(near$zip, near$radius, s))
names(singles) <- services

single_rows <- vapply(
  services,
  \(s) {
    r <- singles[[s]]
    sprintf("| %s | %s | %s |", s, r$status, describe_count(r))
  },
  character(1)
)

combos <- list(
  c("hivtesting", "ryanwhite", "clinics"),
  c("hivtesting", "stdtesting"),
  c("hivtesting", "housing"),
  services
)

combo_row <- function(svc) {
  r <- fetch_ids(near$zip, near$radius, svc)
  parts <- singles[svc]
  label <- if (length(svc) == length(services)) {
    "all 14"
  } else {
    paste(svc, collapse = ",")
  }
  usable <- all(vapply(
    parts,
    \(p) is_ids(p$ids) || length(p$ids) == 0 && !is.null(p$ids),
    logical(1)
  ))
  if (!(is_ids(r$ids) || (length(r$ids) == 0 && !is.null(r$ids))) || !usable) {
    return(sprintf(
      "| %s | %s | %s | | | | |",
      label,
      r$status,
      describe_count(r)
    ))
  }
  ids <- unique(r$ids)
  union_ids <- unique(unlist(lapply(parts, `[[`, "ids")))
  inter_ids <- Reduce(intersect, lapply(parts, \(p) unique(p$ids)))
  sprintf(
    "| %s | %s | %d | %d | %d | %s | %s |",
    label,
    r$status,
    length(ids),
    length(union_ids),
    length(inter_ids),
    setequal(ids, union_ids),
    setequal(ids, inter_ids)
  )
}

combo_rows <- vapply(combos, combo_row, character(1))

manhattan <- lapply(
  list("hivtesting", services),
  \(svc) {
    r <- fetch_ids("10001", 1, svc)
    label <- if (length(svc) > 1) "all 14" else svc
    sprintf("| %s | %s | %s |", label, r$status, describe_count(r))
  }
)

report <- c(
  paste0("## Services probe, ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "### One service at a time, ZIP 20176, radius 10",
  "",
  "| Service | Status | Providers |",
  "|---|---|---|",
  single_rows,
  "",
  "### Combinations, ZIP 20176, radius 10",
  "",
  "Union and intersection are of the provider IDs returned for each service alone. TRUE in a \"same as\" column means the combined request returned exactly that set.",
  "",
  "| Services | Status | Providers | Union of singles | Intersection of singles | Same as union | Same as intersection |",
  "|---|---|---|---|---|---|---|",
  combo_rows,
  "",
  "### ZIP 10001, radius 1",
  "",
  "| Services | Status | Providers |",
  "|---|---|---|",
  unlist(manhattan),
  ""
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(report, sep = "\n", file = summary_file, append = TRUE)
}
cat(report, sep = "\n")
