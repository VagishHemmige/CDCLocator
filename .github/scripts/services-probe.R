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

count_providers <- function(zipcode, radius, svc) {
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
    return(c(status = "request failed", providers = ""))
  }
  n <- if (resp_status(resp) == 200) {
    body <- tryCatch(resp_body_json(resp), error = \(e) NULL)
    if (is.list(body)) as.character(length(body)) else "not a JSON array"
  } else {
    ""
  }
  c(status = as.character(resp_status(resp)), providers = n)
}

cases <- c(
  lapply(services, \(s) list(label = s, zip = "20176", radius = 10, svc = s)),
  list(
    list(
      label = "hivtesting,ryanwhite,clinics (probe example)",
      zip = "20176",
      radius = 10,
      svc = c("hivtesting", "ryanwhite", "clinics")
    ),
    list(
      label = "hivtesting,stdtesting",
      zip = "20176",
      radius = 10,
      svc = c("hivtesting", "stdtesting")
    ),
    list(
      label = "hivtesting,housing",
      zip = "20176",
      radius = 10,
      svc = c("hivtesting", "housing")
    ),
    list(label = "all 14", zip = "20176", radius = 10, svc = services),
    list(
      label = "hivtesting, ZIP 10001, radius 1",
      zip = "10001",
      radius = 1,
      svc = "hivtesting"
    ),
    list(
      label = "all 14, ZIP 10001, radius 1",
      zip = "10001",
      radius = 1,
      svc = services
    )
  )
)

rows <- vapply(
  cases,
  \(x) {
    r <- count_providers(x$zip, x$radius, x$svc)
    sprintf(
      "| %s | %s | %s | %s | %s |",
      x$label,
      x$zip,
      x$radius,
      r[["status"]],
      r[["providers"]]
    )
  },
  character(1)
)

report <- c(
  paste0("## Services probe, ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "| Services | ZIP | Radius | Status | Providers |",
  "|---|---|---|---|---|",
  rows
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(report, sep = "\n", file = summary_file, append = TRUE)
}
cat(report, sep = "\n")
