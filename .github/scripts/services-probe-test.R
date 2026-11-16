# Offline check of services-probe.R with mocked responses: run from the
# repository root with `Rscript --vanilla .github/scripts/services-probe-test.R`.
# Fails if the probe prints a credential, miscounts a shape or misreads
# "any of" matching. Makes no network requests.

library(httr2)

Sys.setenv(
  CDC_LOCATOR_CLIENT_ID = "SENTINELID123",
  CDC_LOCATOR_CLIENT_TOKEN = "SENTINELTOKEN456"
)

json <- function(text, status = 200L) {
  response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(text)
  )
}

# A made-up world where each provider offers some services and a request
# returns providers offering ANY of the requested services.
offers <- list(
  p1 = c("hivtesting", "stdtesting"),
  p2 = c("hivtesting", "ryanwhite"),
  p3 = c("clinics"),
  p4 = c("housing"),
  SENTINELID123 = "clinics"
)

local_mocked_responses(function(req) {
  svc <- strsplit(url_parse(req$url)$query$services, ",")[[1]]
  if (identical(svc, "pep")) {
    return(json("null"))
  }
  if (identical(svc, "condom")) {
    return(json('{"message": "SENTINELTOKEN456"}'))
  }
  if (identical(svc, "doxypep")) {
    return(json("not json"))
  }
  if (identical(svc, "mailintesting")) {
    return(json(paste("echo", req$headers$client_token), status = 500L))
  }
  ids <- names(offers)[vapply(offers, \(o) any(svc %in% o), logical(1))]
  if (length(ids) == 0) {
    return(json("[]"))
  }
  json(paste0(
    "[",
    paste0('{"id":"', ids, '","type":"provider"}', collapse = ","),
    "]"
  ))
})

Sys.sleep <- function(...) invisible(NULL)
out <- capture.output(source(".github/scripts/services-probe.R", local = TRUE))

stopifnot(
  "a credential reached the output" = !any(grepl("SENTINEL", out)),
  "hivtesting count" = any(grepl(
    "^\\| hivtesting \\| 200 \\| 2 \\| FALSE \\|$",
    out
  )),
  "empty array counts as 0" = any(grepl(
    "^\\| selftesting \\| 200 \\| 0 \\|  \\|$",
    out
  )),
  "null is not a count" = any(grepl(
    "^\\| pep \\| 200 \\| unexpected shape \\|  \\|$",
    out
  )),
  "object is not a count" = any(grepl(
    "^\\| condom \\| 200 \\| unexpected shape \\|  \\|$",
    out
  )),
  "malformed JSON labelled" = any(grepl(
    "^\\| doxypep \\| 200 \\| malformed JSON \\|  \\|$",
    out
  )),
  "error status without a count" = any(grepl(
    "^\\| mailintesting \\| 500 \\|  \\|  \\|$",
    out
  )),
  "any-of matching reads as union" = any(grepl(
    "^\\| first 4 \\| 200 \\| 2 \\| 2 \\| 0 \\| TRUE \\| FALSE \\|$",
    out
  )),
  "unusable singles leave the comparison blank" = any(grepl(
    "^\\| all 14 \\| 200 \\| [0-9]+ \\| \\| \\| \\| \\|$",
    out
  )),
  "every list-length and leave-one-out row present" = sum(grepl(
    "^\\| (first [0-9]+|all but [a-z]+) \\|",
    out
  )) ==
    24
)
cat(
  "services-probe offline check passed:",
  length(out),
  "lines, no credential in output\n"
)
