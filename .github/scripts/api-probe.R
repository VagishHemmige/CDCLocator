# Probes the HIV.gov Locator API and reports what each request returns: status,
# content type, the shape of a JSON body (names and types, not values) and the
# text of any error body. Used to learn the current API contract.
#
# Run by api-probe.yaml with the repository secrets. It never prints request
# headers or credentials. Requests are spaced one second apart.

library(httr2)

id <- Sys.getenv("CDC_LOCATOR_CLIENT_ID")
token <- Sys.getenv("CDC_LOCATOR_CLIENT_TOKEN")
if (!nzchar(id) || !nzchar(token)) {
  stop("CDC_LOCATOR_CLIENT_ID and CDC_LOCATOR_CLIENT_TOKEN must be set.")
}

base_url <- "https://locator-api.hiv.gov"

probe_request <- function(path, query = list(), auth = "real") {
  req <- request(base_url) |>
    req_url_path_append(path) |>
    req_url_query(!!!query) |>
    req_user_agent(
      "CDCLocator API probe (https://github.com/VagishHemmige/CDCLocator)"
    ) |>
    req_timeout(30) |>
    req_error(is_error = \(resp) FALSE)
  if (auth == "real") {
    req <- req_headers(
      req,
      client_id = id,
      client_token = token,
      .redact = c("client_id", "client_token")
    )
  } else if (auth == "fake") {
    req <- req_headers(
      req,
      client_id = "fake-client-id",
      client_token = "fake-client-token"
    )
  }
  req
}

# Describes the structure of parsed JSON without its values.
shape <- function(x, depth = 0, max_depth = 8) {
  pad <- strrep("  ", depth)
  if (is.null(x)) {
    return(paste0(pad, "null"))
  }
  if (!is.list(x)) {
    return(paste0(pad, class(x)[1], " [", length(x), "]"))
  }
  if (is.null(names(x))) {
    head_line <- paste0(pad, "array [", length(x), "]")
    if (length(x) == 0 || depth >= max_depth) {
      return(head_line)
    }
    return(c(
      head_line,
      paste0(pad, "  first element:"),
      shape(x[[1]], depth + 2, max_depth)
    ))
  }
  out <- paste0(pad, "object {", paste(names(x), collapse = ", "), "}")
  if (depth >= max_depth) {
    return(out)
  }
  for (nm in names(x)) {
    out <- c(
      out,
      paste0(pad, "  $", nm, ":"),
      shape(x[[nm]], depth + 2, max_depth)
    )
  }
  out
}

describe <- function(label, path, query = list(), auth = "real") {
  Sys.sleep(1)
  shown_query <- if (length(query)) {
    paste(names(query), unlist(query), sep = "=", collapse = "&")
  } else {
    "(none)"
  }
  lines <- c(
    paste0("### ", label),
    "",
    paste0("`GET ", path, "?", shown_query, "`, auth: ", auth),
    ""
  )
  resp <- tryCatch(req_perform(probe_request(path, query, auth)), error = \(e) {
    e
  })
  if (inherits(resp, "error")) {
    return(c(lines, paste0("Request failed: ", conditionMessage(resp)), ""))
  }
  type <- resp_header(resp, "content-type") %||% "(none)"
  lines <- c(
    lines,
    paste0("Status ", resp_status(resp), ", content type `", type, "`"),
    ""
  )
  body <- tryCatch(resp_body_string(resp), error = \(e) "")
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = \(e) NULL
  )
  if (resp_status(resp) >= 400 || is.null(parsed)) {
    text <- substr(gsub("[\r\n]+", " ", body), 1, 500)
    lines <- c(
      lines,
      "Body (first 500 characters):",
      "",
      "```",
      text,
      "```",
      ""
    )
  } else {
    lines <- c(lines, "Shape:", "", "```", shape(parsed), "```", "")
  }
  lines
}

`%||%` <- function(x, y) if (is.null(x)) y else x

report <- c(
  paste0(
    "## Locator API probe, ",
    format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  "",
  describe(
    "Place translator (documented example shape)",
    "address/place",
    list(city = "Peoria", state = "IL")
  ),
  describe(
    "Point translator",
    "address/point",
    list(latitude = 40.69365, longitude = -89.58899)
  ),
  describe(
    "Providers, documented example",
    "providers",
    list(zipcode = "20176", radius = 10)
  ),
  describe("Providers, no radius", "providers", list(zipcode = "20176")),
  describe(
    "Providers, documented services example",
    "providers",
    list(
      zipcode = "20176",
      services = "hivtesting,ryanwhite,clinics",
      radius = 10
    )
  ),
  describe(
    "Providers, live test request",
    "providers",
    list(zipcode = "10001", radius = 1)
  ),
  describe(
    "Providers, 10001 radius 10",
    "providers",
    list(zipcode = "10001", radius = 10)
  ),
  describe(
    "Providers, services Hepc",
    "providers",
    list(zipcode = "20176", services = "Hepc", radius = 10)
  ),
  describe(
    "Providers, services hepc",
    "providers",
    list(zipcode = "20176", services = "hepc", radius = 10)
  ),
  describe(
    "Providers, radius 51",
    "providers",
    list(zipcode = "20176", radius = 51)
  ),
  describe(
    "Providers, unknown ZIP",
    "providers",
    list(zipcode = "00000", radius = 10)
  ),
  describe(
    "Providers, fake credentials",
    "providers",
    list(zipcode = "20176", radius = 10),
    auth = "fake"
  ),
  describe(
    "Place translator, no credentials",
    "address/place",
    list(city = "Peoria", state = "IL"),
    auth = "none"
  )
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(report, sep = "\n", file = summary_file, append = TRUE)
}
cat(report, sep = "\n")
