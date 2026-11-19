#' Tidy CDC Locator provider output
#'
#' Flattens nested provider attributes into an analysis-ready tibble.
#'
#' @param providers Output from cdc_providers()
#'
#' @return A tibble with one row for each row of `providers` (one per
#'   provider record, or with `cdc_providers(distinct = FALSE)` one per
#'   provider record and ZIP code that found it) and columns `provider_type`,
#'   `provider_id`, `search_zipcode` (the ZIP code whose search found the
#'   provider), `name`, `city`, `country`, `state`, `street`, `zipcode`,
#'   `categories`, `contact_others`, `contact_telephone`, `contact_website`,
#'   `languages` (a list of character vectors), `latitude`, `longitude` and
#'   `distance` (miles from the searched ZIP code). When no providers matched,
#'   the tibble has these columns and zero rows.
#' @export
cdc_providers_tidy <- function(providers) {
  if (!inherits(providers, "tbl_df")) {
    cli::cli_abort("{.arg providers} must be the output of cdc_providers().")
  }

  if (nrow(providers) == 0) {
    return(.empty_providers_tidy())
  }

  out <- providers |>
    tidyr::unnest_wider("attributes") |>
    tidyr::unnest_wider("address", names_sep = "_") |>
    tidyr::unnest_wider("contact", names_sep = "_") |>
    tidyr::unnest_wider("point", names_sep = "_") |>
    dplyr::rename(
      provider_type = "type",
      provider_id = "id",
      city = "address_city",
      country = "address_country",
      state = "address_state",
      zipcode = "address_zipcode",
      street = "address_street",
      latitude = "point_latitude",
      longitude = "point_longitude"
    )

  # The API returns each provider's languages as a JSON array.
  out$languages <- lapply(out$languages, \(x) as.character(unlist(x)))
  out
}

.empty_providers_tidy <- function() {
  tibble::tibble(
    provider_type = character(),
    provider_id = character(),
    search_zipcode = character(),
    name = character(),
    city = character(),
    country = character(),
    state = character(),
    street = character(),
    zipcode = character(),
    categories = logical(),
    contact_others = logical(),
    contact_telephone = character(),
    contact_website = character(),
    languages = list(),
    latitude = double(),
    longitude = double(),
    distance = double()
  )
}
