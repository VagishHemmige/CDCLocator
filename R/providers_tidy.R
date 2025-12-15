#' Tidy CDC Locator provider output
#'
#' Flattens nested provider attributes into an analysis-ready tibble.
#'
#' @param providers Output from cdc_providers()
#'
#' @return A tidy tibble with one row per provider
#' @export
cdc_providers_tidy <- function(providers) {

  if (!inherits(providers, "tbl_df")) {
    cli::cli_abort("{.arg providers} must be the output of cdc_providers().")
  }

  providers |>
    tidyr::unnest_wider(attributes) |>
    tidyr::unnest_wider(address, names_sep = "_") |>
    tidyr::unnest_wider(contact, names_sep = "_") |>
    tidyr::unnest_wider(point, names_sep = "_") |>
    dplyr::rename(
      provider_type = type,
      provider_id   = id,
      name           = name,
      city           = address_city,
      state          = address_state,
      zipcode        = address_zipcode,
      street         = address_street,
      latitude       = point_latitude,
      longitude      = point_longitude
    )
}
