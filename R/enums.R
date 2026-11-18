# -------------------------------------------------------------------------
# Service enums for CDC Locator API
# -------------------------------------------------------------------------

#' Valid service enums for the CDC Locator API
#'
#' These values are passed to the `services` query parameter of the
#' `/providers` endpoint as a comma-separated list. The API's 2025 instructions
#' list Hepatitis C as `Hepc`, but on 2026-11-16 the API matched only `hepc`,
#' so all values are lowercase and user input is matched in any case.
#'
#' @details
#' The following enums are supported:
#'
#' - hivtesting       : HIV Testing
#' - stdtesting       : STD Testing
#' - selftesting      : Self-administered HIV & STD testing
#' - preptesting     : Pre-Exposure Prophylaxis (PrEP)
#' - pep              : Post-Exposure Prophylaxis (PEP)
#' - condom           : Condom distribution
#' - hepc             : Hepatitis C testing
#' - doxypep          : Doxycycline post-exposure prophylaxis
#' - mailintesting    : Mail-in HIV self-testing kits
#' - ryanwhite        : Ryan White HIV/AIDS Program services
#' - clinics          : HRSA-funded health clinics
#' - substanceabuse   : Substance use disorder treatment
#' - mentalhealth     : Mental health services
#' - housing          : Housing assistance
#'
#' @noRd
.cdc_locator_services <- c(
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

#' Validate CDC Locator service enums
#'
#' @param services Character vector of service enums, matched in any case, or
#'   `NULL` for every service.
#'
#' @return The services as the API expects them: lowercase, or every service
#'   when `services` is `NULL`.
#' @param call The frame named in errors: the user-facing function.
#' @noRd
.validate_cdc_locator_services <- function(services, call = parent.frame()) {
  if (is.null(services)) {
    return(.cdc_locator_services)
  }

  if (!is.character(services)) {
    cli::cli_abort("{.arg services} must be a character vector.", call = call)
  }

  if (length(services) == 0) {
    cli::cli_abort(
      c(
        "{.arg services} must name at least one service.",
        "i" = "Use {.code services = NULL} to search every service."
      ),
      call = call
    )
  }

  services <- tolower(services)
  bad <- setdiff(services, .cdc_locator_services)

  if (length(bad) > 0) {
    cli::cli_abort(
      c(
        "Invalid CDC Locator service enum(s):",
        "x" = paste(bad, collapse = ", "),
        "i" = "Valid values are:",
        " " = paste(.cdc_locator_services, collapse = ", ")
      ),
      call = call
    )
  }

  services
}
