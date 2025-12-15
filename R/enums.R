# -------------------------------------------------------------------------
# Service enums for CDC Locator API
# -------------------------------------------------------------------------

#' Valid service enums for the CDC Locator API
#'
#' These values are passed to the `services` query parameter of the
#' `/providers` endpoint as a comma-separated list.
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
#' - Hepc             : Hepatitis C testing
#' - doxypep          : Doxycycline post-exposure prophylaxis
#' - mailintesting    : Mail-in HIV self-testing kits
#' - ryanwhite        : Ryan White HIV/AIDS Program services
#' - clinics          : HRSA-funded health clinics
#' - substanceabuse   : Substance use disorder treatment
#' - mentalhealth     : Mental health services
#' - housing          : Housing assistance
#'
#' @keywords internal
.cdc_locator_services <- c(
  "hivtesting",
  "stdtesting",
  "selftesting",
  "preptesting",
  "pep",
  "condom",
  "Hepc",
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
#' @param services Character vector of service enums.
#'
#' @return Invisibly returns TRUE if valid.
#' @keywords internal
.validate_cdc_locator_services <- function(services) {

  if (is.null(services)) {
    return(invisible(TRUE))
  }

  if (!is.character(services)) {
    cli::cli_abort("{.arg services} must be a character vector.")
  }

  bad <- setdiff(services, .cdc_locator_services)

  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Invalid CDC Locator service enum(s):",
      "x" = paste(bad, collapse = ", "),
      "i" = "Valid values are:",
      " " = paste(.cdc_locator_services, collapse = ", ")
    ))
  }

  invisible(TRUE)
}
