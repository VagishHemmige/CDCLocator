# -------------------------------------------------------------------------
# Small internal utilities
# -------------------------------------------------------------------------

#' Null-coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise the right-hand side.
#'
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# ---- argument checks ------------------------------------------------------
# Each check errors before any request is sent. `arg` is the argument's name
# and `call` the user-facing function, both used in the message.

# One string that isn't missing or blank.
.check_string <- function(x, arg, call) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be a single non-empty string.",
      call = call
    )
  }
  invisible(x)
}

# A five-digit ZIP code, as a string so leading zeros are kept.
.check_zipcode <- function(x, arg, call) {
  if (is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a string, such as {.val 02110}.",
        "i" = "Numbers drop leading zeros: 02110 becomes 2110."
      ),
      call = call
    )
  }
  .check_string(x, arg, call)
  if (!grepl("^[0-9]{5}$", x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a five-digit ZIP code, such as {.val 02110}.",
        "x" = "You supplied {.val {x}}."
      ),
      call = call
    )
  }
  invisible(x)
}

# One finite number between `min` and `max`, whole if `whole` is TRUE.
.check_number <- function(x, arg, min, max, whole = FALSE, unit = "", call) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number, not missing or infinite.",
      call = call
    )
  }
  if (whole && x != round(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a whole number.",
      call = call
    )
  }
  if (x < min || x > max) {
    cli::cli_abort(
      "{.arg {arg}} must be between {min} and {max}{unit}.",
      call = call
    )
  }
  invisible(x)
}
