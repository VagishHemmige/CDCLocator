# -------------------------------------------------------------------------
# Small internal utilities
# -------------------------------------------------------------------------

#' Null-coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise the right-hand side.
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
