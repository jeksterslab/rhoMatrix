#' Sanity Checks Symmetric Matrices
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#' @family Symmetric Functions
#' @keywords linearAlgebra check
#' @noRd
.check_sym <- function(x) {
  stopifnot(
    is.matrix(x),
    x == t(x)
  )
}
