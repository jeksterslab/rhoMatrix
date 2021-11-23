#' Strict Half-Vectorize - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vechs description references return
#' @inheritParams vechs
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @noRd
.vechs <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = FALSE
    )
  ]
}
