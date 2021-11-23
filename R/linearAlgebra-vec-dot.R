#' Vectorize - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vec description details references return
#' @inheritParams vec
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @noRd
.vec <- function(x) {
  dim(x) <- NULL
  x
}
