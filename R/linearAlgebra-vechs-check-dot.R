#' Sanity Checks for the Strict Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams sym_of_vechs
#' @param diags Vector.
#'   Diagonal elements.
#' @inheritParams .check_vech
#' @family Vectorization Functions
#' @keywords linearAlgebra check
#' @noRd
.check_vechs <- function(x,
                         diags = NULL,
                         return_k = FALSE) {
  stopifnot(
    is.vector(x)
  )
  k <- 0.5 * (
    sqrt(
      1 + 8 * length(x)
    ) + 1
  )
  stopifnot(
    k %% 1 == 0
  )
  if (!is.null(diags)) {
    stopifnot(
      is.vector(diags)
    )
    diags_length <- length(diags)
    stopifnot(
      diags_length == 1 || diags_length == k
    )
  }
  if (return_k) {
    return(k)
  }
}
