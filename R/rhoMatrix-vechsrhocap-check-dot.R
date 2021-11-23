#' Sanity Check for the Strict Half-Vectorization
#' of the Correlation Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Strict half-vectorization of the covariance matrix.
#' @param return_k Logical.
#'   Return the dimension of the covariance matrix.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation check
#' @export
.check_vechsrhocap <- function(x,
                               return_k = FALSE) {
  k <- .check_vechs(
    x = x,
    diags = NULL,
    return_k = TRUE
  )
  stopifnot(
    all(
      abs(x) <= 1
    )
  )
  if (return_k) {
    return(k)
  }
}
