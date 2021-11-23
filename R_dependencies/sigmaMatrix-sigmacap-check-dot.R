#' Sanity Check for the Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Covariance matrix.
#' @param return_k Logical.
#'   Return the dimension of the covariance matrix.
#' @family Covariance Functions
#' @keywords sigmaMatrix covariance check
#' @noRd
.check_sigmacap <- function(x,
                            return_k = FALSE) {
  .check_sym(x)
  stopifnot(
    all(
      diag(x) > 0
    )
  )
  if (return_k) {
    return(dim(x)[1])
  }
}
