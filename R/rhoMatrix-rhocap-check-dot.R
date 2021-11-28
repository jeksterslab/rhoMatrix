#' Sanity Check for the Correlation Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Correlation matrix.
#' @param return_k Logical.
#'   Return the dimension of the covariance matrix.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation check
#' @noRd
.check_rhocap <- function(x,
                          return_k = FALSE) {
  stopifnot(
    is.matrix(x),
    x == t(x)
  )
  stopifnot(
    all(
      diag(x) == 1
    )
  )
  stopifnot(
    all(
      abs(
        .vec(x)
      ) <= 1
    )
  )
  if (return_k) {
    return(dim(x)[1])
  }
}
