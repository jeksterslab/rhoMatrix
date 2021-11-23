#' Correlation Matrix from Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix.
#' @param q Numeric vector.
#'   Inverse of the standard deviation vector.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation dot
#' @export
.rhocap_of_sigmacap <- function(x,
                                q) {
  q * x * rep(
    x = q,
    each = dim(x)[1]
  )
}
