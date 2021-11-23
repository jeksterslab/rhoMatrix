#' Covariance Matrix from Correlation Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Correlation matrix.
#' @param sd Numeric vector.
#'   Vector of standard deviations.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation dot
#' @export
.sigmacap_of_rhocap <- function(x,
                                sd) {
  sd * x * rep(
    x = sd,
    each = dim(x)[1]
  )
}
