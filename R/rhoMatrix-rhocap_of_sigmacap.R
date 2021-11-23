#' Correlation Matrix from Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation
#' @export
rhocap_of_sigmacap <- function(x) {
  if (is.vector(x) & length(x) == 1) {
    return(1)
  }
  .check_sigmacap(x)
  .rhocap_of_sigmacap(
    x = x,
    q = sqrt(1 / diag(x))
  )
}
