#' Covariance Matrix from Correlation Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Correlation matrix.
#' @param sd Numeric vector.
#'   Vector of standard deviations.
#' @family Correlation Functions
#' @keywords rhoMatrix correlation
#' @export
sigmacap_of_rhocap <- function(x,
                               sd) {
  if (is.vector(x) & length(x) == 1) {
    stopifnot(
      is.vector(sd),
      length(sd) == 1
    )
    return(
      sd^2 * x
    )
  }
  .check_cor(x)
  stopifnot(
    dim(x)[1] == length(sd)
  )
  .sigmacap_of_rhocap(
    x = x,
    sd = sd
  )
}
