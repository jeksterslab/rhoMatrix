## ---- test-rhoMatrix-cov_of_cor
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    data(iris)
    sigmacap <- unname(stats::cov(iris[, 1:4]))
    rhocap <- unname(stats::cor(iris[, 1:4]))
    testthat::test_that(
      paste(text, "1"),
      {
        testthat::expect_equal(
          sigmacap[1, 1],
          cov_of_cor(
            rhocap[1, 1],
            sqrt(
              sigmacap[1, 1]
            )
          )
        )
      }
    )
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          sigmacap,
          cov_of_cor(
            rhocap,
            sd = sqrt(
              diag(
                sigmacap
              )
            )
          )
        )
      }
    )
  },
  text = "test-rhoMatrix-cov_of_cor"
)
