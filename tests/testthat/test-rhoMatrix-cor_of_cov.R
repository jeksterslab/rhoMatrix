## ---- test-rhoMatrix-cor_of_cov
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
          rhocap[1, 1],
          cor_of_cov(
            sigmacap[1, 1]
          )
        )
      }
    )
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          rhocap,
          cor_of_cov(
            sigmacap
          )
        )
      }
    )
  },
  text = "test-rhoMatrix-cor_of_cov"
)
