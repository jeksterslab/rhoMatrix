## ---- test-rhoMatrix-cor-check
lapply(
  X = seq_len(3),
  FUN = function(k,
                 n,
                 text,
                 tol) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- stats::cor(
      matrix(
        data = stats::runif(
          n = n * k
        ),
        ncol = k
      )
    )
    testthat::test_that(
      paste(text),
      {
        testthat::expect_equal(
          k,
          .check_cor(
            x,
            return_k = TRUE
          )
        )
      }
    )
    x[k, k] <- x[k, k] * -1
    testthat::test_that(
      paste(text, "negative variance"),
      {
        testthat::expect_error(
          .check_cor(x)
        )
      }
    )
    if (k == 1) {
      testthat::test_that(
        paste(text, "asymmetric"),
        {
          testthat::expect_error(
            .check_cor(
              matrix(
                data = 1,
                nrow = 2,
                ncol = 3
              )
            )
          )
        }
      )
    }
  },
  n = 1000,
  text = "test-rhoMatrix-cor-check",
  tol = 0.001
)
