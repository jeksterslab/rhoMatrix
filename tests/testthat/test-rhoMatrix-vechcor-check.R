## ---- test-rhoMatrix-vechscor-check
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
          .check_vechscor(
            .vechs(x),
            return_k = TRUE
          )
        )
      }
    )
    if (k == 1) {
      testthat::test_that(
        paste(text, "invalid length"),
        {
          testthat::expect_error(
            .check_vechscor(
              rep(x = 1, times = 2)
            )
          )
        }
      )
      testthat::test_that(
        paste(text, "invalid length"),
        {
          testthat::expect_error(
            .check_vechscor(
              rep(x = 1, times = 4)
            )
          )
        }
      )
    } else {
      testthat::test_that(
        paste(text, "greater than 1"),
        {
          testthat::expect_error(
            .check_vechscor(
              .vechs(x + 10)
            )
          )
        }
      )
    }
  },
  n = 1000,
  text = "test-rhoMatrix-vechscor-check",
  tol = 0.001
)
