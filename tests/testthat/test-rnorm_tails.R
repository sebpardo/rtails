n <- 5000
test_that("rnorm_tails() works correctly", {

  x <- rnorm_tails(n, sigma = 0.1)

  expect_type(x, "double")
  expect_length(x, n)

  x2 <- rnorm_tails(n, sigma = 0.1, ac = 1)
  ac_est <- acf(x2, lag.max = 1, plot = FALSE)
  expect_gt(min(ac_est$acf),  0.7)

  x3 <- rnorm_tails(n, sigma = 0.1, bias_correct = FALSE)
  expect_lt(mean(x), mean(x3))

  n2 <- 80
  x4 <- rnorm_tails(n2, sigma = 0.1, seed = 1984)
  x5 <- rnorm_tails(n2, sigma = 0.1, seed = 1984)
  expect_identical(x4, x5)

  x6 <- rnorm_tails(n2, sigma = 0.1, bias_correct = FALSE,
                   sample_bias_correct = TRUE, seed = 1984)
  expect_equal(mean(x6), 1)

  x7 <- rnorm_tails(n2, sigma = 0.1, bias_correct = FALSE,
                    sample_bias_correct = FALSE, log = TRUE, seed = 1984)
  expect_lt(mean(x7), mean(x6))

  x8 <- rnorm(n2, -0.5 * 0.1^2, 0.1)
  x9 <- rnorm_tails(x8, rate = 1/2, replace = TRUE)

  expect_true(mean(x9) != 1)

})

test_that("rnorm_tails() fails correctly", {

  expect_error(rnorm_tails(n, sigma = 0.1, ac = 1.1),
               "Autocorrelation value")
  expect_error(rnorm_tails(n, sigma = -0.1),
               "'sigma' and 'high_sigma' must be greater than zero.")
  expect_error(rnorm_tails(n, sigma = 0.1, high_sigma = -1),
               "'sigma' and 'high_sigma' must be greater than zero.")
  expect_error(rnorm_tails(-10, sigma = 3, high_sigma = 4),
                 "'n' must be a positive integer.")
  expect_error(rnorm_tails(10, sigma = 3, high_sigma = 2),
               "'sigma' is equal or greater than 'high_sigma'")
  expect_error(rnorm_tails(n, sigma = 0.4, bias_correct = "foo"))
  expect_error(supressWarnings(rnorm_tails(n, sigma = 0.4, seed = "foo")))

  })
