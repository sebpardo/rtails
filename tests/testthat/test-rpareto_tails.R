test_that("rpareto_tails() works correctly", {

  n <- 50
  x <- rpareto_tails(n, shape = 2.1)

  expect_type(x, "double")
  expect_equal(length(x), n)

  x2 <- rpareto_tails(n, shape = 3, ac = 1)
  ac_est <- acf(x2, lag.max = 1, plot = FALSE)
  expect_gt(min(ac_est$acf),  0.7)

  x3 <- rpareto_tails(n, shape = 2.1, bias_correct = FALSE)
  expect_lt(mean(x), mean(x3))

  x4 <- rpareto_tails(n, shape = 2.1, seed = 1984)
  x5 <- rpareto_tails(n, shape = 2.1, seed = 1984)
  expect_identical(x4, x5)

  expect_warning(rpareto_tails(n, shape = 1.6), "Variance is infinite")

  # two warnings, therefore nested expect_warning() calls
  expect_warning(expect_warning(rpareto_tails(n, 1.01, seed = 1,
                               bias_correct = FALSE,
                               sample_bias_correct = TRUE),
                 "Variance is infinite"),
          "Negative values produced after sample bias correction")
})

test_that("rpareto_tails() fails correctly", {

  expect_error(rpareto_tails(n, shape = 3, ac = 1.1),
               "Autocorrelation value")
  expect_error(rpareto_tails(n, shape = -1),
               "Shape parameter must be greater than zero.")
  expect_error(rpareto_tails(-10, shape = 3),
                 "'n' must be a positive integer or vector.")
  expect_error(rpareto_tails(n, shape = 2.1, bias_correct = "foo"))
  expect_error(supressWarnings(rpareto_tails(n, shape = 2.1, seed = "foo")))
})
