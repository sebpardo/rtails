n <- 50

test_that("rst_tails() works correctly", {

  x <- rst_tails(n, df = 8)

  expect_type(x, "double")
  expect_equal(length(x), n)

  x2 <- rst_tails(n,  df = 8, ac = 1)
  ac_est <- acf(x2, lag.max = 1, plot = FALSE)
  expect_gt(min(ac_est$acf),  0)

  x3 <- rst_tails(n,  df = 8, bias_correct = FALSE)
  expect_lt(mean(x), mean(x3))

  expect_equal(mean(x), 1)

  x4 <- rst_tails(n,  df = 8, seed = 1984)
  x5 <- rst_tails(n,  df = 8, seed = 1984)
  expect_identical(x4, x5)

})

test_that("rst_tails() fails correctly", {

  expect_error(rst_tails(n,  df = 8, ac = 1.1),
               "Autocorrelation value")
  expect_error(rst_tails(n,  df = -8),
               "Degrees of freedom parameter must be greater than zero.")
  expect_error(rst_tails(-10, df = -8),
                 "'n' must be a positive integer.")
  expect_error(rst_tails(n, df = 8, bias_correct = "foo"))
  expect_error(supressWarnings(rst_tails(n, df = 8, seed = "foo")))
})
