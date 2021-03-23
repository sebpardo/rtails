n <- 50

test_that("rgev_tails() works correctly", {

  x <- rgev_tails(n, scale = 0.3, shape = 0.3)

  expect_type(x, "double")
  expect_equal(length(x), n)
  expect_equal(mean(x), 1)

  x2 <- rgev_tails(n, scale = 0.3, shape = 0.3, ac = 1)
  ac_est <- acf(x2, lag.max = 1, plot = FALSE)
  expect_gt(min(ac_est$acf),  0)

  x3 <- rgev_tails(n, scale = 0.3, shape = 0.3, bias_correct = FALSE)
  expect_lt(mean(x), mean(x3))

  x4 <- rgev_tails(n, scale = 0.3, shape = 0.3, seed = 1984)
  x5 <- rgev_tails(n, scale = 0.3, shape = 0.3, seed = 1984)
  expect_identical(x4, x5)

})

test_that("rgev_tails() fails correctly", {

  expect_error(rgev_tails(n,  scale = 0.3, shape = 0.3, ac = 1.1),
               "Autocorrelation value")
  expect_error(rgev_tails(n, scale = -0.3, shape = 0.3),
               "Scale parameter must be greater than zero.")
  expect_error(rgev_tails(-10, scale = 0.3, shape = 0.3),
                 "'n' must be a positive integer")
  expect_error(rgev_tails(n, scale = 0.3, shape = 0.3, bias_correct = "foo"))
  expect_error(supressWarnings(rgev_tails(n, scale = 0.3, shape = 0.3, seed = "foo")))
  expect_error(rgev_tails(n, scale =3, shape = 5), "Mean cannot be estimated for bias correction as draws are too heavy-tailed")
})
