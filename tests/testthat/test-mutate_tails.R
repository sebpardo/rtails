n <- 50
sd1 <- 0.1

test_that("mutate_tails() works correctly", {

  x <- exp(rnorm(n, -0.5 * sd1^2, sd1))

  x2 <- mutate_tails(x, dist = "student-t", args = list(df = 5))

  expect_type(x2, "double")
  expect_equal(length(x2), n)

  x3 <- suppressWarnings(mutate_tails(x, dist = "pareto",
                                      args = list(shape = 1.01)))

  expect_type(x3, "double")
  expect_equal(length(x3), n)

  x4 <- mutate_tails(x, dist = "student-t", args = list(df = 5),
                     bootstrap = TRUE, nout = 100)

  expect_length(x4, 100L)

  expect_warning(mutate_tails(x, dist = "student-t", args = list(df = 5),
                              bootstrap = FALSE, nout = 100),
                 "'nout' is different to")

})

test_that("mutate_tails() fails correctly", {

  expect_error(mutate_tails(x, dist = "student-t", args = list(foo = 5)))
  expect_error(mutate_tails(x, dist = "pareto", args = list(foo = 5)))

  expect_error(mutate_tails(FALSE, dist = "student-t", args = list(df = 2)),
               "Vector 'x' has logical values.")
  expect_error(mutate_tails(rnorm(n)), "Vector 'x' has negative values.")

})

