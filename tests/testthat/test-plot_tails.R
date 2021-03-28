n <- 50
let <- letters[1:4]

test_that("plot_tails() works correctly", {

  x <- rst_tails(n, df = 8)
  p <- plot_tails(x)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$y, "y")
  expect_length(p$data$y, n)

  x2 <- rnorm_tails(n, high_sigma = 2, rate = 1/5)
  p2 <- plot_tails(x2)
  expect_s3_class(p2, "ggplot")
  expect_identical(p2$labels$y, "y")
  expect_equal(which(p2$data$mix == TRUE), sort(attr(x2, "ht")))

  x3 <- rpareto_tails(n, shape = 2.1, seed = 1984)
  x4 <- rpareto_tails(n, shape = 2.1, seed = 1984)

  p3 <- plot_tails(x3)
  p4 <- plot_tails(x4)

  expect_equal(p3$labels$y, p4$labels$y)

})

test_that("plot_tails() fails correctly", {

  expect_error(plot_tails(let), "'vec' must be numeric.")
  expect_error(plot_tails(FALSE), "'vec' must be numeric.")
  expect_error(plot_tails(NA), "'vec' must be numeric.")
  expect_error(plot_tails(rep(Inf, 3)), "Some values are infinite.")
  expect_error(plot_tails(c(1.1, 0.9, Inf)), "Some values are infinite.")

})
