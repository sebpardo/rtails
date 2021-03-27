#' Mutating existing normal deviation to heavy-tailed ones
#'
#' Random generation for a mixture of normal distributions, both centered
#' around zero and bias corrected, as well as exponentiated.
#'
#' @param x vector of random deviates, must be positive and centered around one.
#' @param dist what type of distribution to convert to. Options are `student-t`
#'   and `pareto`.
#' @param args list of arguments to pass to the quantile function.
#'   If `dist = student-t`, then a `df` argument is required, while if
#'   `dist = pareto` then a shape argument is required.
#' @param nout length of output vector. Defaults to length(x). This is only
#'   used when `bootstrap = TRUE`.
#' @param bootstrap logical, whether to bootstrap samples? If `TRUE`, then
#'   number of samples is determined by `nout`. Bootstraps are with replacement.
#'
#' @details This function takes a series of normally-distributed deviations
#' and "stretches" them into a heavy-tailed distribution. It uses `pnorm()` to
#' transform to a uniform distribution and then a quantile function to
#' transform to a distribution of type `dist`.
#'
#' @importFrom EnvStats qpareto
#' @importFrom stats qt pnorm
#'
#' @examples
#' recdevs <- exp(rnorm(60, -0.5 * 0.1^2, 0.1)) # bias-corrected
#' recdevs_st <- mutate_tails(recdevs, dist = "student-t",
#'                                   args = list(df = 0.5))
#' plot_tails(recdevs)
#' plot_tails(recdevs_st)
#'
#' recdevs_p <- mutate_tails(recdevs, dist = "pareto",
#'                                  args = list(shape = 1.005))
#' plot_tails(recdevs_p)
#'
#' @export
mutate_tails <- function(x, dist = c("student-t", "pareto"), nout = length(x),
                         bootstrap = FALSE,  args = list()) {
  if (any(x < 0)) stop("Vector 'x' has negative values.", call. = FALSE)
  dist <- match.arg(dist)
  lx <- length(x)
  if (nout != lx && !bootstrap) {
    warning(paste0("'nout' is different to length(x) yet bootstrap = FALSE, ",
                   "ignoring 'nout'."))
  }
  px <- pnorm(x) # or pnorm(log(x)) or log(pnorm(x))?

  if (dist == "student-t") qt <- qt(px, df = args$df)
  if (dist == "pareto") {
    parpars <- generate_pareto_par(shape = args$shape)
    qt <- qpareto(px, location = parpars$location, shape = args$shape)
  }
  # potential centering options:
  x_new <- qt/mean(qt)
  # x_new <- qt - mean(qt) + 1
  #x_new <- scale(qt, scale = FALSE) + 1

  if (any(x_new < 0)) warning("Negative values produced after centering.")

  if (bootstrap) {
    x_boot <- sample(x_new, size = nout, replace = TRUE)
    return(x_boot)
  }
  x_new
}
