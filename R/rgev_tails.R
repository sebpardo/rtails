#' Random generation of generalized extreme value deviations
#'
#' Random generation for the generalized extreme value (GEV) distribution,
#' centered around a mean of one, with parameter `shape`.
#'
#' @param n sample size.
#' @param loc location parameter.
#' @param scale scale parameter. Must be > 0.
#' @param shape shape parameter. Default is shape = 2.
#' @param bias_correct logical. Should we bias correct using the sample mean?
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0`
#'  autocorrelation is incorporated in the vector using an AR(\emph{1}) process.
#' @param log logical. Whether to return the log-transformed distribution.
#' @param seed seed. Numeric for `set.seed()`. Defaults to NA where no seed is
#'   set.
#'
#' @importFrom evd rgev
#'
#' @examples
#' rgev <- rgev_tails(50, 0, 1, bias_correct = TRUE)
#' plot_tails(rgev)
#'
#' @export
rgev_tails <- function(n, scale = 1, shape = 0, bias_correct = TRUE,
                       ac = 0, log = FALSE, seed = NA) {

  if (is.na(n) || n <= 0 || n != trunc(n) || length(n) != 1) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }

  if (scale <= 0) stop("Scale parameter must be greater than zero.",
                       call. = FALSE)

  # # For warnings
  # gev_pars <- generate_gev_par(scale, shape)

  if (!is.na(seed)) set.seed(seed)
  gev <- rgev(n, loc = 0, scale, shape)

  if (ac != 0) {
    gev <- acfy(gev, ac)
  }

  if (log) {
    return(gev)
  }

  gev_exp <- exp(gev)

  if (bias_correct) {
    sample_bias_corr(gev)
    } else {
      return(gev_exp)
    }
}





# Function to return the location value needed so that
# the mean of the distribution is 1.
# Also calculate the variance of the distribution.
generate_gev_par <- function(scale, shape) {
  loc <- 0
  if(shape >= 1) {
    warning("Mean is infinite because shape <= 1.")
    mu <- Inf
  } else {
    gk <- function(n) gamma(1 - n * shape)
    if (shape == 0) {
      mu <- loc + scale * -digamma(1) # Euler's constant
      location <- 1 - scale * -digamma(1) # location for mean = 1
      variance <- scale^2 * pi^2 / 6
    } else {
      mu <- loc + scale * ((gk(1) - 1) / shape)
      location <- 1 - scale * ((gk(1) - 1) / shape) # location for mean = 1
    }
  }

  if (shape < 0.5 & shape != 0) {
    variance <- scale^2 * (gk(2) - gk(1)^2) / shape^2
  }  else
    if (shape >= 0.5) {
      warning("Variance is infinite because shape >= 0.5.")
      variance <- Inf
    }

  return(list(mu = mu, loc_mu1 = location, variance = variance))
}

