#' Random generation of generalized extreme value deviations
#'
#' Random generation for the generalized extreme value (GEV) distribution, centered around a mean
#' of one, with parameter `shape`.
#'
#' @param n sample size.
#' @param loc location parameter.
#' @param scale scale parameter.
#' @param shape shape parameter. Default is shape = 2.
#' @param bias_correct logical. Should we bias correct.
#'   The mean to use for bias correction can only be calculated when `shape < 1`.
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0` autocorrelation is
#'   incorporated in the vector using an AR(\emph{1}) process.
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
  gevpar <- generate_gev_par(scale, shape)
 # bias_corr <- 0
 # if (bias_correct) bias_corr <- gevpar$location
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
    bias_corr <- log(mean(gev_exp))
    gev_corr <- gev - bias_corr
    gev_corr_exp <- exp(gev_corr)
    return(gev_corr_exp)
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

