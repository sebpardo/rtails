# Function to return the location parameter such that mean of the distribution is 1.
# Also calculate the variance of the distribution
generate_pareto_par <- function(shape) {
  if(shape <= 1) {
    warning("Mean is infinite because shape <= 1.")
    mu <- Inf
    location <- NA
  } else {
    mu <- 1
    location <- mu * (shape - 1) / shape
  }

  if(shape <= 2) {
    warning("Variance is infinite because shape <= 2.")
    variance <- Inf
  } else {
    variance <- location^2 * shape / (shape - 1) / (shape - 1) / (shape - 2)
  }

  return(list(mu = mu, location = location, variance = variance))
}

