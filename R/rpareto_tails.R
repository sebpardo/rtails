#' Random generation of Pareto deviations
#'
#' Random generation for the Pareto distribution, centered around a mean
#' of one, with parameter `shape`.
#'
#' @param n sample size.
#' @param shape shape parameter. Default is shape = 2.
#'
#' @importFrom EnvStats rpareto
#'
#' @examples
#' rpt <- rpareto_tails(50, 3)
#' plot_tails(rpt)
#'
#' @export
rpareto_tails <- function(n, shape = 2) {
  rpar <- generate_pareto_par(shape)
  rpareto(n, rpar$location, shape)
}
