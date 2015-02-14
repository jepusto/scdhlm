
CI_SMD_single <- function(delta, kappa, nu, V_delta, cover, bound) {
  start_val <- J(nu) * (delta + c(-1, 1) * qt(1 - (1 - cover) / 2, df = nu) * sqrt(V_delta)) / kappa
  L <- kappa * nlminb(start = start_val[1],
                      objective = function(ncp) (qt((1 - cover) / 2, df = nu, ncp=-ncp) + delta / kappa)^2,
                      lower = -bound, upper = bound)$par
  U <- kappa * nlminb(start = start_val[2],
                      objective = function(ncp) (qt((1 - cover)  / 2, df = nu, ncp=ncp) - delta / kappa)^2, 
                      lower = -bound, upper = bound)$par
  if (L == -bound * kappa) L <- start_val[1] * kappa
  if (U == bound * kappa) U <- start_val[2] * kappa
  c(L,U)
}

CI_SMD <- function(delta, kappa, nu, cover = 0.95, bound = 35) {
  nu <- pmax(nu, 2.001)
  V_delta = kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J(nu)^2)
  mapply(CI_SMD_single, delta = delta, kappa = kappa, nu = nu, V_delta = V_delta,
         MoreArgs = list(cover = cover, bound = bound))
}

coverage <- function(delta, CI) CI[1,] < delta & CI[2,] > delta



## approximate non-central t confidence interval ####

#' @title Approximate confidence interval for \code{g_REML} effect size estimate
#' 
#' @description Calculates an approximate confidence interval given a \code{g_REML}
#' object, based on a non-central t approximation.
#' 
#' @param g_REML a \code{g_REML} object
#' @param cover confidence level
#' @param bound numerical tolerance for non-centrality parameter in \code{\link{qt}}.
#' 
#' @export 
#' 
#' @return A vector of upper and lower confidence bounds.
#' 
#' @examples
#' data(Laski)
#' Laski_RML <- lme(fixed = outcome ~ treatment,
#'                  random = ~ 1 | case, 
#'                  correlation = corAR1(0, ~ time | case), 
#'                  data = Laski)
#' Laski_g <- g_REML(Laski_RML, p_const = c(0,1), 
#'                   r_const = c(1,0,1), returnModel=FALSE)
#' CI_g(Laski_g)

CI_g <- function(g_REML, cover = 0.95, bound = 35) {
  delta <- g_REML$delta_AB
  kappa <- g_REML$kappa
  nu <- g_REML$nu
  V_delta <- kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J(nu)^2)
  CI_SMD_single(delta=delta, kappa=kappa, nu=nu, V_delta=V_delta, 
                cover=cover, bound=bound)
}
