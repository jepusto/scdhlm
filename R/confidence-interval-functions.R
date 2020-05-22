
CI_SMD_single <- function(delta, kappa, nu, V_delta, cover, bound) {
  start_val <- suppressWarnings(J(nu) * (delta + c(-1, 1) * qt(1 - (1 - cover) / 2, df = nu) * sqrt(V_delta)) / kappa)
  L <- kappa * nlminb(start = start_val[1],
                      objective = function(ncp) suppressWarnings((qt((1 - cover) / 2, df = nu, ncp=-ncp) + delta / kappa)^2),
                      lower = -bound, upper = bound)$par
  U <- kappa * nlminb(start = start_val[2],
                      objective = function(ncp) suppressWarnings((qt((1 - cover)  / 2, df = nu, ncp=ncp) - delta / kappa)^2), 
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



## symmetric and approximate non-central t confidence interval ####

#' @title Calculates a confidence interval for a standardized mean difference
#'   effect size
#'
#' @description Calculates a confidence interval given a \code{g_REML}, a
#'   \code{g_HPS}, or a \code{g_mlm} object using either a central t
#'   distribution (for a symmetric interval) or a non-central t distribution
#'   (for an asymmetric interval).
#'
#' @param g an estimated effect size object of class \code{g_REML}, class
#'   \code{g_HPS}, or class \code{g_mlm}.
#' @param cover confidence level
#' @param bound numerical tolerance for non-centrality parameter in
#'   \code{\link[stats]{qt}}.
#' @param symmetric If \code{TRUE} (the default), use a symmetric confidence
#'   interval. If \code{FALSE}, use a non-central t approximation to obtain an
#'   asymmetric confidence interval.
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
#' Laski_g_REML <- g_REML(Laski_RML, p_const = c(0,1), r_const = c(1,0,1), returnModel = FALSE)
#' CI_g(Laski_g_REML, symmetric = TRUE)
#' CI_g(Laski_g_REML, symmetric = FALSE)
#'
#' Laski_HPS <- with(Laski, effect_size_MB(outcome, treatment, case, time))
#' CI_g(Laski_HPS, symmetric = FALSE)
#'
#' Laski_g_mlm <- g_mlm(Laski_RML, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE)
#' CI_g(Laski_g_mlm, symmetric = FALSE)
#' 

#' @name CI_g
#' @method CI_g g_REML
#' @export

CI_g.g_REML <- function(g, cover = 0.95, bound = 35, symmetric = TRUE) {
  delta <- g$delta_AB
  g_AB <- g$g_AB
  V_g_AB <- g$V_g_AB
  kappa <- g$kappa
  nu <- g$nu
  V_delta <- kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J(nu)^2)
  
  if (symmetric) {
    
    suppressWarnings(g_AB + c(-1, 1) * stats::qt(1 - (1 - cover) / 2, df = nu) * sqrt(V_g_AB))
    
  } else{
    
    CI_SMD_single(delta = delta, kappa = kappa, nu = nu, V_delta = V_delta,
                  cover = cover, bound = bound)
    
  }
  
}

#' @method CI_g g_HPS
#' @export

CI_g.g_HPS <- function(g, cover = 0.95, bound = 35, symmetric = TRUE) {
  delta <- g$delta_hat_unadj
  delta_adj <- g$delta_hat
  V_delta_adj <- g$V_delta_hat
  kappa <- g$theta
  nu <- g$nu
  V_delta <- kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J(nu)^2)
  
  if (symmetric) {
    
    suppressWarnings(delta_adj + c(-1, 1) * stats::qt(1 - (1 - cover) / 2, df = nu) * sqrt(V_delta_adj))
    
  } else {
    
    CI_SMD_single(delta=delta, kappa=kappa, nu=nu, V_delta=V_delta,
                  cover=cover, bound=bound)
    
  }
  
}

#' @method CI_g g_mlm
#' @export

CI_g.g_mlm <- function(g, cover = 0.95, bound = 35, symmetric = TRUE) {
  delta <- g$delta_AB
  g_AB <- g$g_AB
  SE_g_AB <- g$SE_g_AB
  kappa <- g$kappa
  nu <- g$nu
  J_nu <- 1 - 3 / (4 * nu - 1)
  V_delta <- kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J_nu^2)

  if (symmetric) {

    suppressWarnings(g_AB + c(-1, 1) * stats::qt(1 - (1 - cover) / 2, df = nu) * SE_g_AB)

  } else {

    CI_SMD_single(delta = delta, kappa = kappa, nu = nu,
                  V_delta = V_delta, cover = cover, bound = bound)
  }

}