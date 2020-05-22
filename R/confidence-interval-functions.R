
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
