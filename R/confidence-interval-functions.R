
CI_SMD_single <- function(delta, kappa, nu, V_delta, cover, bound) {
  t_crit <- qt(1 - cover / 2, df = nu)
  L <- kappa * nlminb(start = (delta - t_crit * sqrt(V_delta)) / kappa,
                      objective = function(ncp) (qt(cover / 2, df = nu, ncp=-ncp) + delta / kappa)^2,
                      lower = -bound, upper = bound)$par
  U <- kappa * nlminb(start = (delta + t_crit * sqrt(V_delta)) / kappa,
                      objective = function(ncp) (qt(cover / 2, df = nu, ncp=ncp) - delta / kappa)^2, 
                      lower = -bound, upper = bound)$par
  c(L,U)
}

CI_SMD <- function(delta, kappa, nu, cover = 0.05, bound = 35,
                   V_delta = kappa^2 * nu / (nu - 2) + delta^2 * (nu / (nu - 2) - 1 / J(nu)^2)) 
  mapply(CI_SMD_single, delta = delta, kappa = kappa, nu = nu, V_delta = V_delta,
         MoreArgs = list(cover = cover, bound = bound))

coverage <- function(delta, CI) CI[1,] < delta & CI[2,] > delta

