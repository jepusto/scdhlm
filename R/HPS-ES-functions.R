## matrix trace function ####

product.trace <- function(A,B) sum(as.vector(t(A)) * as.vector(B))

## Create correlation matrix ####

HLM_AR1_corr <- function(id_fac, time, rho, phi) {
  C.mat <- matrix(0,length(id_fac), length(id_fac))
  for (i in levels(id_fac)) {
    ind <- (id_fac == i)
    C.mat[ind,ind] <- rho + (1 - rho) * phi^as.matrix(dist(time[ind]))
  }
  return(C.mat)
}

## Calculate lagged sums of squares ####

auto_SS <- function(x, n = length(x)) {
  if (n > 1) {
    e <- x - mean(x)
    auto_0 <- sum(e * e)
    auto_1 <- sum(e[1:(n-1)] * e[2:n])
  } else {
    auto_0 <- NA
    auto_1 <- NA
  }
  return(c(auto_0, auto_1))
}

## Hedges G correction ####

J <- function(x) 1 - 3 / (4 * x - 1)




## calculate effect size (with associated estimates) for multiple baseline design ####

#' @title Calculates HPS effect size
#' 
#' @description Calculates the HPS effect size estimator based on data from a multiple baseline design, 
#' as described in Hedges, Pustejovsky, & Shadish (2013). Note that the data must contain one row per 
#' measurement occasion per subject.
#' 
#' @param outcome Vector of outcome data. May not contain any missing values.
#' @param treatment Vector of treatment indicators. Must be the same length as \code{outcome}.
#' @param id factor vector indicating unique cases. Must be the same length as \code{outcome}.
#' @param time vector of measurement occasion times. Must be the same length as \code{outcome}.
#' 
#' @export 
#' 
#' @return A list with the following components
#' \tabular{ll}{
#' \code{g_dotdot} \tab total number of non-missing observations \cr
#' \code{K} \tab number of time-by-treatment groups containing at least one observation \cr
#' \code{D_bar} \tab numerator of effect size estimate \cr
#' \code{S_sq} \tab sample variance, pooled across time points and treatment groups \cr
#' \code{delta_hat_unadj} \tab unadjusted effect size estimate \cr
#' \code{phi_hat} \tab corrected estimate of first-order auto-correlation \cr
#' \code{sigma_sq_w} \tab corrected estimate of within-case variance \cr
#' \code{rho_hat} \tab estimated intra-class correlation \cr
#' \code{theta} \tab estimated scalar constant \cr
#' \code{nu} \tab estimated degrees of freedom \cr
#' \code{delta_hat} \tab corrected effect size estimate \cr
#' \code{V_delta_hat} \tab estimated variance of \code{delta_hat}
#' }
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs. 
#' Working paper, Evanston, IL.
#' 
#' @examples
#' data(Saddler)
#' with(subset(Saddler, measure==1), effect_size_MB(outcome, treatment, case, time))
#' 
#' data(Laski)
#' with(Laski, effect_size_MB(outcome, treatment, case, time))

effect_size_MB <- function(outcome, treatment, id, time) {
  
  ###########
  ## setup ##
  ###########
  
  # create factor variables
  treatment_fac <- factor(treatment)
  id_fac <- factor(id)
  time_fac <- factor(time)
    
  # unique times, unique cases, calculate sample sizes  
  time_points <- seq(min(time), max(time),1)    # unique measurement occasions j = 1,...,N
  N <- length(time_points)                      # number of unique measurement occasions
  h_i_p <- table(id_fac, treatment_fac)         # number of non-missing observations for case i in phase p
  cases <- levels(id_fac)                       # unique cases i = 1,...,m
  m <- length(cases)                            # number of cases
  g_dotdot <- length(outcome)                   # total number of non-missing observations
  
  
  ######################################
  ## calculate unadjusted effect size ##
  ######################################
  
  # fixed effects regression with id-by-treatment interaction
  case_FE <- lm(outcome ~ id_fac + id_fac:treatment_fac + 0)
  X_case_FE <- model.matrix(case_FE)                            # design matrix from id-by-treatment fixed-effects regression
  X_trt <- attr(X_case_FE, "assign") == 2                       # indicator for individual treatment effects
  XtX_inv_case_FE <- solve(t(X_case_FE) %*% X_case_FE)          # inverse of (X'X) for this design matrix
  
  # calculate D-bar
  D_bar <- mean(coef(case_FE)[X_trt])           # See p. 11, Eq. (2).
  
  # fixed effects regression with time-by-treatment interaction
  time_FE <- lm(outcome ~ time_fac + time_fac:treatment_fac + 0)
  X_time_FE <- model.matrix(time_FE)[,time_FE$qr$pivot[1:time_FE$qr$rank]]    # design matrix for time-by-treatment fixed-effects regression
  
  # calculate pooled variance S-squared
  S_sq <- summary(time_FE)$sigma^2              # See p. 12, Eq. (3) and also footnote 5.
  K <- time_FE$rank                             # number of time-by-treatment groups containing at least one observation. See p. 11.

  # calculate unadjusted effect size
  delta_hat_unadj <- D_bar / sqrt(S_sq)         # See p. 13, Eq. (4)
  
  
  ##################################
  ## nuisance parameter estimates ##
  ##################################
  
  # auto-covariances - See first display equation on p. 32.
  acv_SS <- matrix(unlist(tapply(case_FE$residuals[order(id_fac, time)], id_fac, auto_SS)), m, 2, byrow=TRUE)
  
  # calculate adjusted autocorrelation
  phi_YW <- sum(acv_SS[,2]) / sum(acv_SS[,1])
  phi_correction <- sum((h_i_p - 1) / h_i_p) / (g_dotdot - 2 * m)   # This is the constant C given on p. 33.
  phi_hat <- phi_YW + phi_correction                                # See last display equation on p. 32.
  
  # calculate adjusted within-case variance estimate
  sigma_sq_correction <- g_dotdot - product.trace(XtX_inv_case_FE, t(X_case_FE) %*% HLM_AR1_corr(id_fac, time, 0, phi_hat) %*% X_case_FE) 
                                                                    # This correction is equal to g_dotdot * F, where F is given on p. 33. 
  sigma_sq_w <- sum(acv_SS[,1]) / sigma_sq_correction
  
  # calculate intra-class correlation
  rho_hat <- max(0, 1 - sigma_sq_w / S_sq)                          # See last display equation on p. 33.
  
  
  
  ############################################
  ## calculate degrees of freedom and theta ##
  ############################################
  
  # create A matrix
  A_mat <- diag(rep(1, g_dotdot)) - X_time_FE %*% solve(t(X_time_FE) %*% X_time_FE) %*% t(X_time_FE)  # S^2 = y'(A_mat)y / (g_dotdot - K). See p. 29. 
  
  # create correlation matrix
  V_mat <- HLM_AR1_corr(id_fac, time, rho_hat, phi_hat)    # V_mat is the matrix Sigma on p. 28, scaled by tau^2 + sigma^2. 
  
  # calculate degrees of freedom
  AV <- A_mat %*% V_mat
  nu <- (g_dotdot - K)^2 / product.trace(AV, AV)          # See p. 15, Eq. (11). Also note that product.trace(AV, AV) = tr(A Sigma A Sigma). See p. 30.

  # calculate theta
  theta <- sqrt(sum(diag(XtX_inv_case_FE %*% t(X_case_FE) %*% V_mat %*% X_case_FE %*% XtX_inv_case_FE)[X_trt])) / m   # See p. 15, Eq. (10). 
  

  #######################################
  ## adjusted effect size and variance ##
  #######################################
  
  # calculate adjusted effect size
  delta_hat <- J(nu) * delta_hat_unadj    # See p. 15, Eq. (13). 
  
  # calculate variance of adjusted effect size
  V_delta_hat <- J(nu)^2 * (theta^2 * nu / (nu - 2) + delta_hat^2 * (nu / (nu - 2) - 1 / J(nu)^2))   # See p. 15, Eq. (14). 
  
  
  ####################
  ## return results ##
  ####################
  
  results <- list(g_dotdot = g_dotdot, K = K, D_bar = D_bar, S_sq = S_sq, delta_hat_unadj = delta_hat_unadj, 
               phi_hat = phi_hat, sigma_sq_w = sigma_sq_w, rho_hat = rho_hat, 
               theta = theta, nu = nu, delta_hat = delta_hat, V_delta_hat = V_delta_hat)
  return(results)
}



## calculate effect size (with associated estimates) for (AB)^k design ####

#' @title Calculates HPS effect size
#' 
#' @description Calculates the HPS effect size estimator based on data from an (AB)^k design, 
#' as described in Hedges, Pustejovsky, & Shadish (2012). Note that the data must contain one row per 
#' measurement occasion per subject.
#' 
#' @param outcome Vector of outcome data. May not contain any missing values.
#' @param treatment Vector of treatment indicators. Must be the same length as \code{outcome}.
#' @param id factor vector indicating unique cases. Must be the same length as \code{outcome}.
#' @param time vector of measurement occasion times. Must be the same length as \code{outcome}.
#' 
#' @export 
#' 
#' @return A list with the following components
#' \tabular{ll}{
#' \code{g_dotdot} \tab total number of non-missing observations \cr
#' \code{K} \tab number of time-by-treatment groups containing at least one observation \cr
#' \code{D_bar} \tab numerator of effect size estimate \cr
#' \code{S_sq} \tab sample variance, pooled across time points and treatment groups \cr
#' \code{delta_hat_unadj} \tab unadjusted effect size estimate \cr
#' \code{phi_hat} \tab corrected estimate of first-order auto-correlation \cr
#' \code{sigma_sq_w} \tab corrected estimate of within-case variance \cr
#' \code{rho_hat} \tab estimated intra-class correlation \cr
#' \code{theta} \tab estimated scalar constant \cr
#' \code{nu} \tab estimated degrees of freedom \cr
#' \code{delta_hat} \tab corrected effect size estimate \cr
#' \code{V_delta_hat} \tab estimated variance of \code{delta_hat}
#' }
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#'  A standardized mean difference effect size for single case designs. 
#'  Research Synthesis Methods, 3, 224-239. doi:10.1002/jrsm.1052
#' 
#' @examples
#' data(Saddler)

effect_size_ABk <- function(outcome, treatment, id, time) {
  
  ###########
  ## setup ##
  ###########
  
  # create factor variables
  treatment_fac <- factor(treatment)
  id_fac <- factor(id)
  time_fac <- factor(time)
  
  # unique times, unique cases, calculate sample sizes  
  time_points <- seq(min(time), max(time),1)    # unique measurement occasions j = 1,...,N
  N <- length(time_points)                      # number of unique measurement occasions
  h_i_p <- table(id_fac, treatment_fac)         # number of non-missing observations for case i in phase p
  cases <- levels(id_fac)                       # unique cases i = 1,...,m
  m <- length(cases)                            # number of cases
  g_dotdot <- length(outcome)                   # total number of non-missing observations
  
  
  ######################################
  ## calculate unadjusted effect size ##
  ######################################
  
  # fixed effects regression with id-by-treatment interaction
  case_FE <- lm(outcome ~ id_fac + id_fac:treatment_fac + 0)
  X_case_FE <- model.matrix(case_FE)                            # design matrix from id-by-treatment fixed-effects regression
  X_trt <- attr(X_case_FE, "assign") == 2                       # indicator for individual treatment effects
  XtX_inv_case_FE <- solve(t(X_case_FE) %*% X_case_FE)          # inverse of (X'X) for this design matrix
  
  # calculate D-bar
  D_bar <- mean(coef(case_FE)[X_trt])           # See p. 11, Eq. (2).
  
  # fixed effects regression with time-by-treatment interaction
  time_FE <- lm(outcome ~ time_fac + time_fac:treatment_fac + 0)
  X_time_FE <- model.matrix(time_FE)[,time_FE$qr$pivot[1:time_FE$qr$rank]]    # design matrix for time-by-treatment fixed-effects regression
  
  # calculate pooled variance S-squared
  S_sq <- summary(time_FE)$sigma^2              # See p. 12, Eq. (3) and also footnote 5.
  K <- time_FE$rank                             # number of time-by-treatment groups containing at least one observation. See p. 11.
  
  # calculate unadjusted effect size
  delta_hat_unadj <- D_bar / sqrt(S_sq)         # See p. 13, Eq. (4)
  
  
  ##################################
  ## nuisance parameter estimates ##
  ##################################
  
  # auto-covariances - See first display equation on p. 32.
  acv_SS <- matrix(unlist(tapply(case_FE$residuals[order(id_fac, time)], id_fac, auto_SS)), m, 2, byrow=TRUE)
  
  # calculate adjusted autocorrelation
  phi_YW <- sum(acv_SS[,2]) / sum(acv_SS[,1])
  phi_correction <- sum((h_i_p - 1) / h_i_p) / (g_dotdot - 2 * m)   # This is the constant C given on p. 33.
  phi_hat <- phi_YW + phi_correction                                # See last display equation on p. 32.
  
  # calculate adjusted within-case variance estimate
  sigma_sq_correction <- g_dotdot - product.trace(XtX_inv_case_FE, t(X_case_FE) %*% HLM_AR1_corr(id_fac, time, 0, phi_hat) %*% X_case_FE) 
  # This correction is equal to g_dotdot * F, where F is given on p. 33. 
  sigma_sq_w <- sum(acv_SS[,1]) / sigma_sq_correction
  
  # calculate intra-class correlation
  rho_hat <- max(0, 1 - sigma_sq_w / S_sq)                          # See last display equation on p. 33.
  
  
  
  ############################################
  ## calculate degrees of freedom and theta ##
  ############################################
  
  # create A matrix
  A_mat <- diag(rep(1, g_dotdot)) - X_time_FE %*% solve(t(X_time_FE) %*% X_time_FE) %*% t(X_time_FE)  # S^2 = y'(A_mat)y / (g_dotdot - K). See p. 29. 
  
  # create correlation matrix
  V_mat <- HLM_AR1_corr(id_fac, time, rho_hat, phi_hat)    # V_mat is the matrix Sigma on p. 28, scaled by tau^2 + sigma^2. 
  
  # calculate degrees of freedom
  AV <- A_mat %*% V_mat
  nu <- (g_dotdot - K)^2 / product.trace(AV, AV)          # See p. 15, Eq. (11). Also note that product.trace(AV, AV) = tr(A Sigma A Sigma). See p. 30.
  
  # calculate theta
  theta <- sqrt(sum(diag(XtX_inv_case_FE %*% t(X_case_FE) %*% V_mat %*% X_case_FE %*% XtX_inv_case_FE)[X_trt])) / m   # See p. 15, Eq. (10). 
  
  
  #######################################
  ## adjusted effect size and variance ##
  #######################################
  
  # calculate adjusted effect size
  delta_hat <- J(nu) * delta_hat_unadj    # See p. 15, Eq. (13). 
  
  # calculate variance of adjusted effect size
  V_delta_hat <- J(nu)^2 * (theta^2 * nu / (nu - 2) + delta_hat^2 * (nu / (nu - 2) - 1 / J(nu)^2))   # See p. 15, Eq. (14). 
  
  
  ####################
  ## return results ##
  ####################
  
  results <- list(g_dotdot = g_dotdot, K = K, D_bar = D_bar, S_sq = S_sq, delta_hat_unadj = delta_hat_unadj, 
                  phi_hat = phi_hat, sigma_sq_w = sigma_sq_w, rho_hat = rho_hat, 
                  theta = theta, nu = nu, delta_hat = delta_hat, V_delta_hat = V_delta_hat)
  return(results)
}
