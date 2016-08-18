
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
#' @param phi Optional value of the auto-correlation nuisance parameter, to be used 
#' in calculating the small-sample adjusted effect size
#' @param rho Optional value of the intra-class correlation nuisance parameter, to be used 
#' in calculating the small-sample adjusted effect size
#' 
#' @note If phi or rho is left unspecified (or both), estimates for the nuisance
#' parameters will be calculated.
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
#' \code{phi} \tab corrected estimate of first-order auto-correlation \cr
#' \code{sigma_sq_w} \tab corrected estimate of within-case variance \cr
#' \code{rho} \tab estimated intra-class correlation \cr
#' \code{theta} \tab estimated scalar constant \cr
#' \code{nu} \tab estimated degrees of freedom \cr
#' \code{delta_hat} \tab corrected effect size estimate \cr
#' \code{V_delta_hat} \tab estimated variance of \code{delta_hat}
#' }
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods, 4}(4), 324-341. doi:\href{http://doi.org/10.1002/jrsm.1086}{10.1002/jrsm.1086}
#' 
#' @examples
#' data(Saddler)
#' with(subset(Saddler, measure=="writing quality"), effect_size_MB(outcome, treatment, case, time))
#' 
#' data(Laski)
#' with(Laski, effect_size_MB(outcome, treatment, case, time))
#' 

effect_size_MB <- function(outcome, treatment, id, time, phi, rho) {
  
  ###########
  ## setup ##
  ###########
  
  # create factor variables
  dat <- data.frame(id_fac = factor(id),
                    treatment_fac = factor(treatment),
                    time_fac = factor(time),
                    outcome, time)
  dat <- dat[with(dat, order(id_fac, time)),]
    
  # unique times, unique cases, calculate sample sizes  
  time_points <- seq(min(time), max(time),1)            # unique measurement occasions j = 1,...,N
  N <- length(time_points)                              # number of unique measurement occasions
  h_i_p <- with(dat, table(id_fac, treatment_fac))      # number of non-missing observations for case i in phase p
  cases <- levels(dat$id_fac)                           # unique cases i = 1,...,m
  m <- length(cases)                                    # number of cases
  g_dotdot <- length(outcome)                           # total number of non-missing observations
  
  
  ######################################
  ## calculate unadjusted effect size ##
  ######################################
  
  # fixed effects regression with id-by-treatment interaction
  case_FE <- lm(outcome ~ id_fac + id_fac:treatment_fac + 0, data = dat)
  X_case_FE <- model.matrix(case_FE)                            # design matrix from id-by-treatment fixed-effects regression
  X_trt <- attr(X_case_FE, "assign") == 2                       # indicator for individual treatment effects
  XtX_inv_case_FE <- solve(t(X_case_FE) %*% X_case_FE)          # inverse of (X'X) for this design matrix
  
  # calculate D-bar
  D_bar <- mean(coef(case_FE)[X_trt])           # See p. 11, Eq. (2).
  
  # fixed effects regression with time-by-treatment interaction
  time_FE <- lm(outcome ~ time_fac + time_fac:treatment_fac + 0, data = dat)
  X_time_FE <- model.matrix(time_FE)[,time_FE$qr$pivot[1:time_FE$qr$rank]]    # design matrix for time-by-treatment fixed-effects regression
  
  # calculate pooled variance S-squared
  S_sq <- summary(time_FE)$sigma^2              # See p. 12, Eq. (3) and also footnote 5.
  K <- time_FE$rank                             # number of time-by-treatment groups containing at least one observation. See p. 11.

  # calculate unadjusted effect size
  delta_hat_unadj <- D_bar / sqrt(S_sq)         # See p. 13, Eq. (4)
  
  
  ##################################
  ## nuisance parameter estimates ##
  ##################################
  
  if (missing(phi) | missing(rho)) {
    # auto-covariances - See first display equation on p. 32.
    acv_SS <- matrix(unlist(tapply(case_FE$residuals, dat$id_fac, auto_SS)), m, 2, byrow=TRUE)
    
    # calculate adjusted autocorrelation
    if (missing(phi)) {
      phi_YW <- sum(acv_SS[,2], na.rm=T) / sum(acv_SS[,1], na.rm=T)
      phi_correction <- sum((h_i_p - 1) / h_i_p) / (g_dotdot - 2 * m)   # This is the constant C given on p. 33.
      phi <- phi_YW + phi_correction                                # See last display equation on p. 32.      
    }
    
    if (missing(rho)) {
      # calculate adjusted within-case variance estimate
      sigma_sq_correction <- g_dotdot - product_trace(XtX_inv_case_FE, t(X_case_FE) %*% 
                                                        with(dat, HLM_AR1_corr(id_fac, time, 0, phi)) %*% X_case_FE) 
      
      # This correction is equal to g_dotdot * F, where F is given on p. 33. 
      sigma_sq_w <- sum(acv_SS[,1], na.rm=T) / sigma_sq_correction
      
      # calculate intra-class correlation
      rho <- max(0, 1 - sigma_sq_w / S_sq)                          # See last display equation on p. 33.    
    } else sigma_sq_w <- NA
  } else sigma_sq_w <- NA
  
  
  ############################################
  ## calculate degrees of freedom and theta ##
  ############################################
  
  # create A matrix
  A_mat <- diag(rep(1, g_dotdot)) - X_time_FE %*% solve(t(X_time_FE) %*% X_time_FE) %*% t(X_time_FE)  # S^2 = y'(A_mat)y / (g_dotdot - K). See p. 29. 
  
  # create correlation matrix V_mat, which is the matrix Sigma on p. 28, scaled by tau^2 + sigma^2. 
  V_mat <- with(dat, HLM_AR1_corr(id_fac, time, rho, phi))
  
  # calculate degrees of freedom
  AV <- A_mat %*% V_mat
  nu <- (g_dotdot - K)^2 / product_trace(AV, AV)          # See p. 15, Eq. (11). Also note that product_trace(AV, AV) = tr(A Sigma A Sigma). See p. 30.

  # calculate theta
  theta <- sqrt(sum(diag(XtX_inv_case_FE %*% t(X_case_FE) %*% V_mat %*% X_case_FE %*% XtX_inv_case_FE)[X_trt])) / m   # See p. 15, Eq. (10). 
  

  #######################################
  ## adjusted effect size and variance ##
  #######################################
  
  # calculate adjusted effect size
  delta_hat <- J(nu) * delta_hat_unadj    # See p. 15, Eq. (13). 
  
  # calculate variance of adjusted effect size
  nu_trunc <- max(nu, 2.001)
  V_delta_hat <- J(nu)^2 * (theta^2 * nu_trunc / (nu_trunc - 2) + delta_hat^2 * (nu_trunc / (nu_trunc - 2) - 1 / J(nu)^2))   # See p. 15, Eq. (14). 
  
  
  ####################
  ## return results ##
  ####################
  
  results <- list(g_dotdot = g_dotdot, K = K, D_bar = D_bar, S_sq = S_sq, delta_hat_unadj = delta_hat_unadj, 
               phi = phi, sigma_sq_w = sigma_sq_w, rho = rho, 
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
#' @param phase factor vector indicating unique phases (each containing one contiguous control 
#' condition and one contiguous treatment condition). Must be the same length as \code{outcome}.
#' @param time vector of measurement occasion times. Must be the same length as \code{outcome}.
#' @param phi Optional value of the auto-correlation nuisance parameter, to be used 
#' in calculating the small-sample adjusted effect size
#' @param rho Optional value of the intra-class correlation nuisance parameter, to be used 
#' in calculating the small-sample adjusted effect size
#' 
#' @note If phi or rho is left unspecified (or both), estimates for the nuisance
#' parameters will be calculated.
#' 
#' @export 
#' 
#' @return A list with the following components
#' \tabular{ll}{
#' \code{M_a} \tab Matrix reporting the total number of time points with data for all ids, 
#' by phase and treatment condition \cr
#' \code{M_dot} \tab Total number of time points used to calculate the total variance (the sum of \code{M_a}) \cr
#' \code{D_bar} \tab numerator of effect size estimate \cr
#' \code{S_sq} \tab sample variance, pooled across time points and treatment groups \cr
#' \code{delta_hat_unadj} \tab unadjusted effect size estimate \cr
#' \code{phi} \tab corrected estimate of first-order auto-correlation \cr
#' \code{sigma_sq_w} \tab corrected estimate of within-case variance \cr
#' \code{rho} \tab estimated intra-class correlation \cr
#' \code{theta} \tab estimated scalar constant \cr
#' \code{nu} \tab estimated degrees of freedom \cr
#' \code{delta_hat} \tab corrected effect size estimate \cr
#' \code{V_delta_hat} \tab estimated variance of the effect size
#' }
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:\href{http://doi.org/10.1002/jrsm.1052}{10.1002/jrsm.1052}
#' 
#' @examples
#' data(Lambert)
#' with(Lambert, effect_size_ABk(outcome, treatment, case, phase, time))
#'    
#' data(Anglesea)
#' with(Anglesea, effect_size_ABk(outcome, condition, case, phase, session))
#' 


effect_size_ABk <- function(outcome, treatment, id, phase, time, phi, rho) {
  
  ###########
  ## setup ##
  ###########
  
  # create factor variables
  dat <- data.frame(id_fac = factor(id),
                    phase_fac = factor(phase), 
                    treatment_fac = factor(treatment),
                    outcome, time)
  dat <- dat[with(dat, order(id_fac, phase_fac, treatment_fac, time)),]
  
  # number of cases
  m <- nlevels(dat$id_fac)                            
  
  # re-number time points per HPS (2012)
  dat$phase_point <- with(dat, unlist(tapply(outcome, 
                                             list(treatment_fac, phase_fac, id_fac), 
                                             function(x) 1:length(x))))
  dat$phase_point_fac <- ordered(dat$phase_point)
  
  # determine M^a values. See p. 231, formulas (16-17)
  M_a <- with(dat, apply(tapply(outcome, list(phase_point, treatment_fac, phase_fac), 
                                function(x) length(x) == m), c(2,3), sum, na.rm = TRUE))
  M_dot <- sum(M_a, na.rm = TRUE)  
  
  dat$include <- mapply(function(phase, treat, point)
    m == sum(phase==dat$phase_fac & treat==dat$treatment_fac & point==dat$phase_point),
    dat$phase_fac, dat$treatment_fac, dat$phase_point)
  
  ######################################
  ## calculate unadjusted effect size ##
  ######################################
  
  if (nlevels(dat$phase_fac) > 1) {
    # fixed effects regression with id-by-phase-by-treatment interaction
    case_FE <- lm(outcome ~ id_fac:phase_fac + id_fac:phase_fac:C(treatment_fac, contr.treatment) + 0, data = dat)
    # fixed effects regression with phase-point-by-treatment-by-phase interaction
    time_FE <- lm(outcome ~ phase_point_fac:treatment_fac:phase_fac + 0,
                  data = dat, subset = dat$include)    
  } else {
    # fixed effects regression with id-by-phase-by-treatment interaction
    case_FE <- lm(outcome ~ id_fac + id_fac:C(treatment_fac, treatment) + 0, data = dat)
    # fixed effects regression with phase-point-by-treatment-by-phase interaction
    time_FE <- lm(outcome ~ phase_point_fac:treatment_fac + 0,
                  data = dat, subset = dat$include)    
    
  }
  
  # design matrix from id-by-phase-by-treatment fixed-effects regression
  X_case_FE <- model.matrix(case_FE)
  X_keep <- !is.na(coef(case_FE))
  X_trt <- (attr(X_case_FE, "assign") == 2)[X_keep]    # indicator for individual treatment effects
  XtX_inv_case_FE <- chol2inv(chol(t(X_case_FE[,X_keep,drop=FALSE]) %*% X_case_FE[,X_keep,drop=FALSE]))          # inverse of (X'X) for this design matrix
  
  # calculate D-bar
  D_bar <- mean(coef(case_FE)[X_keep][X_trt])           # See p. 231, formula (19).
  
  # design matrix for phase-point-by-treatment-by-phase fixed-effects regression
  X_time_FE <- model.matrix(time_FE)[,time_FE$qr$pivot[1:time_FE$qr$rank]]    
  
  # calculate pooled variance S-squared
  S_sq <- summary(time_FE)$sigma^2              # See p. 231, formula (18).
  
  # calculate unadjusted effect size
  delta_hat_unadj <- D_bar / sqrt(S_sq)         # See p. 231, formula (20).
  
  
  ##################################
  ## nuisance parameter estimates ##
  ##################################
  
  if (missing(phi) | missing(rho)) {
    # auto-covariances - See first display equation on p. 32.
    YW <- with(dat, aggregate(outcome, by = list(id_fac, phase_fac, treatment_fac), 
                    function(x) c(auto_SS(x), length(x)))) # calculate auto-covariances by case by phase
    YW <- cbind(YW[,1:3], YW$x)
    names(YW) <- c("id_fac","phase_fac","treatment_fac","g0","g1","n")

    # calculate adjusted estimate of pooled auto-correlation. See p. 238.
    if (missing(phi)) phi <- sum(YW$g1, na.rm=T) / sum(YW$g0, na.rm=T) + sum(1 - 1 / YW$n) / sum(YW$n - 1)
    
    if (missing(rho)) {
      # calculate adjusted within-case variance estimate
      sigma_sq_correction <- length(dat$outcome) - 
                              product_trace(XtX_inv_case_FE, 
                                            t(X_case_FE[,X_keep,drop=FALSE]) %*% 
                                              with(dat, HLM_AR1_corr(id_fac, phase_point, 0, phi)) %*% 
                                              X_case_FE[,X_keep,drop=FALSE]) 
      sigma_sq_w <- sum(YW$g0, na.rm=T) / sigma_sq_correction
      
      rho <- max(0, 1 - sigma_sq_w / S_sq)      # See last display equation on p. 238.
    } else sigma_sq_w <- NA
  } else sigma_sq_w <- NA
  
  
  ############################################
  ## calculate degrees of freedom and theta ##
  ############################################
  
  # create A matrix. S^2 = y'(A_mat)y / (M_dot * (m - 1)). See p. 236. 
  A_mat <- diag(rep(1, dim(X_time_FE)[1])) - X_time_FE %*% solve(t(X_time_FE) %*% X_time_FE) %*% t(X_time_FE)  
  
  # V_mat is the matrix Sigma_T on p. 236, scaled by tau^2 + sigma^2. 
  V_mat <- with(dat, HLM_AR1_corr(id_fac, time, rho, phi))
  
  # calculate degrees of freedom. See p. 232, formula (28). 
  AV <- A_mat %*% V_mat[dat$include, dat$include]
  nu <- (M_dot * (m - 1))^2 / product_trace(AV, AV)
  
  # calculate theta. See p. 232, formula (27).
  theta <- sqrt(sum((XtX_inv_case_FE %*% t(X_case_FE[,X_keep,drop=FALSE]) %*% 
              V_mat %*% X_case_FE[,X_keep,drop=FALSE] %*% XtX_inv_case_FE)[X_trt, X_trt])) / sum(X_trt)
  
  
  #######################################
  ## adjusted effect size and variance ##
  #######################################
  
  # calculate adjusted effect size. See p. 232, formula (29).
  delta_hat <- J(nu) * delta_hat_unadj    # See p. 15, Eq. (13). 
  
  # calculate variance of adjusted effect size. See p. 232, formula (30).
  nu_trunc <- max(nu, 2.001)
  V_delta_hat <- J(nu)^2 * (theta^2 * nu_trunc / (nu_trunc - 2) + delta_hat^2 * (nu_trunc / (nu_trunc - 2) - 1 / J(nu)^2))   # See p. 15, Eq. (14). 
  
  
  ####################
  ## return results ##
  ####################
  
  results <- list(M_a = M_a, M_dot = M_dot,
                  D_bar = D_bar, S_sq = S_sq, delta_hat_unadj = delta_hat_unadj, 
                  phi = phi, sigma_sq_w = sigma_sq_w, rho = rho, 
                  theta = theta, nu = nu, 
                  delta_hat = delta_hat, V_delta_hat = V_delta_hat)
  return(results)
}



##-----------------------------------------------------------------------------
## calculate HPS effect size (with associated estimates)
## Note: this is a vectorized version of the function for use in simulations 
##-----------------------------------------------------------------------------

HPS_effect_size <- function(outcomes, treatment, id, time) {
  
  ## setup ##
  
  # create factor variables
  treatment_fac <- factor(treatment)
  id_fac <- factor(id)
  time_fac <- factor(time)
  time_list <- by(time, id_fac, function(x) x)
  
  # unique times, unique cases, calculate sample sizes  
  time_points <- seq(min(time), max(time),1)    # unique measurement occasions j = 1,...,N
  N <- length(time_points)                      # number of unique measurement occasions
  h_i_p <- table(id_fac, treatment_fac)         # number of non-missing observations for case i in phase p
  cases <- levels(id_fac)                       # unique cases i = 1,...,m
  m <- length(cases)                            # number of cases
  g_dotdot <- length(treatment)                 # total number of non-missing observations
  
  
  ## calculate unadjusted effect size ##
  
  y <- as.matrix(outcomes)[,1]
  
  # fixed effects regression with id-by-treatment interaction
  case_FE <- lm(y ~ id_fac + id_fac:treatment_fac + 0)
  X_case_FE <- model.matrix(case_FE)                            # design matrix from id-by-treatment fixed-effects regression
  X_trt <- attr(X_case_FE, "assign") == 2                       # indicator for individual treatment effects
  XtX_inv_Xt_case_FE <- solve(t(X_case_FE) %*% X_case_FE) %*% t(X_case_FE)          #  (X'X)^-1 X'
  
  # calculate D-bar
  D_bar <- colMeans((XtX_inv_Xt_case_FE[X_trt,] %*% outcomes))
  
  # fixed effects regression with time-by-treatment interaction
  time_FE <- lm(y ~ time_fac + time_fac:treatment_fac + 0)
  X_time_FE <- model.matrix(time_FE)[,time_FE$qr$pivot[1:time_FE$qr$rank]]    # design matrix for time-by-treatment fixed-effects regression
  
  # calculate pooled variance S-squared
  
  A_mat <- diag(rep(1, g_dotdot)) - X_time_FE %*% solve(t(X_time_FE) %*% X_time_FE) %*% t(X_time_FE)  # S^2 = y'(A_mat)y / (g_dotdot - K). See p. 29. 
  K <- time_FE$rank                             # number of time-by-treatment groups containing at least one observation. See p. 11.
  S_sq <- colSums(outcomes * (A_mat %*% outcomes)) / (g_dotdot - K)
  
  # calculate unadjusted effect size
  delta_hat_unadj <- D_bar / sqrt(S_sq)         # See p. 13, Eq. (4)
  
  
  ## nuisance parameter estimates ##
  
  # auto-covariances - See first display equation on p. 32.
  residuals <- (diag(rep(1, g_dotdot)) - X_case_FE %*% XtX_inv_Xt_case_FE) %*% outcomes
  acv_SS <- rowSums(matrix(unlist(by(residuals, id_fac, function(x) colSums(x[1:(dim(x)[1]-1),] * x[2:dim(x)[1],]))), dim(outcomes)[2], m))
  SS_within <- colSums(residuals * residuals)
  
  # calculate adjusted autocorrelation
  phi_correction <- sum((h_i_p - 1) / h_i_p) / (g_dotdot - 2 * m)     # This is the constant C given on p. 33.
  phi_hat <- acv_SS / SS_within + phi_correction                      # See last display equation on p. 32.
  
  # calculate adjusted within-case variance estimate
  sigma_sq_correction <- g_dotdot - sapply(phi_hat, function(phi)
    product_trace(XtX_inv_Xt_case_FE, 
      prod_blockmatrix(AR1_corr_block(phi=phi, block=id_fac, times=time_list), 
                       X_case_FE)))
  
  # This correction is equal to g_dotdot * F, where F is given on p. 33. 
  sigma_sq_w <- SS_within / sigma_sq_correction
  
  # calculate intra-class correlation
  rho_hat <- pmax(0, 1 - sigma_sq_w / S_sq) # See last display equation on p. 33.
  
  
  ## calculate degrees of freedom and theta ##
  
  df_theta <- function(rho, phi) {
    V_mat <- lmeAR1_cov_block(block=id_fac, Z_design=rep(1,g_dotdot), 
                    theta = list(sigma_sq = 1 - rho, phi=phi, Tau = rho), 
                    times = time_list)
    AV <- prod_matrixblock(A_mat, V_mat, block=id_fac)
    nu <- (g_dotdot - K)^2 / product_trace(AV, AV)          # See p. 15, Eq. (11). Also note that product.trace(AV, AV) = tr(A Sigma A Sigma). See p. 30.
    theta <- sqrt(sum(diag(prod_matrixblock(XtX_inv_Xt_case_FE[X_trt,], V_mat, block=id_fac) %*% t(XtX_inv_Xt_case_FE[X_trt,])))) / m   # See p. 15, Eq. (10). 
    return(c(nu, theta))
  }
  
  df_theta <- mapply(df_theta, rho=rho_hat, phi=phi_hat)
  df <- df_theta[1,]
  theta <- df_theta[2,]
  
  ## adjusted effect size and variance ##
  
  # calculate adjusted effect size
  delta_hat <- J(df) * delta_hat_unadj    # See p. 15, Eq. (13). 
  
  # calculate variance of adjusted effect size
  df_trunc <- pmax(df, 2.001)
  V_delta_hat <- J(df)^2 * (theta^2 * df_trunc / (df_trunc - 2) + delta_hat^2 * (df_trunc / (df_trunc - 2) - 1 / J(df)^2))   # See p. 15, Eq. (14). 
  
  
  
  ## return results ##
  
  rbind(D_bar = D_bar, S_sq = S_sq, delta_hat_unadj = delta_hat_unadj, 
        phi_hat = phi_hat, rho_hat = rho_hat, kappa_hat = theta, df = df,
        delta_hat = delta_hat, V_delta_hat = V_delta_hat)
}