##------------------------------------------------------------------------------
## create design matrix 
##------------------------------------------------------------------------------

MBTreatTimes <- function(m, n, min_B = 3, min_T = 3) {
  if (n / (m + 1) >= max(min_B, min_T)) {
    treat_times <- 1 + round((1:m) * n / (m + 1))
  } else if ((n - min_B) / m >= min_T) {
    treat_times <- min_B + round((0:(m-1)) * (n - min_B) / m)
  } else if ((n - min_B - min_T + 1) / (m - 1) > 1) {
    treat_times <- min_B + 1 + round((0:(m-1)) * (n - min_B - min_T) / (m - 1))
  } else {
    treat_times <- min_B + rep(1:(n - min_B - min_T + 1), length.out = m)
  }
  return(sort(treat_times))
}


#' @title Create a design matrix for a single-case design
#' 
#' @description Create a design matrix containing a linear trend, a treatment effect, and a 
#' trend-by-treatment interaction for a single-case design with \code{m} cases and \code{n} 
#' measurement occasions.
#' 
#' @param m number of cases
#' @param n number of time points
#' @param treat_times (Optional) vector of length \code{m} listing treatment introduction times for each case. 
#' @param center centering point for time trend.
#'   
#' @export 
#' 
#' @return A design matrix 
#' 
#' @examples 
#' design_matrix(3, 16, c(5,9,13))

design_matrix <- function(m, n, treat_times = n / 2 + 1, center = 0) {
  X <- data.frame(matrix(NA, m * n, 5))
  attr(X, "bal") <- (length(unique(treat_times)) == 1)
  if (length(treat_times) < m) treat_times <- rep(treat_times, length.out=m)
  
  names(X) <- c("id","constant","treatment","trend","interaction")
  
  X$id <- factor(rep(1:m, each = n))
  X$constant <- rep(1, m * n)
  X$treatment <- as.vector(sapply(treat_times, function(t) c(rep(0, t - 1), rep(1,n - t + 1))))
  X$trend <- rep(1:n, m) - center
  X$interaction <- as.vector(sapply(treat_times, function(t) c(rep(0, t - 1), 1:(n - t + 1))))
  
  return(X)
}

##------------------------------------------------------------------------
## generate simulation data
##------------------------------------------------------------------------

# iterations <- 10
# design <- design_matrix(m=3, n=6, treat_times=2:4)
# beta <- c(0, 1.2, 0, 0)
# sigma_sq <- 0.5
# phi <- 0.4
# Tau <- (1 - sigma_sq) * diag(c(1,1,0,0))

simulation_data <- function(iterations, design, beta, sigma_sq, phi, Tau) {
  N <- dim(design)[1]
  block <- design[,1]
  V_mat <- lmeAR1_cov_block(block=block, Z_design=design[,-1], 
                theta = list(sigma_sq=sigma_sq, phi=phi, Tau=Tau))
  V_chol <- lapply(V_mat, chol)  
  mean_vector <- as.matrix(design[,-1]) %*% beta
  errors <- t(prod_matrixblock(matrix(rnorm(iterations * N), iterations, N), V_chol, block))
  matrix(mean_vector, N, iterations) + errors
}


##------------------------------------------------------------------------
## estimate model, calculate effect size statistics 
##------------------------------------------------------------------------

random_formula <- function(random_terms)
  as.formula(paste("~ 0 + ",paste(random_terms, collapse=" + "),"| id"))

lme_fit <- function(y, design, fixed_terms, random_terms, method="REML") {
  lme(fixed = as.formula(paste("outcome ~ 0 + ", paste(fixed_terms, collapse=" + "))), 
      random = random_formula(random_terms), 
      correlation = corAR1(0, ~ trend | id),
      data = cbind(design, outcome = y),
      method = method, keep.data = FALSE,
      control=lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE))
}


##------------------------------------------------------------------------
## Compare RML to HPS for model MB1
##------------------------------------------------------------------------

#' @title Run simulation comparing REML and HPS estimates
#' 
#' @description Simulates data from a simple linear mixed effects model, then calculates 
#' REML and HPS effect size estimators as described in Pustejovsky, Hedges, & Shadish (2013).
#' 
#' @param iterations number of independent iterations of the simulation
#' @param beta vector of fixed effect parameters
#' @param rho intra-class correlation parameter
#' @param phi autocorrelation parameter 
#' @param design design matrix. If not specified, it will be calculated based on \code{m}, \code{n}, and \code{MB}.
#' @param m number of cases. Not used if \code{design} is specified.
#' @param n number of measurement occasions. Not used if \code{design} is specified.
#' @param MB If true, a multiple baseline design will be used; otherwise, an AB design will be used. Not used if \code{design} is specified.
#'   
#' @export 
#' 
#' @return A matrix reporting the mean and variance of the effect size estimates
#' and various associated statistics.
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
#' 
#' @examples
#' compare_RML_HPS(iterations=10, beta = c(0,1,0,0), rho = 0.3, phi = 0.5, design=design_matrix(m=3,n=8))

compare_RML_HPS <- function(iterations, beta, rho, phi, design, m, n, MB = TRUE) {
  if (missing(design)) {
    treat_times <- if (MB) MBTreatTimes(m=m,n=n) else rep(n / 2 + 1, m)
    design <- design_matrix(m=m, n=n, treat_times = treat_times)
  }
    
  fixed_terms <- c("constant","treatment")
  random_terms <- c("constant")
  p_const <- c(0,1)
  r_const <- c(1,0,1)
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = diag(c(rho,0,0,0)))
  
  RML <- apply(y_sims, 2, function(y) 
    g_REML(lme_fit(y, design, fixed_terms, random_terms),
           p_const, r_const,
           X_design=as.matrix(design[,fixed_terms]), 
           Z_design=as.matrix(design[,random_terms]),
           block=design$id, times=NULL, returnModel=FALSE)[-14:-12])
  RML_mat <- matrix(unlist(RML), length(unlist(RML[[1]])), iterations, 
                    dimnames = list(names(unlist(RML[[1]]))))
  rho <- RML_mat["Tau.id.var(constant)",] / (RML_mat["Tau.id.var(constant)",] + RML_mat["sigma_sq",])
  HPS <- HPS_effect_size(y_sims, treatment=design$treatment, id=design$id, time=design$trend)
  
  RML_coverage1 <- coverage(delta = beta[2], CI = CI_SMD(delta = RML_mat["delta_AB",], kappa = RML_mat["kappa",], nu = RML_mat["nu",]))
  RML_coverage2 <- abs(RML_mat["g_AB",] - beta[2]) / sqrt(RML_mat["V_g_AB",]) < qt(0.975, df = RML_mat["nu",])
  HPS_coverage1 <- coverage(delta = beta[2], CI = CI_SMD(delta = HPS["delta_hat_unadj",], kappa = HPS["kappa",], nu = HPS["df",]))
  HPS_coverage2 <- abs(HPS["delta_hat",] - beta[2]) / sqrt(HPS["V_delta_hat",]) < qt(0.975, df = HPS["df",])
  estimates <- rbind(RML_mat, rho, RML_coverage1, RML_coverage2, HPS, HPS_coverage1, HPS_coverage2)
  rbind(cbind(mean = rowMeans(estimates), var = apply(estimates, 1, var)), 
       cov = c(NA, cov(estimates["g_AB",], estimates["delta_hat",])))
}



##------------------------------------------------------------------------
## Simulate model MB2
##------------------------------------------------------------------------

convergence_handler_MB2 <- function(design, y, method="REML") {
  fixed_terms <- c("constant","treatment")
  p_const <- c(0,1)
  W <- NULL
  m_full <- withCallingHandlers(
    tryCatch(
      lme_fit(y, design, fixed_terms=fixed_terms, 
              random_terms=c("constant","treatment"), method=method),
      error = function(e) e),
    warning = function(w) W <<- w)
  attr(m_full, "warning") <- W
  
  if (!inherits(m_full,"error")) {
    g <- g_REML(m_full, p_const=p_const, r_const=c(1,0,1,0,0),
                X_design=as.matrix(design[,fixed_terms]), 
                Z_design=as.matrix(design[,c("constant","treatment")]),
                block=design$id, times=NULL, returnModel=FALSE)[-14:-12]
  } else {
    m_reduced <- lme_fit(y, design, fixed_terms=fixed_terms, 
                         random_terms="constant", method=method)
    attr(m_reduced,"warning") <- m_full
    g <- g_REML(m_reduced, p_const=p_const, r_const=c(1,0,1),
                  X_design=as.matrix(design[,fixed_terms]), 
                  Z_design=as.matrix(design[,"constant"]),
                  block=design$id, times=NULL, returnModel=FALSE)[-14:-12]
    g$Tau <- c(g$Tau, "id.cov(treatment,constant)" = 0, "id.var(treatment)" = 0)
  }
  g
}

#' @title Simulate Model MB2 from Pustejovsky (2013)
#' 
#' @description Simulates data from a linear mixed effects model, then calculates 
#' REML effect size estimator as described in Pustejovsky (2013).
#' 
#' @param iterations number of independent iterations of the simulation
#' @param beta vector of fixed effect parameters
#' @param rho intra-class correlation parameter
#' @param phi autocorrelation parameter 
#' @param tau1_ratio ratio of treatment effect variance to intercept variance 
#' @param tau_corr correlation between case-specific treatment effects and intercepts  
#' @param design design matrix. If not specified, it will be calculated based on \code{m}, \code{n}, and \code{MB}.
#' @param m number of cases. Not used if \code{design} is specified.
#' @param n number of measurement occasions. Not used if \code{design} is specified.
#' @param MB If true, a multiple baseline design will be used; otherwise, an AB design will be used. Not used if \code{design} is specified.
#'   
#' @export 
#' 
#' @return A matrix reporting the mean and variance of the effect size estimates
#' and various associated statistics.
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
#' 
#' @examples
#' set.seed(8)
#' simulate_MB2(iterations = 10, beta = c(0,1,0,0), rho = 0.4, phi = 0.5, tau1_ratio = 0.5, tau_corr = -0.4, design = design_matrix(m=3, n=8))
#' set.seed(8)
#' simulate_MB2(iterations = 10, beta = c(0,1,0,0), rho = 0.4, phi = 0.5, tau1_ratio = 0.5, tau_corr = -0.4, m = 3, n = 8, MB = FALSE)


simulate_MB2 <- function(iterations, beta, rho, phi, tau1_ratio, tau_corr, design, m, n, MB = TRUE) {

  if (missing(design)) {
    treat_times <- if (MB) MBTreatTimes(m=m,n=n) else rep(n / 2 + 1, m)
    design <- design_matrix(m=m, n=n, treat_times = treat_times)
  }
  
  Tau <- rbind(cbind(
    rho * matrix(c(1,rep(tau_corr * sqrt(tau1_ratio),2),tau1_ratio), 2, 2), 
    matrix(0,2,2)), matrix(0,2,4))
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = Tau)
  RML <- apply(y_sims, 2, convergence_handler_MB2, design=design)
  RML_mat <- matrix(unlist(RML), length(unlist(RML[[1]])), iterations, 
                    dimnames = list(names(unlist(RML[[1]]))))
  RML_coverage1 <- coverage(delta = beta[2], CI = CI_SMD(delta = RML_mat["delta_AB",], kappa = RML_mat["kappa",], nu = RML_mat["nu",]))
  RML_coverage2 <- abs(RML_mat["g_AB",] - beta[2]) / sqrt(RML_mat["V_g_AB",]) < qt(0.975, df = RML_mat["nu",])
  estimates <- rbind(RML_mat, RML_coverage1, RML_coverage2)
  
  cbind(mean = rowMeans(estimates), var = apply(estimates, 1, var))
}


##------------------------------------------------------------------------
## Simulate model MB4 
##------------------------------------------------------------------------

convergence_handler_MB4 <- function(design, y, p_const, r_const) {
  fixed_terms <- c("constant","treatment","trend","interaction")
  W <- NULL
  m_full <- withCallingHandlers(
    tryCatch(
      lme_fit(y, design, fixed_terms=fixed_terms, random_terms=c("constant","trend")),
      error = function(e) e),
    warning = function(w) W <<- w)
  attr(m_full, "warning") <- W
  
  if (!inherits(m_full,"error")) {
    g <- g_REML(m_full, p_const=p_const, r_const=r_const,
                X_design=as.matrix(design[,fixed_terms]), 
                Z_design=as.matrix(design[,c("constant","trend")]),
                block=design$id, times=NULL, returnModel=FALSE)[-14:-12]
  } else {
    m_reduced <- lme_fit(y, design, fixed_terms=fixed_terms, random_terms="constant")
    attr(m_reduced,"warning") <- m_full
    g <- g_REML(m_reduced, p_const=p_const, r_const=c(1,0,1),
                X_design=as.matrix(design[,fixed_terms]), 
                Z_design=as.matrix(design[,"constant"]),
                block=design$id, times=NULL, returnModel=FALSE)[-14:-12]
    g$Tau <- c(g$Tau, "id.cov(trend,constant)" = 0, "id.var(trend)" = 0)
  }
  g
}

#' @title Simulate Model MB4 from Pustejovsky (2013)
#' 
#' @description Simulates data from a linear mixed effects model, then calculates 
#' REML effect size estimator as described in Pustejovsky (2013).
#' 
#' @param iterations number of independent iterations of the simulation
#' @param beta vector of fixed effect parameters
#' @param rho intra-class correlation parameter
#' @param phi autocorrelation parameter 
#' @param tau2_ratio ratio of trend variance to intercept variance 
#' @param tau_corr correlation between case-specific trends and intercepts
#' @param p_const vector of constants for calculating numerator of effect size
#' @param r_const vector of constants for calculating denominator of effect size
#' @param design design matrix. If not specified, it will be calculated based on \code{m}, \code{n}, and \code{MB}.
#' @param m number of cases. Not used if \code{design} is specified.
#' @param n number of measurement occasions. Not used if \code{design} is specified.
#' @param MB If true, a multiple baseline design will be used; otherwise, an AB design will be used. Not used if \code{design} is specified.
#'   
#' @export 
#' 
#' @return A matrix reporting the mean and variance of the effect size estimates
#' and various associated statistics.
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
#' 
#' @examples
#' simulate_MB4(iterations = 10, beta = c(0,1,0,0), rho = 0.8, phi = 0.5, tau2_ratio = 0.5, tau_corr = 0, p_const = c(0,1,0,7), r_const = c(1,0,1,0,0), design = design_matrix(3, 16, treat_times=c(5,9,13), center = 12))
#' simulate_MB4(iterations = 10, beta = c(0,1,0,0), rho = 0.8, phi = 0.5, tau2_ratio = 0.5, tau_corr = 0, m = 6, n = 8)

simulate_MB4 <- function(iterations, beta, rho, phi, tau2_ratio, tau_corr, 
                         p_const, r_const, design, m, n, MB = TRUE) {

  if (missing(design)) {
    if (missing(p_const)) p_const <- c(0,1,0, n/4)
    if (missing(r_const)) r_const <- c(1,0,1,0,0)
    treat_times <- if (MB) MBTreatTimes(m=m,n=n) else rep(n / 2 + 1, m)
    design <- design_matrix(m=m, n=n, treat_times = treat_times, center = 3*n/4)
  }
  
  Tau <- rbind(cbind(
    rho * matrix(c(1,rep(tau_corr * sqrt(tau2_ratio),2),tau2_ratio), 2, 2), 
    matrix(0,2,2)), matrix(0,2,4))[c(1,3,2,4),c(1,3,2,4)]
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = Tau)
  RML <- apply(y_sims, 2, convergence_handler_MB4, 
               design=design, p_const=p_const, r_const=r_const)
  RML_mat <- matrix(unlist(RML), length(unlist(RML[[1]])), iterations, 
                    dimnames = list(names(unlist(RML[[1]]))))
  RML_coverage1 <- coverage(delta = sum(beta * p_const), CI = CI_SMD(delta = RML_mat["delta_AB",], kappa = RML_mat["kappa",], nu = RML_mat["nu",]))
  RML_coverage2 <- abs(RML_mat["g_AB",] - sum(beta * p_const)) / sqrt(RML_mat["V_g_AB",]) < qt(0.975, df = RML_mat["nu",])
  estimates <- rbind(RML_mat, RML_coverage1, RML_coverage2)
  
  cbind(mean = rowMeans(estimates), var = apply(estimates, 1, var))
}

#------------------------------------------
# Simulate from fitted g_REML model 
#------------------------------------------

fit_g <- function(y, object) {
  W <- NULL
  m_fit <- withCallingHandlers(
    tryCatch(
      m_fit <- update(object, fixed = update(as.formula(object$call$fixed), y ~ .), 
                      data = data.frame(y=y, object$data)),
      error = function(e) e),
    warning = function(w) W <<- w)
  attr(m_fit, "warning") <- W
  
  if (!inherits(m_fit,"error")) {
    unlist(g_REML(m_fit, p_const=object$p_const, object$r_const, 
                  X_design = model.matrix(object, data = object$data), 
                  Z_design = model.matrix(object$modelStruct$reStruct, data = object$data), 
                  block = object$groups[[1]], 
                  times = attr(object$modelStruct$corStruct, "covariate"), 
                  returnModel=FALSE)[-14:-12])    
  } else {
    rep(NA, length(unlist(object[1:11])))
  }
}

#' @title Simulate data from a fitted \code{g_REML} object
#' 
#' @description Simulates data from the linear mixed effects model used to estimate the
#' specified standardized mean difference effect size. 
#' Suitable for parametric bootstrapping.
#' 
#' @param object a \code{g_REML} object
#' @param nsim number of models to simulate
#' @param seed seed value. See documentation for \code{\link{simulate}}
#' @param parallel if \code{TRUE}, run in parallel using foreach backend.
#' @param ... additional optional arguments
#' 
#' @return A matrix with one row per simulation, with columns corresponding to the output
#' of \code{g_REML}.
#' 
#' @examples
#' data(Laski)
#' Laski_RML <- lme(fixed = outcome ~ treatment, random = ~ 1 | case, correlation = corAR1(0, ~ time | case), data = Laski)
#' Laski_g <- g_REML(Laski_RML, p_const = c(0,1), r_const = c(1,0,1))
#' simulate(Laski_g, nsim = 20)
#' 
#' 
#' @S3method simulate g_REML


simulate.g_REML <- function(object, nsim = 1, seed = NULL, parallel = FALSE, ...) {
  require(plyr)
  set.seed(seed)
  
  X_design <- model.matrix(object, data = object$data)
  Z_design <- model.matrix(object$modelStruct$reStruct, data = object$data)
  block <- object$groups[[1]]
  
  nXterms <- dim(X_design)[2]
  Zterms <- match(colnames(Z_design), colnames(X_design))
  Tau <- matrix(0, nXterms, nXterms)
  Tau[Zterms,Zterms] <- Tau_matrix(object$Tau)
  
  sim_data <- simulation_data(iterations = nsim, 
                              design = cbind(block,X_design), 
                              beta = object$coefficients$fixed, 
                              sigma_sq = object$sigma_sq, phi = object$phi, Tau = Tau)

  if (parallel) paropts <- list(.packages = c("nlme","scdhlm")) else paropts = NULL

  as.data.frame(aaply(sim_data, 2, fit_g, object = object, .parallel = parallel, .paropts = paropts))

}