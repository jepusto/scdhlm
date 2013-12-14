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

#iterations <- 10
#design <- design_matrix(m=3, n=6, treat_times=2:4)
#beta <- c(0, 1.2, 0, 0)
#sigma_sq <- 0.5
#phi <- 0.4
#Tau <- (1 - sigma_sq) * diag(c(1,1,0,0))

simulation_data <- function(iterations, design, beta, sigma_sq, phi, Tau) {
  N <- dim(design)[1]
  V_chol <- lapply(LME_cov_block(block=design$id, Z_design=design[,2:5], sigma_sq=sigma_sq, phi=phi, Tau=Tau), chol)  
  mean_vector <- as.matrix(design[,2:5]) %*% beta
  errors <- t(prod_matrixblock(matrix(rnorm(iterations * N), iterations, N), V_chol, design$id))
  matrix(mean_vector, N, iterations) + errors
}


##------------------------------------------------------------------------
## estimate model, calculate effect size statistics 
##------------------------------------------------------------------------

require(nlme)

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
## Compare RML to HPS for models MB1 and TR1 ##
##------------------------------------------------------------------------
# iterations <- 5000
# design <- design_matrix(m=3, n=6, treat_times=4)
# beta <- c(0,1,0,0)
# rho <- 0.8
# phi <- 0.5


compare_RML_HPS <- function(iterations, design, beta, rho, phi) {
  
  fixed_terms <- c("constant","treatment")
  random_terms <- c("constant")
  p_const <- c(0,1)
  r_const <- c(1,0,1)
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = diag(c(rho,0,0,0)))
  RML <- apply(y_sims, 2, function(y) 
    g_AB(lme_fit(y, design, fixed_terms, random_terms), 
         design, fixed_terms, random_terms, p_const, r_const))
  HPS <- HPS_effect_size(y_sims, treatment=design$treatment, id=design$id, time=design$trend)
  
  estimates <- rbind(RML, HPS)
  list(means = rowMeans(estimates), var = apply(estimates, 1, var), 
       cov = cor(t(estimates[c("g_star","g_AB","delta_hat"),]))[lower.tri(diag(3))])
}

#compare_RML_HPS(50, design, beta, rho, phi)



##------------------------------------------------------------------------
## Simulate model TR2
##------------------------------------------------------------------------

# iterations <- 1000
# design <- design_matrix(m=3, n=8, treat_times=5)
# beta <- c(0,1,0,0)
# rho <- 0
# phi <- 0.5
# tau1_ratio <- 0.5
# tau_corr <- -0.4

convergence_handler_TR2 <- function(design, y, method="REML") {
  fixed_terms <- c("constant","treatment")
  p_const <- c(0,1)
  W <- NULL
  m_full <- withCallingHandlers(
    tryCatch(
      lme_fit(y, design, fixed_terms=fixed_terms, random_terms=c("constant","treatment"), method=method),
      error = function(e) e),
    warning = function(w) W <<- w)
  attr(m_full, "warning") <- W
  
  if (!inherits(m_full,"error")) {
    g <- g_AB(m_full, design, 
              fixed_terms=fixed_terms, random_terms=c("constant","treatment"), 
              p_const=p_const, r_const=c(1,0,1,0,0))
  } else {
    m_reduced <- lme_fit(y, design, fixed_terms=fixed_terms, random_terms="constant", method=method)
    attr(m_reduced,"warning") <- m_full
    g <- c(g_AB(m_reduced, design, 
                fixed_terms=fixed_terms, random_terms="constant", 
                p_const=p_const, r_const=c(1,0,1)),
           "id.cov(treatment,constant)" = 0,
           "id.var(treatment)" = 0)
  }
  g
}

simulate_TR2 <- function(iterations, design, beta, rho, phi, tau1_ratio, tau_corr) {
  
  Tau <- rbind(cbind(
    rho * matrix(c(1,rep(tau_corr * sqrt(tau1_ratio),2),tau1_ratio), 2, 2), 
    matrix(0,2,2)), matrix(0,2,4))
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = Tau)
  RML <- apply(y_sims, 2, convergence_handler_TR2, design=design)
  tau_corr_est <- RML[15,] / sqrt(RML[14,] * RML[16,])
  
  list(means = rowMeans(RML), var = apply(RML, 1, var), cov = cor(RML["g_star",],RML["g_AB",]), 
       tau_corr=table(tau_corr_est), errors=table(RML[16,]==0))
}



##------------------------------------------------------------------------
## Simulate model MB4 
##------------------------------------------------------------------------

convergence_handler_MB4 <- function(design, y, p_const) {
  fixed_terms <- c("constant","treatment","trend","interaction")
  W <- NULL
  m_full <- withCallingHandlers(
    tryCatch(
      lme_fit(y, design, fixed_terms=fixed_terms, random_terms=c("constant","trend")),
      error = function(e) e),
    warning = function(w) W <<- w)
  attr(m_full, "warning") <- W
  
  if (!inherits(m_full,"error")) {
    g <- g_AB(m_full, design, 
              fixed_terms=fixed_terms, random_terms=c("constant","trend"), 
              p_const=p_const, r_const=c(1,0,1,0,0))
  } else {
    m_reduced <- lme_fit(y, design, fixed_terms=fixed_terms, random_terms="constant")
    attr(m_reduced,"warning") <- m_full
    g <- c(g_AB(m_reduced, design, 
                fixed_terms=fixed_terms, random_terms="constant", 
                p_const=p_const, r_const=c(1,0,1)),
           "id.cov(trend,constant)" = 0,
           "id.var(trend)" = 0)
  }
  g
}

simulate_MB4 <- function(iterations, design, beta, rho, phi, tau2_ratio, tau_corr, p_const) {
  
  Tau <- rbind(cbind(
    rho * matrix(c(1,rep(tau_corr * sqrt(tau2_ratio),2),tau2_ratio), 2, 2), 
    matrix(0,2,2)), matrix(0,2,4))[c(1,3,2,4),c(1,3,2,4)]
  
  y_sims <- simulation_data(iterations, design, beta, sigma_sq = 1 - rho, phi, Tau = Tau)
  RML <- apply(y_sims, 2, convergence_handler_MB4, design=design, p_const=p_const)
  tau_corr_est <- RML[15,] / sqrt(RML[14,] * RML[16,])
  
  list(means = rowMeans(RML), var = apply(RML, 1, var), cov = cor(RML["g_star",],RML["g_AB",]), 
       tau_corr=table(tau_corr_est), errors=table(RML[16,]==0))
}

# iterations <- 1000
# m <- 3
# n <- 16
# design <- design_matrix(m, n, treat_times=MBTreatTimes(m,n), center = 3 * n / 4)
# beta <- c(0,1,0,0)
# rho <- 0.8
# phi <- 0.5
# tau2_ratio <- 0.5
# tau_corr <- 0
# 
# MB4 <- simulate_MB4(iterations, design, beta, rho, phi, tau2_ratio, tau_corr)
# tau_corr <- rep(as.double(names(MB4$tau_corr)),MB4$tau_corr)
# hist(tau_corr)
# table(round(tau_corr,2))
# plot(density(tau_corr))
# MB4$errors


