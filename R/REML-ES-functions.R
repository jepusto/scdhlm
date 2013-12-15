##------------------------------------------------------------------------------
## Create AR(1) correlation and inverse correlation matrices
##------------------------------------------------------------------------------

AR1_corr <- function(phi, times) phi^as.matrix(dist(times))


AR1_corr_block <- function(phi, block, times=NULL) {
  if (is.null(times)) times <- lapply(table(block), seq, from=1)
  lapply(times, AR1_corr, phi=phi)
}


AR1_corr_inv <- function(phi, n) 
  diag((rep(1,n) + c(0,rep(phi^2,n-2),0))/(1 - phi^2)) + 
      rbind(cbind(rep(0,n-1), diag(rep(-phi/(1 - phi^2),n-1))), rep(0,n)) + 
      rbind(rep(0,n), cbind(diag(rep(-phi/(1 - phi^2),n-1)),rep(0,n-1)))



##------------------------------------------------------------------------------
## Create lme covariance and inverse covariance matrices
##------------------------------------------------------------------------------

# create covariance matrix from Tau parameters 

Tau_matrix <- function(Tau_coef) {
  q <- (sqrt(1 + 8 * length(Tau_coef)) - 1) / 2
  Tau_mat <- matrix(NA,q,q)
  Tau_mat[upper.tri(Tau_mat, diag=TRUE)] <- Tau_coef
  Tau_mat[lower.tri(Tau_mat)] <- t(Tau_mat)[lower.tri(Tau_mat)]
  return(Tau_mat)
}


# create block-diagonal covariance matrix with AR(1) level-1 error

lmeAR1_cov_block <- function(block, Z_design, theta, times = NULL) {
  if(is.matrix(theta$Tau)) {
    Tau_mat <- theta$Tau 
    } else {
      Tau_mat <- Tau_matrix(theta$Tau)
    }
  ZTauZ <- by(Z_design, block, function(z) {
            z_mat <- as.matrix(z)
            z_mat %*% Tau_mat %*% t(z_mat)})
  AR1_mat <- AR1_corr_block(phi=theta$phi, block=block, times=times)
  mapply(function(a,b) a + theta$sigma_sq * b, ZTauZ, AR1_mat, SIMPLIFY = FALSE)
}


# create inverse covariance matrix for a single block

# Note: This function assumes that either 
# a) the Tau argument is passed as the eigen-decomposition of Tau
# including an indicator vector for which dimensions to use or 
# b) Tau is an invertible matrix.
# Option (a) must be used if Tau is of less than full rank.
# what about 1-dimensional Tau with tau_0^2 = 0?

lmeAR1_cov_inv  <- function(Z_design, sigma_sq, phi, Tau, times=NULL) {
  if (is.null(times)) {
    n <- ifelse(is.vector(Z_design), length(Z_design), dim(Z_design)[1])
    A_inv <- AR1_corr_inv(phi, n) / sigma_sq
  } else {
    A_inv <- solve(AR1_corr(phi, times)) / sigma_sq
  }

  if (class(Tau)=="list") {
    if (sum(Tau$use) == 0) return(A_inv) else {
      # use eigen-decomposition of Tau if available
      Z_mat <- as.matrix(Z_design) %*% Tau$vectors[,Tau$use]
      Tau_inv <- diag(1/Tau$values[Tau$use], nrow=sum(Tau$use))
    }
  } else {
    # otherwise assume that Tau is invertible
    Z_mat <- as.matrix(Z_design)
    Tau_inv <- chol2inv(chol(Tau))
  }
  A_inv_Z <- A_inv %*% Z_mat
  A_inv - A_inv_Z %*% chol2inv(chol(Tau_inv + t(Z_mat) %*% A_inv_Z)) %*% t(A_inv_Z)  
}


# create block-diagonal inverse covariance matrix

lmeAR1_cov_block_inv <- function(block, Z_design, theta, times=NULL) {
  Tau_eigen <- eigen(Tau_matrix(theta$Tau), symmetric=TRUE)
  Tau_eigen$use <- round(Tau_eigen$values,14) > 0
  if (is.null(times)) {
    by(Z_design, block, lmeAR1_cov_inv, sigma_sq=theta$sigma_sq, phi=theta$phi, Tau=Tau_eigen)
  } else {
    Z_list <- by(Z_design, block, function(x) x)
    mapply(lmeAR1_cov_inv, Z_design = Z_list, times = times, 
           MoreArgs = list(sigma_sq=theta$sigma_sq, phi=theta$phi, Tau=Tau_eigen))
  }
}


##------------------------------------------------------------------------------
## create Q matrix
##------------------------------------------------------------------------------

Q_matrix <- function(block, X_design, Z_design, theta, times=NULL) {
  V_inv <- lmeAR1_cov_block_inv(block=block, Z_design=Z_design, theta=theta, times=times)
  Vinv_X <- prod_blockmatrix(V_inv, X_design, block = block)
  VinvX_XVXinv_XVinv <- Vinv_X %*% chol2inv(chol(t(X_design) %*% Vinv_X)) %*% t(Vinv_X)
  block_minus_matrix(V_inv, VinvX_XVXinv_XVinv, block)
}


##------------------------------------------------------------------------------
## Create first derivative matrices
##------------------------------------------------------------------------------

dV_dsigmasq <- function(block, times, phi)
  AR1_corr_block(phi=phi, block=block, times=times)
#dV_dsigmasq(block=rep(1:3, each = 4), times=NULL, phi=0.5)

dV_dphi <- function(block, times, phi, sigma_sq) {
  if (is.null(times)) times <- lapply(table(block), seq, from=1)
  time_diff <- lapply(times, function(x) as.matrix(dist(x)))
  lapply(time_diff, function(x) sigma_sq * x * phi^pmax(0, x - 1))
}

dV_dTau_index <- function(block, Z_design, tau_index)
  by(Z_design, block, function(Z) 
    as.matrix(Z)[,tau_index, drop=FALSE] %*% t(Z)[rev(tau_index),,drop=FALSE])

dV_dTau_unstruct <- function(block, Z_design) {
  Tau_q <- dim(Z_design)[2]
  tau_index <- cbind(unlist(sapply(1:Tau_q, function(x) seq(1,x))), 
                     unlist(sapply(1:Tau_q, function(x) rep(x,x))))
  apply(tau_index, 1, function(t) dV_dTau_index(block, Z_design, tau_index=unique(t)))
}

##------------------------------------------------------------------------------
## extract variance components
##------------------------------------------------------------------------------

extract_varcomp <- function(m_fit) {
  sigma_sq <- m_fit$sigma^2                                         # sigma^2
  phi <- as.double(coef(m_fit$modelStruct$corStruct, FALSE))        # phi
  Tau_coef <- coef(m_fit$modelStruct$reStruct, FALSE) * sigma_sq    # unique coefficients in Tau
  
  varcomp <- list(sigma_sq=sigma_sq, phi=phi, Tau = Tau_coef)
  class(varcomp) <- "varcomp"
  return(varcomp)
}


##------------------------------------------------------------------------------
## Expected Information Matrix 
##------------------------------------------------------------------------------

Info_Expected <- function(theta, X_design, Z_design, block, times=NULL) {
  
  Q_mat <- Q_matrix(block, X_design, Z_design, theta, times=times) 
  
  # create N * N * r array with QdV entries
  r <- length(unlist(theta))
  QdV <- array(NA, dim = c(dim(Q_mat),r))
  QdV[,,1] <- prod_matrixblock(Q_mat, dV_dsigmasq(block=block, times=times, phi=theta$phi), block=block)
  QdV[,,2] <- prod_matrixblock(Q_mat, dV_dphi(block=block, times=times, phi=theta$phi, sigma_sq=theta$sigma_sq), block=block)
  QdV[,,-2:-1] <- unlist(lapply(dV_dTau_unstruct(block, Z_design), prod_matrixblock, A=Q_mat, block=block))
  
  # calculate I_E
  I_E <- matrix(NA, r, r)
  for (i in 1:r)
    for (j in 1:i)
      I_E[i,j] <- product_trace(QdV[,,i], QdV[,,j]) / 2
  I_E[upper.tri(I_E)] <- t(I_E)[upper.tri(I_E)]
  
  return(I_E)
}

#' @title Calculate expected information matrix
#' 
#' @description Calculates the expected information matrix from a fitted linear mixed effects
#' model with AR(1) correlation structure in the level-1 errors.
#' 
#' @param m_fit Fitted model of class lme, with AR(1) correlation structure at level 1.
#' 
#' @export 
#' 
#' @return Expected Information matrix corresponding to variance components of \code{m_fit}.

Info_Expected_lmeAR1 <- function(m_fit) {
  theta <- extract_varcomp(m_fit)
  X_design <- model.matrix(m_fit, data = m_fit$data)
  Z_design <- model.matrix(m_fit$modelStruct$reStruct, data = m_fit$data)
  block <- nlme::getGroups(m_fit)
  times <- attr(m_fit$modelStruct$corStruct, "covariate")
  Info_Expected(theta=theta, X_design=X_design, Z_design=Z_design, block=block, times=times)
}



## estimate adjusted REML effect size (with associated estimates) for multiple baseline design ####

#' @title Calculates adjusted REML effect size
#' 
#' @description Estimates a design-comparable standardized mean difference effect size based on data 
#' from a multiple baseline design, using adjusted REML method as described in Pustejovsky, Hedges, 
#' & Shadish (2013). Note that the data must contain one row per measurement occasion per case.
#' 
#' @param m_fit Fitted model of class lme, with AR(1) correlation structure at level 1.
#' @param p_const Vector of constants for calculating numerator of effect size. 
#' Must be the same length as fixed effects in \code{m_fit}.
#' @param r_const Vector of constants for calculating denominator of effect size. 
#' Must be the same length as the number of variance component parameters in \code{m_fit}.
#' @param X_design (Optional) Design matrix for fixed effects. Will be extracted from \code{m_fit} if not specified.
#' @param Z_design (Optional) Design matrix for random effects. Will be extracted from \code{m_fit} if not specified.
#' @param block (Optional) Factor variable describing the blocking structure. Will be extracted from \code{m_fit} if not specified.
#' @param times (Optional) list of times used to describe AR(1) structure. Will be extracted from \code{m_fit} if not specified.
#' 
#' @export 
#' 
#' @return A list with the following components
#' \tabular{ll}{
#' \code{p_beta} \tab Numerator of effect size \cr
#' \code{r_theta} \tab Squared denominator of effect size \cr
#' \code{delta_AB} \tab Unadjusted (REML) effect size estimate \cr
#' \code{nu} \tab Estimated denominator degrees of freedom \cr
#' \code{g_AB} \tab Corrected effect size estimate \cr
#' \code{V_g_AB} \tab Approximate variance estimate \cr
#' \code{cnvg_warn} \tab Indicator that model did not converge \cr
#' \code{sigma_sq} \tab Estimated level-1 variance \cr
#' \code{phi} \tab Estimated autocorrelation \cr
#' \code{Tau} \tab Vector of level-2 variance components \cr
#' \code{I_E_inv} \tab Expected information matrix \cr
#' }
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
#' 

g_REML <- function(m_fit, p_const, r_const, 
                   X_design = model.matrix(m_fit, data = m_fit$data), 
                   Z_design = model.matrix(m_fit$modelStruct$reStruct, data = m_fit$data), 
                   block = nlme::getGroups(m_fit),
                   times = attr(m_fit$modelStruct$corStruct, "covariate")) {

  # basic model estimates
  p_beta <- sum(nlme::fixed.effects(m_fit) * p_const)               # p'Beta
  theta <- extract_varcomp(m_fit)                                   # full theta vector
  r_theta <- sum(unlist(theta) * r_const)                           # r'theta
  delta_AB <- p_beta / sqrt(r_theta)                                # delta_AB              
  kappa_sq <- (t(p_const) %*% vcov(m_fit) %*% p_const) / r_theta    # kappa^2
  cnvg_warn <- !is.null(attr(m_fit,"warning"))                      # indicator that RML estimation has not converged
      
  # calculate inverse expected information
  I_E <- Info_Expected(theta=theta, X_design=X_design, Z_design=Z_design, block=block, times=times)
  I_E_inv <- chol2inv(chol(I_E))
  

  nu <- 2 * r_theta^2 / (t(r_const) %*% I_E_inv %*% r_const)
  g_AB <- J(nu) * delta_AB
  nu_trunc <- max(nu, 2.001)
  V_g_AB <- J(nu)^2 * (nu_trunc * kappa_sq / (nu_trunc - 2) + g_AB^2 * (nu_trunc / (nu_trunc - 2) - 1 / J(nu_trunc)^2))
  
  return(c(list(p_beta=p_beta, r_theta=r_theta, delta_AB=delta_AB, nu=nu, 
                 g_AB=g_AB, V_g_AB=V_g_AB, cnvg_warn=cnvg_warn), theta, list(I_E_inv=I_E_inv)))
}


