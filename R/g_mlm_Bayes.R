# calculate mlm effect size using Bayesian estimation methods
#' @title Calculates a between-case standardized mean difference effect size estimate
#'
#' @description Estimates a standardized mean difference effect size from a
#'   multi-level model estimated using \code{brms::brm}.
#'
#' @param mod Fitted model of class brmsfit (estimated using
#'   \code{brms::brm()}), from which to estimate the effect size.
#' @param p_const Vector of constants for calculating numerator of effect size.
#'   Must be the same length as fixed effects in \code{mod}.
#' @param r_const Vector of constants for calculating denominator of effect
#'   size. Must be the same length as the number of variance component
#'   parameters in \code{mod_denom}.
#' @param rconst_base_var_index Something, not really sure what.
#' @param cover Confidence level.
#'
#' @export
#'
#' @return A list with the following components 
#' \tabular{ll}{ 
#'  \code{g_AB} \tab Posterior mean effect size estimate \cr
#'  \code{SE_g_AB} \tab Approximate standard error of mean effect size estimate \cr
#'  \code{nu} \tab Estimated denominator degrees of freedom \cr
#'  \code{CI_L} \tab Lower bound of credible interval for effect size \cr
#'  \code{CI_U} \tab Upper bound of credible interval for effect size \cr
#'  \code{es_num_vec} \tab Posterior samples of effect size numerator \cr
#'  \code{es_denom_vec} \tab Posterior samples of squared denominator of effect size \cr
#'  \code{autocor_param} \tab Posterior mean auto-correlation \cr
#'  \code{var_param} \tab Posterior mean of level-1 variance model parameter \cr
#'  \code{rho} \tab Posterior mean intra-class correlation \cr
#' }
#'
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
#'   Design-comparable effect sizes in multiple baseline designs: A general
#'   modeling framework. \emph{Journal of Educational and Behavioral Statistics,
#'   39}(4), 211-227. \doi{10.3102/1076998614547577}
#'

g_mlm_Bayes <- function(mod, p_const, r_const, rconst_base_var_index = 1, cover = 95) {
  
  param_names <- brms::variables(mod)
  
  # calculate the numerator of BCSMD
  
  posterior_samples_fixed <- brms::as_draws_matrix(mod, variable = "^b_", regex = TRUE)
  
  if ("b_sigma_Intercept" %in% param_names) {
    samples_fixed <- posterior_samples_fixed[,!startsWith(colnames(posterior_samples_fixed), "b_sigma_")]
    sigma_sq <- exp(2*(posterior_samples_fixed[,"b_sigma_Intercept"]))
  } else {
    samples_fixed <- posterior_samples_fixed 
    sigma <- brms::as_draws_matrix(mod, variable = "sigma", regex = TRUE)
    sigma_sq <- sigma^2
  }
  
  es_num_vec <- apply(samples_fixed, 1, function(x) sum(x * p_const))
  
  # calculate the denominator of BCSMD
  
  samples_r_sd <- brms::as_draws_matrix(mod, variable = "^sd_", regex = TRUE)
  samples_r_var <- samples_r_sd^2
  
  if (sum(grepl("^cor_",param_names)) > 0) {
    samples_r_cor <- brms::as_draws_matrix(mod, variable = "^cor_", regex = TRUE)
    cor_names_split <- strsplit(colnames(samples_r_cor), split = "__")
    cor_sd_suf <- lapply(cor_names_split, function(x) x[-1])
    cor_sd_pre <- lapply(cor_names_split, function(x) paste0("sd_", gsub(".*\\_", "", x[[1]])))
    cor_sd_names <- mapply(function(x,y)  paste0(x, "__", y), cor_sd_pre, cor_sd_suf, SIMPLIFY = FALSE)
    sd_prod <- sapply(cor_sd_names, function(x) samples_r_sd[,x[1]] * samples_r_sd[,x[2]])
    samples_r_cov <- samples_r_cor * sd_prod
  } else {
    samples_r_cov <- NULL
  }
  
  samples_r_varcov <- cbind(samples_r_var, samples_r_cov, sigma_sq)
  
  es_denom_sq <- apply(samples_r_varcov, 1, function(x) sum(x * r_const))
  es_denom_vec <- sqrt(es_denom_sq)
  
  # calculate BCSMD
  
  es_vec <- es_num_vec / es_denom_vec
  
  # calculate rho
  
  if (rconst_base_var_index == 1) {
    rho <- mean(samples_r_varcov[,1] / (samples_r_varcov[,1] + samples_r_varcov[,ncol(samples_r_varcov)]))
  } else {
    rho_level2 <- mean((samples_r_varcov[,1] + samples_r_varcov[,rconst_base_var_index]) / 
                         (samples_r_varcov[,1] + samples_r_varcov[,rconst_base_var_index] + 
                            samples_r_varcov[,ncol(samples_r_varcov)]))
    rho_level2 <- round(rho_level2, 4)
    
    rho_level3 <- mean(samples_r_varcov[,1] /
                         (samples_r_varcov[,1] + samples_r_varcov[,rconst_base_var_index] + 
                            samples_r_varcov[,ncol(samples_r_varcov)]))
    rho_level3 <- round(rho_level3, 4)
    
    rho <- paste0("Level2: ", rho_level2, " Level3: ", rho_level3)
  }
  
  # get the corStruct and varStruct param
  
  if (sum(grepl("^ar",param_names)) > 0) {
    autocor_draw <- brms::as_draws_matrix(mod, variable = "^ar", regex = TRUE)
    autocor_param <- mean(autocor_draw)
  } else if (sum(grepl("^ma",param_names)) > 0) {
    autocor_draw <- brms::as_draws_matrix(mod, variable = "^ma", regex = TRUE)
    autocor_param <- mean(autocor_draw)
  } else {
    autocor_param <- NA_real_
  }
  
  if (sum(grepl("^b_sigma_",param_names)) > 0) {
    var_param_name <- setdiff(param_names[startsWith(param_names, "b_sigma_")], "b_sigma_Intercept")
    var_param_draw <- brms::as_draws_matrix(mod, variable = var_param_name, regex = TRUE)
    var_param <- exp(mean(var_param_draw))
  } else {
    var_param <- NA_real_
  }
  
  
  g <- mean(es_vec)
  SE_g <- sd(es_vec)
  nu <- 2 * (mean(es_denom_vec))^2 / var(es_denom_vec)
  CI_L <- quantile(es_vec, (1 - cover / 100) / 2)
  CI_U <- quantile(es_vec, (1 + cover / 100) / 2)
  
  res <- list(
    g_AB = g, SE_g_AB = SE_g, 
    CI_L = CI_L, CI_U = CI_U,
    nu = nu, 
    es_num_vec = es_num_vec, 
    es_denom_vec = es_denom_vec, 
    phi = autocor_param, 
    var_param = var_param, 
    rho = rho,
    CI_cover = cover,
    converged = NA
  )
  
  class(res) <- c("Bayes-bcsmd", "bcsmd")
  
  return(res)
  
}

