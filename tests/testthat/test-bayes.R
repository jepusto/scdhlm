skip_if_not_installed("brms")
skip_if_not_installed("utils")

library(brms)
library(utils)

# Another way to calculate Bayesian estimates

prod_sd <- function(sd) {
  comb <- combn(sd, 2)
  prod_vec <- apply(comb, 2, prod)
  return(prod_vec)
}

calc_g_Bayes <- function(mod, design, 
                         FE_base = 0, RE_base = 0, RE_base_2 = NULL, 
                         FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                         corStruct = "AR1", varStruct = "hom",
                         A, B, center) {
  
  # p_const and bc_mat
  
  p_const <- c(rep(0L, length(FE_base)), (B - A)^as.integer(FE_trt))
  r_dim <- length(RE_base) + length(RE_trt)
  r_const_dim <- r_dim * (r_dim + 1) / 2
  bc_vec <- (B - center)^as.integer(RE_base)
  bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
  
  if (design %in% c("RMBB", "CMB")) {
    r_dim2 <- length(RE_base_2) + length(RE_trt_2)
    r_const_dim2 <- r_dim2 * (r_dim2 + 1) / 2
    bc_vec2 <- (B - center)^as.integer(RE_base_2)
    bc_mat2 <- 2 * tcrossprod(bc_vec2) - diag(bc_vec2^2)
  } 
  
  # r_const and variance components
  
  r_const_base_var <- diag(bc_mat)
  r_const_base_cor <- bc_mat[upper.tri(bc_mat, diag = FALSE)]
  
  if (design %in% c("RMBB", "CMB")) {
    r_const_base_var2 <- diag(bc_mat2)
    r_const_base_cor2 <- bc_mat2[upper.tri(bc_mat, diag = FALSE)]
  } 
  
  # calculate the numerator of BCSMD
  posterior_samples_fixed <- as_draws_matrix(mod, variable = "^b_", regex = TRUE)
  
  if (varStruct == "het") {
    samples_fixed <- posterior_samples_fixed[,!colnames(posterior_samples_fixed) %in% c("b_sigma_Intercept","b_sigma_trt")]
    sigma_sq <- exp(2*as.vector(posterior_samples_fixed[,"b_sigma_Intercept"]))
  } else {
    samples_fixed <- posterior_samples_fixed 
    sigma <- as.vector(as_draws_matrix(mod, variable = "sigma", regex = TRUE))
    sigma_sq <- sigma^2
  }
  
  fixed_pconst <- samples_fixed %*% diag(p_const)
  es_num_vec <- apply(fixed_pconst, 1, sum)
  
  # calculate the denominator of BCSMD
  samples_r_sd <- as_draws_matrix(mod, variable = "^sd_", regex = TRUE)
  
  if (design %in% c("RMBB", "CMB")) {
    
    # level 3
    r_base_sd2 <- samples_r_sd[, 1:length(RE_base_2)]
    r_base_var_mat2 <- r_base_sd2^2 %*% diag(r_const_base_var2)
    r_base_var_sum2 <- apply(r_base_var_mat2, 1, sum)
    
    if (length(RE_base_2) > 1) {
      samples_r_cor <- as_draws_matrix(mod, variable = "^cor_", regex = TRUE)
      r_base_cor2 <- samples_r_cor[, 1:length(r_const_base_cor2)]
      r_base_sd_prod2 <- do.call(rbind, apply(r_base_sd2, 1, prod_sd, simplify = FALSE))
      r_base_cov_sum2 <- as.vector((r_base_sd_prod2 * r_base_cor2) %*% as.matrix(r_const_base_cor2))
    } else {
      r_base_cov_sum2 <- 0
    }
    
    # level 2
    r_base_sd <- samples_r_sd[, (r_dim2 + 1):(r_dim2 + length(RE_base))]
    r_base_var_mat <- r_base_sd^2 %*% diag(r_const_base_var)
    r_base_var_sum <- apply(r_base_var_mat, 1, sum)
    
    if (length(r_const_base_cor) > 0) {
      r_cor_dim_Lvl3 <- r_const_dim2 - r_dim2
      samples_r_cor <- as_draws_matrix(mod, variable = "^cor_", regex = TRUE)
      r_base_cor <- samples_r_cor[, (r_cor_dim_Lvl3 + 1):(r_cor_dim_Lvl3 + length(r_const_base_cor))]
      r_base_sd_prod <- do.call(rbind, apply(r_base_sd, 1, prod_sd, simplify = FALSE))
      r_base_cov_sum <- as.vector((r_base_sd_prod * r_base_cor) %*% as.matrix(r_const_base_cor))
    } else {
      r_base_cov_sum <- 0
    }
    
    rho_level2 <- mean((as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2) / 
                         (as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2 + sigma_sq))
    rho_level2 <- round(rho_level2, 4)
    rho_level3 <- mean(as.vector(r_base_sd2[,1])^2 / 
                         (as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2 + sigma_sq))
    rho_level3 <- round(rho_level3, 4)
    rho <- paste0("Level2: ", rho_level2, " Level3: ", rho_level3)
    
  } else {
    
    r_base_sd <- samples_r_sd[, 1:length(RE_base)]
    r_base_var_mat <- r_base_sd^2 %*% diag(r_const_base_var)
    r_base_var_sum <- apply(r_base_var_mat, 1, sum)
    
    if (length(RE_base) > 1) {
      samples_r_cor <- as_draws_matrix(mod, variable = "^cor_", regex = TRUE)
      r_base_cor <- samples_r_cor[, 1:length(r_const_base_cor)]
      r_base_sd_prod <- do.call(rbind, apply(r_base_sd, 1, prod_sd, simplify = FALSE))
      r_base_cov_sum <- as.vector((r_base_sd_prod * r_base_cor) %*% as.matrix(r_const_base_cor))
    } else {
      r_base_cov_sum <- 0
    }
    
    r_base_var_sum2 <- r_base_cov_sum2 <- 0
    
    rho <- mean((as.vector(r_base_sd[,1])^2) / (as.vector(r_base_sd[,1])^2 + sigma_sq))
    
  }
  
  es_den_vec <- 
    sqrt(r_base_var_sum + r_base_cov_sum + # 2rd level 
           r_base_var_sum2 + r_base_cov_sum2 +  # 3rd level
           sigma_sq # residual variance
    )
  
  # calculate BCSMD
  es_vec <- es_num_vec / es_den_vec
  
  # get the corStruct and varStruct param
  if (corStruct == "AR1") {
    autocor_draw <- as_draws_matrix(mod, variable = "^ar", regex = TRUE)
    autocor_param <- mean(autocor_draw)
  } else if (corStruct == "MA1") {
    autocor_draw <- as_draws_matrix(mod, variable = "^ma", regex = TRUE)
    autocor_param <- mean(autocor_draw)
  } else {
    autocor_param <- NA_real_
  }
  
  if (varStruct == "het") {
    var_param_draw <- as_draws_matrix(mod, variable = "b_sigma_trt", regex = TRUE)
    var_param <- exp(mean(var_param_draw))
  } else {
    var_param <- NA_real_
  }
  
  g <- mean(es_vec)
  SE_g <- sd(es_vec)
  df <- 2 * (mean(es_den_vec))^2 / var(es_den_vec)
  CI_L <- quantile(es_vec, .025)
  CI_U <- quantile(es_vec, .975)
  
  res <- list(g_AB = g, SE_g_AB = SE_g, nu = df, CI_L = CI_L, CI_U = CI_U,
              es_num_vec = es_num_vec, es_denom_vec = es_den_vec, 
              autocor_param = autocor_param, var_param = var_param, rho = rho)
  
  return(res)
  
}


test_that("The Bayesian estimation works for MBP design.", {
  
  skip_on_cran()
  
  data(Laski)
  
  # simple model
  
  Laski_simple <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP",
                 case = case, phase = treatment,
                 session = time, outcome = outcome,
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 Bayesian = TRUE, 
                 chain = 2, iter = 400, 
                 summary = TRUE,
                 data = Laski)
    )
  
  expect_s3_class(Laski_simple, "data.frame")
  expect_true(is.na(Laski_simple$`Variance parameter`))

  
  # complex model
  # use brm() and g_mlm_Bayes()
  
  Laski_dat <- preprocess_SCD(design = "MBP",
                              case = case, phase = treatment,
                              session = time, outcome = outcome,
                              center = 4, data = Laski)
  
  Laski_comp_mod <- 
    suppressWarnings(
      brm(
        bf(outcome ~ 1 + time + trt + time_trt + (time | case) +
             arma(time = time, gr = case, p = 1, q = 0),
           center = FALSE),
        data = Laski_dat,
        chains = 2, iter = 400, thin = 10, cores = 1, 
        save_pars = save_pars(all = TRUE),
        seed = 43073051)
    )
  
  default_AB <- default_times(design = "MBP", 
                              case = case, phase = treatment, session = time, 
                              data = Laski)
  consts <- calc_consts(method = "Bayes", design = "MBP", center = 4,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = NULL,
                        FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "hom",
                        A = default_AB$A, B = default_AB$B)
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Laski_comp_mod, p_const = consts$p_const, r_const = consts$r_const)
  
  res_calc_g_Bayes <- calc_g_Bayes(mod = Laski_comp_mod, design = "MBP",
                                   FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = NULL,
                                   FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
                                   corStruct = "AR1", varStruct = "hom",
                                   A = default_AB$A, B = default_AB$B, center = 4)
  
  # compare g_mlm_Bayes() and calc_g_Bayes()
  
  expect_equal(res_g_mlm_Bayes, res_calc_g_Bayes)
  
  # use calc_BCSMD()
  
  Laski_comp <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP", case = case, phase = treatment,
                 session = time, outcome = outcome, center = 4,
                 FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
                 Bayesian = TRUE, chains = 2, iter = 400,
                 seed = 43073051,
                 summary = FALSE,
                 data = Laski)
    )
  
  expect_type(Laski_comp, "list")
  expect_true(is.na(Laski_comp$var_param))
  
  # compare results from calc_BCSMD() and brm()
  
  expect_equal(Laski_comp$g, res_g_mlm_Bayes$g)
  expect_equal(Laski_comp$SE, res_g_mlm_Bayes$SE_g)
  expect_equal(Laski_comp$df, res_g_mlm_Bayes$df)
  expect_equal(Laski_comp$phi, res_g_mlm_Bayes$autocor_param)
  expect_equal(Laski_comp$var_param, res_g_mlm_Bayes$var_param)
  expect_equal(Laski_comp$rho, res_g_mlm_Bayes$rho)
  
  # compare Bayes results to RML
  
  Laski_comp_RML <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP", case = case, phase = treatment,
                 session = time, outcome = outcome, center = 4,
                 FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
                 summary = FALSE,
                 data = Laski)
    )
  
  expect_equal(
    suppressWarnings(summary(Laski_comp$model)$ngrps$case),
    Laski_comp_RML$model$dims$ngrps[["case"]]
  )
  
  
})


test_that("The Bayesian estimation works for TR design.", {
  
  skip_on_cran()
  
  data(Anglesea)
  
  # compare g_mlm_Bayes() to calc_g_Bayes()
  
  Ang_Bayes <- 
    suppressWarnings(
      calc_BCSMD(design = "TR",
                 case = case, phase = condition,
                 session = session, outcome = outcome,
                 treatment_name = "treatment",
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 corStruct = "IID", varStruct = "hom",
                 Bayesian = TRUE, chains = 2, iter = 400,
                 seed = 43073051,
                 summary = FALSE,
                 data = Anglesea)
    )
  
  consts <- calc_consts(method = "Bayes", design = "TR", center = 0,
                        FE_base = 0, RE_base = 0, RE_base_2 = NULL,
                        FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                        corStruct = "IID", varStruct = "hom",
                        A = NA, B = NA)
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Ang_Bayes$model, p_const = consts$p_const, r_const = consts$r_const)
  
  res_calc_g_Bayes <- calc_g_Bayes(mod = Ang_Bayes$model, design = "TR",
                                   FE_base = 0, RE_base = 0, RE_base_2 = NULL,
                                   FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                                   corStruct = "IID", varStruct = "hom",
                                   A = NA, B = NA, center = 0)
  
  expect_equal(res_g_mlm_Bayes, res_calc_g_Bayes)
  
  # compare Bayes results to RML
  
  Ang_RML <- 
      calc_BCSMD(design = "TR",
                 case = case, phase = condition,
                 session = session, outcome = outcome,
                 treatment_name = "treatment",
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 summary = FALSE,
                 data = Anglesea)
  
  expect_equal(
    suppressWarnings(summary(Ang_Bayes$model)$ngrps$case),
    Ang_RML$model$dims$ngrps[["case"]]
  )
  
})



test_that("The Bayesian estimation works for CMB design", {
  
  skip_on_cran()
  
  data("Bryant2018")
  
  A <- 4
  B <- 21
  cent <- 0
  
  # heterogeneous residual variances
  Bry_brm_crossed <- 
    suppressWarnings(
      brm(
        bf(outcome ~ session + treatment + session_trt + 
             (1 | group) + (session + session_trt | case) +
             arma(time = session, gr = group:case, p = 1, q = 0),
           sigma ~ treatment, 
           center = FALSE),
        data = Bryant2018,
        chains = 1, iter = 200, thin = 10, cores = 1, 
        save_pars = save_pars(all = TRUE),
        seed = 20230321)
    )
  
  draws_crossed <- as.data.frame(Bry_brm_crossed)
  nCase_crossed <- suppressWarnings(summary(Bry_brm_crossed)$ngrps$case)
  nCluster_crossed <- suppressWarnings(summary(Bry_brm_crossed)$ngrps$group)
  
  
  Bry_brm_nested <- 
    suppressWarnings(
      brm(
        bf(outcome ~ session + treatment + session_trt + 
             (1 | group) + (1 + session + session_trt | group:case) +
             arma(time = session, gr = group:case, p = 1, q = 0),
           sigma ~ treatment, 
           center = FALSE),
        data = Bryant2018,
        chains = 1, iter = 200, thin = 10, cores = 1, 
        save_pars = save_pars(all = TRUE),
        seed = 20230321)
    )
  
  draws_nested <- as.data.frame(Bry_brm_nested)
  nCase_nested <- suppressWarnings(summary(Bry_brm_nested)$ngrps$`group:case`)
  nCluster_nested <- suppressWarnings(summary(Bry_brm_nested)$ngrps$group)
  expect_false(nCase_crossed == nCase_nested)
  expect_equal(nCluster_crossed, nCluster_nested)
  
  Bry_het_nested <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 1, iter = 200,
                 seed = 20230321,
                 summary = FALSE,
                 A = A, B = B,
                 data = Bryant2018)
    )
  
  expect_type(Bry_het_nested, "list")
  expect_equal(nCase_nested, suppressWarnings(summary(Bry_het_nested$model)$ngrps$`cluster:case`))
  expect_equal(nCluster_nested, suppressWarnings(summary(Bry_het_nested$model)$ngrps$cluster))
  expect_equal(mean(draws_nested$`ar[1]`), Bry_het_nested$`phi`, tol = 0.01)
  expect_equal(exp(mean(draws_nested$b_sigma_treatmenttreatment)), Bry_het_nested$`var_param`, tol = 0.01)
  
  # compare g_mlm_Bayes() and calc_g_Bayes()
  
  consts <- calc_consts(method = "Bayes", design = "CMB", center = 0,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                        FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "het",
                        A = A, B = B)
  
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Bry_het_nested$model, 
                                 p_const = consts$p_const, 
                                 r_const = consts$r_const,
                                 rconst_base_var_index = consts$rconst_base_var_index)
  
  res_calc_g_Bayes <- calc_g_Bayes(mod = Bry_het_nested$model, design = "CMB",
                                   FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                                   FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                                   corStruct = "AR1", varStruct = "het",
                                   A = A, B = B, center = 0)
  
  expect_equal(res_g_mlm_Bayes, res_calc_g_Bayes)
  
  # compare Bayes results to RML
  
  Bry_het_RML <- 
    calc_BCSMD(design = "CMB",
               cluster = group, case = case, phase = treatment,
               session = session, outcome = outcome, center = cent,
               treatment_name = "treatment",
               FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
               FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
               corStruct = "AR1", varStruct = "het",
               summary = FALSE,
               data = Bryant2018)
  
  expect_equal(
    suppressWarnings(summary(Bry_het_nested$model)$ngrps$`cluster:case`),
    Bry_het_RML$model$dims$ngrps[["case"]]
  )
  
  expect_equal(
    suppressWarnings(summary(Bry_het_nested$model)$ngrps$cluster),
    Bry_het_RML$model$dims$ngrps[["cluster"]]
  )
  
  
  # constant residual variances
  
  Bry_brm_hom <- 
    suppressWarnings(
      brm(
        bf(outcome ~ session_c + treatment + session_trt + 
             (1 | group) + (session_c + session_trt | group:case) +
             arma(time = session_c, gr = group:case, p = 1, q = 0),
           center = FALSE),
        data = Bryant2018,
        chains = 1, iter = 100, thin = 10, cores = 1,  
        save_pars = save_pars(all = TRUE),
        seed = 20230320
      )
    )
  
  Bry_hom <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                 corStruct = "AR1", varStruct = "hom",
                 Bayesian = TRUE, chains = 1, iter = 100, 
                 seed = 20230320,
                 summary = FALSE,
                 data = Bryant2018)
    )
  
  expect_type(Bry_hom, "list")
  expect_equal(Bry_brm_hom$model, Bry_hom$model$model)
  expect_equal(Bry_brm_hom[["fit"]]@model_pars, Bry_hom$model[["fit"]]@model_pars)
  
  
  # compare Bayes results to RML
  
  Bry_hom_RML <- 
    calc_BCSMD(design = "CMB",
               cluster = group, case = case, phase = treatment,
               session = session, outcome = outcome, center = cent,
               treatment_name = "treatment",
               FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
               FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
               corStruct = "AR1", varStruct = "hom",
               summary = FALSE,
               data = Bryant2018)
  
  expect_equal(
    suppressWarnings(summary(Bry_hom$model)$ngrps$`cluster:case`),
    Bry_hom_RML$model$dims$ngrps[["case"]]
  )
  
  expect_equal(
    suppressWarnings(summary(Bry_hom$model)$ngrps$cluster),
    Bry_hom_RML$model$dims$ngrps[["cluster"]]
  )
  
  
  # random models to check whether calc_BCSMD is working
  
  Bry_comp_A <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = c(0,1), RE_trt_2 = 0,
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 1, iter = 100, 
                 summary = TRUE,
                 data = Bryant2018)
    )
  
  expect_s3_class(Bry_comp_A, "data.frame")
  
  Bry_comp_B <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1,2), RE_base = c(0,1,2), RE_base_2 = c(0,1),
                 FE_trt = c(0,1,2), RE_trt = c(0,1,2), RE_trt_2 = c(0,1),
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 1, iter = 100, 
                 summary = FALSE,
                 data = Bryant2018)
    )
  
  expect_type(Bry_comp_B, "list")
  
  Bry_comp_C <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0), RE_base = c(0), RE_base_2 = c(0),
                 FE_trt = c(0), 
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 1, iter = 100, 
                 summary = FALSE,
                 data = Bryant2018)
    )
  
  expect_type(Bry_comp_C, "list")
  
})


test_that("The Bayesian estimation works for RMBB design", {
  
  skip_on_cran()
  
  data(Thiemann2001)
  
  Thie_Bayes <- 
    suppressWarnings(
      calc_BCSMD(design = "RMBB",
                 case = case, series = series, phase = treatment,
                 session = time, outcome = outcome, center = 15,
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                 corStruct = "MA1", varStruct = "hom",
                 Bayesian = TRUE, chains = 2, iter = 100, 
                 summary = FALSE,
                 data = Thiemann2001)
    )
  
  # compare g_mlm_Bayes() to calc_g_Bayes()
  
  default_AB <- default_times(design = "RMBB", 
                              case = case, series = series, 
                              phase = treatment, session = time, 
                              data = Thiemann2001)
  
  consts <- calc_consts(method = "Bayes", design = "RMBB", center = 15,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                        FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                        corStruct = "MA1", varStruct = "hom",
                        A = default_AB$A, B = default_AB$B)
  
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Thie_Bayes$model, 
                                 p_const = consts$p_const, 
                                 r_const = consts$r_const,
                                 rconst_base_var_index = consts$rconst_base_var_index)
  
  res_calc_g_Bayes <- calc_g_Bayes(mod = Thie_Bayes$model, design = "RMBB",
                                   FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                                   FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                                   corStruct = "MA1", varStruct = "hom",
                                   A = default_AB$A, B = default_AB$B, center = 15)
  
  expect_equal(res_g_mlm_Bayes, res_calc_g_Bayes)
  
  # compare bayes to RML
  
  Thie_RML <- calc_BCSMD(design = "RMBB",
                         case = case, series = series, phase = treatment,
                         session = time, outcome = outcome,
                         FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                         summary = FALSE,
                         data = Thiemann2001)
  
  expect_equal(
    suppressWarnings(summary(Thie_Bayes$model)$ngrps$case),
    Thie_RML$model$dims$ngrps[["case"]]
  )
  
  expect_equal(
    suppressWarnings(summary(Thie_Bayes$model)$ngrps$`case:series`),
    Thie_RML$model$dims$ngrps[["series"]]
  )
  
})
