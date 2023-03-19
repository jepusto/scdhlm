skip_if_not_installed("brms")

library(brms)

test_that("The Bayesian estimation works for two-level model.", {
  
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
  D <- default_AB$B - default_AB$A
  p_const <- c(0,0,1,D)
  r_const <- c(1, D^2, 2*D, 1)
  Laski_comp_brm <- g_mlm_Bayes(Laski_comp_mod, p_const, r_const) 
  
  samples_A <- as_draws_matrix(Laski_comp_mod)
  samples_A_fixed <- samples_A[, startsWith(colnames(samples_A), "b")]
  es_num_A <- apply(samples_A_fixed, 1, function(x) sum(x * p_const))
  
  sigma_A <- samples_A[, "sigma"]
  sigma_A_sq <- sigma_A^2
  samples_A_r_sd <- samples_A[, startsWith(colnames(samples_A), "sd")]
  samples_A_r_var <- samples_A_r_sd^2
  samples_A_r_cov <- samples_A[, startsWith(colnames(samples_A), "cor")] * 
    samples_A_r_sd[,1] * samples_A_r_sd[,2]
  samples_A_r_varcov <- cbind(samples_A_r_var, samples_A_r_cov, sigma_A_sq)
  es_denom_A <- apply(samples_A_r_varcov, 1, function(x) sum(x * r_const))
  
  expect_equal(Laski_comp_brm$es_num_vec, es_num_A)
  expect_equal(Laski_comp_brm$es_denom_vec, es_denom_A)
  
  # use calc_BCSMD()
  Laski_comp <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP", case = case, phase = treatment,
                 session = time, outcome = outcome, center = 4,
                 FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
                 Bayesian = TRUE, chains = 2, iter = 400,
                 summary = FALSE,
                 data = Laski)
    )
  
  expect_type(Laski_comp, "list")
  expect_true(is.na(Laski_comp$var_param))
  
  samples_B <- as_draws_matrix(Laski_comp$model)
  samples_B_fixed <- samples_B[, startsWith(colnames(samples_B), "b")]
  es_num_B <- apply(samples_B_fixed, 1, function(x) sum(x * p_const))
  
  sigma_B <- samples_B[, "sigma"]
  sigma_B_sq <- sigma_B^2
  samples_B_r_sd <- samples_B[, startsWith(colnames(samples_B), "sd")]
  samples_B_r_var <- samples_B_r_sd^2
  samples_B_r_cov <- samples_B[, startsWith(colnames(samples_B), "cor")] * 
    samples_B_r_sd[,1] * samples_B_r_sd[,2]
  samples_B_r_varcov <- cbind(samples_B_r_var, samples_B_r_cov, sigma_B_sq)
  es_denom_B <- apply(samples_B_r_varcov, 1, function(x) sum(x * r_const))
  
  # compare results from calc_BCSMD() and brm()
  
  expect_equal(Laski_comp$g, Laski_comp_brm$g)
  expect_equal(Laski_comp$SE, Laski_comp_brm$SE_g)
  expect_equal(Laski_comp$df, Laski_comp_brm$df)
  expect_equal(Laski_comp$phi, Laski_comp_brm$autocor_param)
  expect_equal(Laski_comp$var_param, Laski_comp_brm$var_param)
  expect_equal(Laski_comp$rho, Laski_comp_brm$rho)
  
})

test_that("The Bayesian estimation works for CMB design", {
  
  skip_on_cran()
  
  data("Bryant2018")
  
  A <- 4
  B <- 21
  cent <- 0
  
  # heterogeneous residual variances
  Bry_brm_het <- 
    suppressWarnings(
      brm(
        bf(outcome ~ session_c + treatment + session_trt + 
             (1 | group) + (session_c + session_trt | case) +
             arma(time = session_c, gr = group:case, p = 1, q = 0),
           sigma ~ treatment, 
           center = FALSE),
        data = Bryant2018,
        chains = 1, iter = 100, thin = 10, cores = 1, 
        save_pars = save_pars(all = TRUE),
        seed = 43073051)
    )
  
  draws_A <- as.data.frame(Bry_brm_het)
  
  Bry_het <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 1, iter = 100, 
                 summary = TRUE,
                 data = Bryant2018)
    )
  
  expect_s3_class(Bry_het, "data.frame")
  expect_equal(mean(draws_A$`ar[1]`), Bry_het$`Auto-correlation`, tol = 0.01)
  expect_equal(exp(mean(draws_A$b_sigma_treatmenttreatment)), Bry_het$`Variance parameter`, tol = 0.02)
  
  # constant residual variances
  Bry_brm_hom <- 
    suppressWarnings(
      brm(
        bf(outcome ~ session_c + treatment + session_trt + 
             (1 | group) + (session_c + session_trt | case) +
             arma(time = session_c, gr = group:case, p = 1, q = 0),
           center = FALSE),
        data = Bryant2018,
        chains = 1, iter = 100, thin = 10, cores = 1,  
        save_pars = save_pars(all = TRUE),
        seed = 43073051
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
                 summary = FALSE,
                 data = Bryant2018)
    )
  
  expect_type(Bry_hom, "list")
  expect_equal(Bry_brm_hom$model, Bry_hom$model$model)
  expect_equal(Bry_brm_hom[["fit"]]@model_pars, Bry_hom$model[["fit"]]@model_pars)
  
  
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
