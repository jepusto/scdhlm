skip_if_not_installed("brms")

library(brms)

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
                 seed = 43073051,
                 summary = FALSE,
                 data = Laski)
    )
  
  expect_type(Laski_comp, "list")
  expect_true(is.na(Laski_comp$var_param))
  
  # compare results from calc_BCSMD() and brm()
  
  expect_equal(Laski_comp$g, Laski_comp_brm$g)
  expect_equal(Laski_comp$SE, Laski_comp_brm$SE_g)
  expect_equal(Laski_comp$df, Laski_comp_brm$df)
  expect_equal(Laski_comp$phi, Laski_comp_brm$autocor_param)
  expect_equal(Laski_comp$var_param, Laski_comp_brm$var_param)
  expect_equal(Laski_comp$rho, Laski_comp_brm$rho)
  
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
  
  # use calc_BCSMD()
  Ang_Bayes <- 
    suppressWarnings(
      calc_BCSMD(design = "TR",
                 case = case, phase = condition,
                 session = session, outcome = outcome,
                 treatment_name = "treatment",
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 Bayesian = TRUE, chains = 2, iter = 400,
                 seed = 43073051,
                 summary = FALSE,
                 data = Anglesea)
    )
  
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
                 data = Bryant2018)
    )
  
  expect_type(Bry_het_nested, "list")
  expect_equal(nCase_nested, suppressWarnings(summary(Bry_het_nested$model)$ngrps$`cluster:case`))
  expect_equal(nCluster_nested, suppressWarnings(summary(Bry_het_nested$model)$ngrps$cluster))
  expect_equal(mean(draws_nested$`ar[1]`), Bry_het_nested$`phi`, tol = 0.01)
  expect_equal(exp(mean(draws_nested$b_sigma_treatmenttreatment)), Bry_het_nested$`var_param`, tol = 0.01)
  
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
                 session = time, outcome = outcome,
                 FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                 Bayesian = TRUE, chains = 2, iter = 100, 
                 summary = FALSE,
                 data = Thiemann2001)
    )
  
  Thie_RML <- calc_BCSMD(design = "RMBB",
                         case = case, series = series, phase = treatment,
                         session = time, outcome = outcome,
                         FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                         summary = FALSE,
                         data = Thiemann2001)
  
  pr_consts <- calc_consts(estimation = "Bayes", design = "RMBB", 
                           FE_base = 0, RE_base = 0, RE_base_2 = 0,
                           FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                           corStruct = "AR1", varStruct = "hom",
                           A = 3, B = 6, center = 0)
  
  expect_equal(
    suppressWarnings(summary(Thie_Bayes$model)$ngrps$case),
    Thie_RML$model$dims$ngrps[["case"]]
  )
  
  expect_equal(
    suppressWarnings(summary(Thie_Bayes$model)$ngrps$`case:series`),
    Thie_RML$model$dims$ngrps[["series"]]
  )
  
})
