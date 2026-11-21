# Only run if brms 2.23.2 and stanli are available
skip_if_not_installed("brms", minimum_version = "2.23.2")
skip_if_not_installed("stanli", minimum_version = "0.25")
stanli_is_available <- stanli::stanli_available()
skip_if_not(stanli_is_available)
cant_run_cmdstanr <- (cmdstanr_is_missing() || cmdStan_is_missing())

suppressPackageStartupMessages(library(brms))

data(Laski)


test_that("Error messages related to Bayesian estimation all work.", {
  skip_if_not_installed("mockery")
  
  mockery::stub(calc_BCSMD, 'stanli::stanli_available', FALSE)
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      backend = "stanli",
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires that the stanli runtime is available"
  )
  
  mockery::stub(calc_BCSMD, 'stanli_is_missing', TRUE)
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      backend = "stanli",
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires the stanli package"
  )
  
  mockery::stub(calc_BCSMD, 'utils::packageVersion', "2.23.0")
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      backend = "stanli",
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires brms version"
  )
  
  mockery::stub(calc_BCSMD, 'cmdStan_is_missing', TRUE)
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      backend = "cmdstanr",
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires CmdStan to be available"
  )
  
  mockery::stub(calc_BCSMD, 'cmdstanr_is_missing', TRUE)
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      backend = "cmdstanr",
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires the cmdstanr package"
  )
  
  mockery::stub(calc_BCSMD, 'brms_is_missing', TRUE)
  expect_error(
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      Bayesian = TRUE, 
      chain = 2, iter = 400, 
      summary = TRUE,
      data = Laski
    ),
    "requires the brms package"
  )
  
})

test_that("calc_BCSMD() works with rstan backend.", {
  
  skip_on_cran()
  skip_on_covr()
  
  mockery::stub(calc_BCSMD, 'stanli_is_missing', TRUE)
  
  Laski_rstan <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP",
                 case = case, phase = treatment,
                 session = time, outcome = outcome,
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 Bayesian = TRUE, 
                 chain = 2, iter = 200, 
                 data = Laski,
                 seed = 20261120)
    )
 
  expect_s3_class(Laski_rstan, "bcsmd")
  
})

test_that("calc_BCSMD() works with cmdstanr backend.", {

  skip_on_cran()
  skip_on_covr()
  skip_if(cant_run_cmdstanr)
  
  Laski_cmdstan <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP",
                 case = case, phase = treatment,
                 session = time, outcome = outcome,
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 Bayesian = TRUE, 
                 backend = "cmdstanr",
                 chain = 2, iter = 200, 
                 data = Laski,
                 seed = 20261120)
    )
  
  expect_s3_class(Laski_cmdstan, "bcsmd")
  
})

test_that("Bayesian estimation works for MBP design.", {
  
  # simple model
  
  Laski_simple <- 
    suppressWarnings(
      calc_BCSMD(design = "MBP",
                 case = case, phase = treatment,
                 session = time, outcome = outcome,
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 corStruct = "IID",
                 Bayesian = TRUE, 
                 chain = 2, iter = 400, 
                 summary = FALSE,
                 data = Laski)
    )
  
  Laski_simple_summary <- summary(Laski_simple)
  expect_s3_class(Laski_simple_summary, "data.frame")
  expect_true(is.na(Laski_simple_summary$`Variance parameter`))
  
  
  expect_error(
    graph_SCD(design="MBP",
              case = case, phase = treatment,
              session = time, outcome = outcome,
              model_fit = Laski_simple$model)
  )
  graph_Laski_comp <- graph_SCD(design="MBP",
                                case = case, phase = treatment,
                                session = time, outcome = outcome,
                                model_fit = Laski_simple$model,
                                data = Laski)
  expect_s3_class(graph_Laski_comp, "ggplot")
  expect_invisible(print(graph_Laski_comp))
  
  
  # complex model
  # use brm() and g_mlm_Bayes()
  
  Laski_dat <- preprocess_SCD(design = "MBP",
                              case = case, phase = treatment,
                              session = time, outcome = outcome,
                              center = 4, data = Laski)
  
  Laski_comp_mod <- 
    brm(
      bf(outcome ~ 1 + time + trt + time_trt + (time | case) +
           arma(time = time, gr = case, p = 1, q = 0),
         center = FALSE),
      data = Laski_dat,
      chains = 2, iter = 400, thin = 10, cores = 1, 
      backend = "stanli",
      silent = 2,
      save_pars = save_pars(all = TRUE),
      seed = 43073051
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
  
  summary_res_g <- summary(res_g_mlm_Bayes)
  expect_s3_class(summary_res_g, "data.frame")
  expect_identical(res_g_mlm_Bayes$g_AB, summary_res_g$`BC-SMD estimate`)
  expect_true(is.na(summary_res_g$Converged))
  expect_true(is.na(summary_res_g$`Initial treatment time`))
  
  # use calc_BCSMD()
  
  Laski_comp <- calc_BCSMD(
    design = "MBP", case = case, phase = treatment,
    session = time, outcome = outcome, center = 4,
    FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
    Bayesian = TRUE, chains = 2, iter = 400,
    seed = 43073051,
    summary = FALSE,
    data = Laski
  )
    
  expect_s3_class(Laski_comp, "bcsmd")
  Laski_comp_summary <- summary(Laski_comp)
  expect_s3_class(Laski_comp_summary, "data.frame")
  
  # compare results from calc_BCSMD() and brm()
  expect_equal(res_g_mlm_Bayes$g_AB, Laski_comp$g_AB)
  expect_equal(res_g_mlm_Bayes$SE_g_AB, Laski_comp$SE_g_AB)
  expect_equal(res_g_mlm_Bayes$nu, Laski_comp$nu)
  expect_equal(res_g_mlm_Bayes$var_param, Laski_comp$var_param)
  expect_equal(res_g_mlm_Bayes$phi, Laski_comp$phi)
  expect_equal(res_g_mlm_Bayes$rho, Laski_comp$rho)
  
  
  # graphing function
  graph_Laski_comp <- graph_SCD(design="MBP",
                                case = case, phase = treatment,
                                session = time, outcome = outcome,
                                model_fit = Laski_comp$model,
                                data = Laski)
  expect_s3_class(graph_Laski_comp, "ggplot")
  expect_invisible(print(graph_Laski_comp))
  
})


test_that("Bayesian estimation works for TR design.", {
  
  data(Anglesea)
  
  Ang_Bayes <- 
    suppressWarnings(
      calc_BCSMD(design = "TR",
                 case = case, phase = condition,
                 session = session, outcome = outcome,
                 treatment_name = "treatment",
                 FE_base = 0, RE_base = 0, FE_trt = 0,
                 corStruct = "AR1", varStruct = "hom",
                 Bayesian = TRUE, chains = 2, iter = 400,
                 seed = 43073051,
                 summary = FALSE,
                 data = Anglesea)
    )
  
  consts <- calc_consts(method = "Bayes", design = "TR", center = 0,
                        FE_base = 0, RE_base = 0, FE_trt = 0, 
                        corStruct = "AR1", varStruct = "hom",
                        A = NA, B = NA)
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Ang_Bayes$model, p_const = consts$p_const, r_const = consts$r_const)
  
  # compare results from calc_BCSMD() and brm()
  expect_equal(res_g_mlm_Bayes$g_AB, Ang_Bayes$g_AB)
  expect_equal(res_g_mlm_Bayes$SE_g_AB, Ang_Bayes$SE_g_AB)
  expect_equal(res_g_mlm_Bayes$nu, Ang_Bayes$nu)
  expect_equal(res_g_mlm_Bayes$var_param, Ang_Bayes$var_param)
  expect_equal(res_g_mlm_Bayes$phi, Ang_Bayes$phi)
  expect_equal(res_g_mlm_Bayes$rho, Ang_Bayes$rho)
  
  # graphing function
  graph_Ang_Bayes <- graph_SCD(design="TR",
                               case = case, phase = condition,
                               session = session, outcome = outcome,
                               model_fit = Ang_Bayes$model, data = Anglesea)
  expect_s3_class(graph_Ang_Bayes, "ggplot")
  expect_invisible(print(graph_Ang_Bayes))
  
})



test_that("Bayesian estimation works for CMB design", {
  
  data("Bryant2018")
  
  A <- 4
  B <- 21
  cent <- 0
  
  # heterogeneous residual variances

  Bry_het_nested <- 
    suppressWarnings(
      calc_BCSMD(design = "CMB",
                 cluster = group, case = case, phase = treatment,
                 session = session, outcome = outcome, center = cent,
                 treatment_name = "treatment",
                 FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                 FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                 corStruct = "AR1", varStruct = "het",
                 Bayesian = TRUE, chains = 2, iter = 200,
                 seed = 20230321,
                 summary = FALSE,
                 A = A, B = B,
                 data = Bryant2018)
    )
  
  expect_s3_class(Bry_het_nested, "bcsmd")
  

  # graphing function
  graph_Bry <- graph_SCD(design = "CMB",
                         cluster = group, case = case, phase = treatment,
                         session = session, outcome = outcome, 
                         treatment_name = "treatment",
                         model_fit = Bry_het_nested$model,
                         data = Bryant2018)
  expect_s3_class(graph_Bry, "ggplot")
  expect_invisible(print(graph_Bry))
  
  # compare g_mlm_Bayes() and calc_BCSMD()
  
  consts <- calc_consts(method = "Bayes", design = "CMB", center = 0,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
                        FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "het",
                        A = A, B = B)
  
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Bry_het_nested$model, 
                                 p_const = consts$p_const, 
                                 r_const = consts$r_const,
                                 rconst_base_var_index = consts$rconst_base_var_index)
  
  # compare results from calc_BCSMD() and brm()
  expect_equal(res_g_mlm_Bayes$g_AB, Bry_het_nested$g_AB)
  expect_equal(res_g_mlm_Bayes$SE_g_AB, Bry_het_nested$SE_g_AB)
  expect_equal(res_g_mlm_Bayes$nu, Bry_het_nested$nu)
  expect_equal(res_g_mlm_Bayes$var_param, Bry_het_nested$var_param)
  expect_equal(res_g_mlm_Bayes$phi, Bry_het_nested$phi)
  expect_equal(res_g_mlm_Bayes$rho, Bry_het_nested$rho)
  
  
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
        backend = "stanli",
        silent = 2,
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
  
  expect_s3_class(Bry_hom, "bcsmd")
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
  
  expect_s3_class(Bry_comp_B, "bcsmd")
  
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
  
  expect_s3_class(Bry_comp_C, "bcsmd")
  
})


test_that("Bayesian estimation works for RMBB design", {
  
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
  
  # graphing function
  graph_Thie <- graph_SCD(design = "RMBB",
                          case = case, series = series, phase = treatment,
                          session = time, outcome = outcome, 
                          model_fit = Thie_Bayes$model,
                          data = Thiemann2001)
  expect_s3_class(graph_Thie, "ggplot")
  expect_invisible(print(graph_Thie))
  
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
  
  # compare results from calc_BCSMD() and brm()
  expect_equal(res_g_mlm_Bayes$g_AB, Thie_Bayes$g_AB)
  expect_equal(res_g_mlm_Bayes$SE_g_AB, Thie_Bayes$SE_g_AB)
  expect_equal(res_g_mlm_Bayes$nu, Thie_Bayes$nu)
  expect_equal(res_g_mlm_Bayes$var_param, Thie_Bayes$var_param)
  expect_equal(res_g_mlm_Bayes$phi, Thie_Bayes$phi)
  expect_equal(res_g_mlm_Bayes$rho, Thie_Bayes$rho)
  
})

test_that("Running pre-complied brm models using update().", {
  
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  
  # using the update method
  # compile the Laski_brm model and then apply to AlberMogan data
  ## compile the Laski_brm
  data(Laski)
  Laski_dat <- preprocess_SCD(design = "MBP",
                              case = case, phase = treatment,
                              session = time, outcome = outcome,
                              center = 4, data = Laski)
  Laski_brm <- 
    suppressWarnings(
      brm(
        bf(outcome ~ 1 + time + trt + time_trt + (time | case) +
             arma(time = time, gr = case, p = 1, q = 0),
           center = FALSE),
        data = Laski_dat,
        chains = 2, iter = 1000, thin = 10, cores = 1, 
        backend = "stanli",
        silent = 2,
        save_pars = save_pars(all = TRUE),
        seed = 20240125)
    )
  ## calculate BC-SMD
  default_AB <- default_times(design = "MBP", 
                              case = case, phase = treatment, session = time, 
                              data = Laski) # Use original Laski to get default A & B
  consts <- calc_consts(method = "Bayes", design = "MBP", center = 4,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = NULL,
                        FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "hom",
                        A = default_AB$A, B = default_AB$B)
  g_Bayes_Laski <- g_mlm_Bayes(mod = Laski_brm, 
                               p_const = consts$p_const, 
                               r_const = consts$r_const)
  
  
  ## apply to AlberMorgan data using update function
  data("AlberMorgan")
  AM_dat <- preprocess_SCD(design = "MBP",
                           case = case, phase = condition,
                           session = session, outcome = outcome,
                           center = 4, data = AlberMorgan)
  names(AM_dat) <- c("case", "treatment", "time", "outcome", "trt", "time_trt")
  AM_fit <- suppressWarnings(update(Laski_brm, newdata = AM_dat))
  
  ## calculate BC-SMD
  default_AB <- default_times(design = "MBP", 
                              case = case, phase = condition, session = session, 
                              data = AlberMorgan)
  consts <- calc_consts(method = "Bayes", design = "MBP", center = 4,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = NULL,
                        FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "hom",
                        A = default_AB$A, B = default_AB$B)
  g_Bayes_AM <- g_mlm_Bayes(mod = AM_fit, 
                            p_const = consts$p_const, 
                            r_const = consts$r_const)
  
  # Get the stan code from a fitted brm model (how I created the inst/stan/StanMBP.stan file.)
  ## You may extract stan code from the fitted brm model
  stancode_Laski <- stancode(Laski_brm)
  ## Or you may make the stan code using make_stancode function
  stan_code <- make_stancode(bf(outcome ~ 1 + time + trt + time_trt + (time | case) +
                                  arma(time = time, gr = case, p = 1, q = 0),
                                center = FALSE),
                             data = Laski_dat)
  ## save the code as "StanMBP.stan" then copy and paste after clicking File/New File/Stan File
  
})
