# check if g_mlm() works

library(nlme)

test_that("g_mlm() is imported appropriately for Laski data.", {
  
  data(Laski)
  
  Laski_RML1 <- lme(fixed = outcome ~ treatment, 
                    random = ~ 1 | case, 
                    correlation = corAR1(0.01, ~ time | case), 
                    data = Laski)
  
  # two-level data
  ## g_REML VS g_mlm
  Laski_g1_REML <- suppressWarnings(g_REML(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE))
  Laski_g1_mlm <- g_mlm(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE)
  
  expect_warning(g_REML(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE))
  expect_equal(varcomp_vcov(Laski_RML1)[1, 1], 20214.7841577, tol = 1e-7)
  
  varcomp_ex <- extract_varcomp_lmeAR1(Laski_RML1) # extract varcomp using old function in scdhlm
  varcomp <- extract_varcomp(Laski_RML1) # extract varcomp using new function in lmeInfo
  expect_equal(varcomp_ex$sigma_sq, varcomp$sigma_sq)
  expect_equal(varcomp_ex$phi, varcomp$cor_params)
  expect_equal(varcomp_ex$Tau, varcomp$Tau$case)
  
  expect_output(summary(Laski_g1_REML))
  expect_output(print(Laski_g1_REML))
  expect_output(summary(Laski_g1_mlm))
  expect_output(print(Laski_g1_mlm))
  
  expect_equal(Laski_g1_mlm$p_beta, Laski_g1_REML$p_beta) # numerator of effect size
  expect_equal(Laski_g1_mlm$r_beta, Laski_g1_REML$r_beta) # squared denominator of effect size
  expect_equal(Laski_g1_mlm$delta_AB, Laski_g1_REML$delta_AB) # unadjusted (REML) effect size estimate
  expect_equal(as.numeric(Laski_g1_mlm$nu), Laski_g1_REML$nu) # degrees of freedom
  expect_equal(as.numeric(Laski_g1_mlm$kappa), Laski_g1_REML$kappa) # constant kappa
  expect_equal(as.numeric(Laski_g1_mlm$g_AB), Laski_g1_REML$g_AB) # corrected effect size estimate
  expect_equal(as.numeric(Laski_g1_mlm$SE_g_AB^2), Laski_g1_REML$V_g_AB) # Approximate variance estimate
  expect_equal(Laski_g1_mlm$theta$sigma_sq, Laski_g1_REML$sigma_sq) # Estimated level-1 variance
  expect_equal(Laski_g1_mlm$theta$cor_params, Laski_g1_REML$phi) # Estimated autocorrelation
  expect_equal(Laski_g1_mlm$theta$Tau$case, Laski_g1_REML$Tau) # Vector of level-2 variance components
  expect_equal(det(Laski_g1_mlm$info_inv), det(Laski_g1_REML$I_E_inv)) # Expected information matrix
  
  
  ## g_mlm VS calc_BCSMD
  Laski_calc_BCSMD <- calc_BCSMD(design = "MBP",
                                 case = case, phase = treatment,
                                 session = time, outcome = outcome,
                                 FE_base = 0, RE_base = 0, FE_trt = 0,
                                 summary = FALSE,
                                 data = Laski)
  expect_equal(Laski_g1_mlm, Laski_calc_BCSMD, check.attributes = FALSE)
  
  Laski_calc_BCSMD_summary <- calc_BCSMD(design = "MBP",
                                         case = case, phase = treatment,
                                         session = time, outcome = outcome,
                                         FE_base = 0, RE_base = 0, FE_trt = 0,
                                         data = Laski)
  expect_equal(Laski_g1_mlm$g_AB, Laski_calc_BCSMD_summary$`BC-SMD estimate`)
  expect_equal(Laski_g1_mlm$SE_g_AB, Laski_calc_BCSMD_summary$`Std. Error`)
  expect_equal(Laski_g1_mlm$nu, Laski_calc_BCSMD_summary$`Degrees of freedom`)
  
  
  # confidence interval
  expect_equal(CI_g(Laski_g1_REML), CI_g(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML), lmeInfo:::CI_g.g_mlm(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML, symmetric = FALSE), CI_g(Laski_g1_mlm, symmetric = FALSE))
  
  expect_equal(CI_g(Laski_g1_mlm)[1], Laski_calc_BCSMD_summary$`95% CI (lower)`)
  expect_equal(CI_g(Laski_g1_mlm)[2], Laski_calc_BCSMD_summary$`95% CI (upper)`)
  
})

test_that("g_mlm() is imported appropriately for Bryant 2016 data.", {
  
  skip_if_not_installed("lmeInfo")
  
  data(Bryant2016, package = "lmeInfo")
  Bryant2016_RML1 <- lme(fixed = outcome ~ treatment,
                         random = ~ 1 | school/case,
                         correlation = corAR1(0.01, ~ session | school/case),
                         data = Bryant2016)
  
  # three-level data
  suppressWarnings(expect_error(g_REML(Bryant2016_RML1, p_const = c(0, 1), r_const = c(1, 0, 1, 1)))) # g_REML not available for 3-level data
  Bryant2016_g1_mlm <- g_mlm(Bryant2016_RML1, p_const = c(0, 1), r_const = c(1, 1, 0, 1),
                             infotype = "expected", returnModel = TRUE)
  
  expect_output(summary(Bryant2016_g1_mlm))
  expect_output(print(Bryant2016_g1_mlm))
  
  Bry2016_calc_BCSMD <- calc_BCSMD(design = "CMB",
                                   cluster = school, case = case, phase = treatment,
                                   session = session, outcome = outcome,
                                   treatment_name = "treatment",
                                   FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                                   summary = FALSE,
                                   data = Bryant2016)
  expect_equal(Bryant2016_g1_mlm, Bry2016_calc_BCSMD, check.attributes = FALSE)
  
  Bry_calc_BCSMD_summary <- calc_BCSMD(design = "CMB",
                                       cluster = school, case = case, phase = treatment,
                                       session = session, outcome = outcome,
                                       treatment_name = "treatment",
                                       FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                                       data = Bryant2016)
  expect_equal(Bryant2016_g1_mlm$g_AB, Bry_calc_BCSMD_summary$`BC-SMD estimate`)
  expect_equal(Bryant2016_g1_mlm$SE_g_AB, Bry_calc_BCSMD_summary$`Std. Error`)
  expect_equal(Bryant2016_g1_mlm$nu, Bry_calc_BCSMD_summary$`Degrees of freedom`)
  
  
  # confidence interval
  expect_equal(CI_g(Bryant2016_g1_mlm, symmetric = TRUE)[1], 0.2180615, tol = 1e-6)
  expect_equal(CI_g(Bryant2016_g1_mlm, symmetric = TRUE)[2], 0.7085615, tol = 1e-6)
  
  expect_equal(CI_g(Bryant2016_g1_mlm)[1], Bry_calc_BCSMD_summary$`95% CI (lower)`)
  expect_equal(CI_g(Bryant2016_g1_mlm)[2], Bry_calc_BCSMD_summary$`95% CI (upper)`)
  
})
