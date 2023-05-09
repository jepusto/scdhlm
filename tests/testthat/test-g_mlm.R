skip_if_not_installed("nlme")

# check if g_mlm() works

library(nlme)

test_that("g_mlm() is imported appropriately for Laski data.", {
  
  data(Laski)
  
  # simple model
  Laski_RML1 <- lme(fixed = outcome ~ treatment, 
                    random = ~ 1 | case, 
                    correlation = corAR1(0.01, ~ time | case), 
                    data = Laski)
  
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
  expect_equal(Laski_g1_mlm$r_theta, Laski_g1_REML$r_theta) # squared denominator of effect size
  expect_equal(Laski_g1_mlm$delta_AB, Laski_g1_REML$delta_AB) # unadjusted (REML) effect size estimate
  expect_equal(as.numeric(Laski_g1_mlm$nu), Laski_g1_REML$nu) # degrees of freedom
  expect_equal(as.numeric(Laski_g1_mlm$kappa), Laski_g1_REML$kappa) # constant kappa
  expect_equal(as.numeric(Laski_g1_mlm$g_AB), Laski_g1_REML$g_AB) # corrected effect size estimate
  expect_equal(as.numeric(Laski_g1_mlm$SE_g_AB^2), Laski_g1_REML$V_g_AB) # Approximate variance estimate
  expect_equal(Laski_g1_mlm$theta$sigma_sq, Laski_g1_REML$sigma_sq) # Estimated level-1 variance
  expect_equal(Laski_g1_mlm$theta$cor_params, Laski_g1_REML$phi) # Estimated autocorrelation
  expect_equal(Laski_g1_mlm$theta$Tau$case, Laski_g1_REML$Tau) # Vector of level-2 variance components
  expect_equal(det(Laski_g1_mlm$info_inv), det(Laski_g1_REML$I_E_inv)) # Expected information matrix
  
  ## confidence interval
  expect_equal(CI_g(Laski_g1_REML), CI_g(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML), lmeInfo:::CI_g.g_mlm(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML, symmetric = FALSE), CI_g(Laski_g1_mlm, symmetric = FALSE))
  

  
  # complex model
  default_AB <- default_times(design = "MBP",
                              case = case, phase = treatment, session = time, 
                              data = Laski)
  D <- default_AB$B - default_AB$A
  
  dat <- preprocess_SCD(design = "MBP", 
                        case = case, phase = treatment, session = time, outcome = outcome, 
                        center = 4, data = Laski)
  Laski_RML2 <- suppressWarnings(lme(fixed = outcome ~ time + treatment + time_trt,
                                     random = ~ time | case, 
                                     correlation = corAR1(0.01, ~ time | case), 
                                     data = dat,
                                     control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE)))
  
  ## g_REML VS g_mlm
  Laski_g2_REML <- suppressWarnings(g_REML(Laski_RML2, p_const = c(0,0,1,D), r_const = c(1,0,1,18,81), returnModel = TRUE))
  Laski_g2_mlm <- suppressWarnings(g_mlm(Laski_RML2, p_const = c(0,0,1,D), r_const = c(1,18,81,0,1), returnModel = TRUE))
  
  expect_equal(Laski_g2_mlm$p_beta, Laski_g2_REML$p_beta) # numerator of effect size
  expect_equal(Laski_g2_mlm$r_theta, Laski_g2_REML$r_theta) # squared denominator of effect size
  expect_equal(Laski_g2_mlm$delta_AB, Laski_g2_REML$delta_AB) # unadjusted (REML) effect size estimate
  expect_equal(as.numeric(Laski_g2_mlm$nu), Laski_g2_REML$nu) # degrees of freedom
  expect_equal(as.numeric(Laski_g2_mlm$kappa), Laski_g2_REML$kappa) # constant kappa
  expect_equal(as.numeric(Laski_g2_mlm$g_AB), Laski_g2_REML$g_AB) # corrected effect size estimate
  expect_equal(as.numeric(Laski_g2_mlm$SE_g_AB^2), Laski_g2_REML$V_g_AB) # Approximate variance estimate
  expect_equal(Laski_g2_mlm$theta$sigma_sq, Laski_g2_REML$sigma_sq) # Estimated level-1 variance
  expect_equal(Laski_g2_mlm$theta$cor_params, Laski_g2_REML$phi) # Estimated autocorrelation
  expect_equal(Laski_g2_mlm$theta$Tau$case, Laski_g2_REML$Tau) # Vector of level-2 variance components
  expect_equal(det(Laski_g2_mlm$info_inv), det(Laski_g2_REML$I_E_inv)) # Expected information matrix
  
  ## confidence interval
  expect_equal(CI_g(Laski_g2_REML), CI_g(Laski_g2_mlm))
  expect_equal(CI_g(Laski_g2_REML), lmeInfo:::CI_g.g_mlm(Laski_g2_mlm))
  expect_equal(CI_g(Laski_g2_REML, symmetric = FALSE), CI_g(Laski_g2_mlm, symmetric = FALSE))
  
})

test_that("g_mlm() is imported appropriately for Bryant 2018 data.", {
  
  data(Bryant2018)
  Bryant_RML1 <- lme(fixed = outcome ~ treatment,
                     random = ~ 1 | group/case,
                     correlation = corAR1(0.01, ~ session | group/case),
                     data = Bryant2018, 
                     na.action = na.omit)
  
  suppressWarnings(expect_error(g_REML(Bryant_RML1, p_const = c(0, 1), r_const = c(1, 0, 1, 1)))) # g_REML not available for 3-level data
  Bry_g1_mlm <- g_mlm(Bryant_RML1, p_const = c(0, 1), r_const = c(1, 1, 0, 1), infotype = "expected", returnModel = TRUE)
  
  expect_output(summary(Bry_g1_mlm))
  expect_output(print(Bry_g1_mlm))
  
})

test_that("Convergence indicator works correctly for g_mlm(), g_REML() and calc_BCSMD().", {
  
  data(Laski)
  
  # simple model
  Laski_BCSMD1 <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment,
               session = time, outcome = outcome, center = 4,
               FE_base = 0, RE_base = 0, FE_trt = 0,
               data = Laski)
  )
  
  expect_true(Laski_BCSMD1$converged)
  
  # complex model
  Laski_BCSMD2 <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment,
               session = time, outcome = outcome, center = 4,
               FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1), RE_trt = c(0,1),
               data = Laski)
  )
  
  expect_equal(attr(Laski_BCSMD2$converged, "class")[1], "simpleWarning")

})
