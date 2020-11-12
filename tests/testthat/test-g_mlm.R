# check if g_mlm() works

library(nlme)

data(Laski)
data(Bryant2016, package = "lmeInfo")

Laski_RML1 <- lme(fixed = outcome ~ treatment, 
                  random = ~ 1 | case, 
                  correlation = corAR1(0, ~ time | case), 
                  data = Laski)

Bryant2016_RML1 <- lme(fixed = outcome ~ treatment,
                       random = ~ 1 | school/case,
                       correlation = corAR1(0, ~ session | school/case),
                       data = Bryant2016)

test_that("g_mlm() is imported appropriately.", {
  
  # two-level data
  Laski_g1_REML <- suppressWarnings(g_REML(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE))
  Laski_g1_mlm <- g_mlm(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE)
  
  expect_warning(g_REML(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE))
  expect_equal(varcomp_vcov(Laski_RML1)[1, 1], 20214.7623585, tol = 1e-7)
  
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
  
  # three-level data
  suppressWarnings(expect_error(g_REML(Bryant2016_RML1, p_const = c(0, 1), r_const = c(1, 0, 1, 1)))) # g_REML not available for 3-level data
  Bryant2016_g1_mlm <- g_mlm(Bryant2016_RML1, p_const = c(0, 1), r_const = c(1, 1, 0, 1),
                             infotype = "expected", returnModel = TRUE)
  
  expect_output(summary(Bryant2016_g1_mlm))
  expect_output(print(Bryant2016_g1_mlm))
  
  # confidence interval
  expect_equal(CI_g(Laski_g1_REML), CI_g(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML), lmeInfo:::CI_g.g_mlm(Laski_g1_mlm))
  expect_equal(CI_g(Laski_g1_REML, symmetric = FALSE), CI_g(Laski_g1_mlm, symmetric = FALSE))
  expect_equal(CI_g(Bryant2016_g1_mlm, symmetric = TRUE)[1], 0.2180581, tol = 1e-6)
  expect_equal(CI_g(Bryant2016_g1_mlm, symmetric = TRUE)[2], 0.7085557, tol = 1e-6)
  
  
})
