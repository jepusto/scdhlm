skip_if_not_installed("brms")

library(brms)


getmode <- function(x) {
  x_dens <- density(x)
  x_mode <- x_dens$x[which.max(x_dens$y)]
  
  return(x_mode)
}


test_that("g_mlm_Bayes() returns same results as MLE for MBP design.", {
  
  skip_on_cran()
  
  data(Laski)
  
  Laski_dat <- preprocess_SCD(design = "MBP",
                              case = case, phase = treatment,
                              session = time, outcome = outcome,
                              center = 4, data = Laski)
  prior <- 
    prior(normal(0,10^6), class = "b", coef = "Intercept") + 
    prior(normal(0,10^6), class = "b", coef = "time") + 
    prior(normal(0,10^6), class = "b", coef = "trt") + 
    prior(normal(0,10^6), class = "b", coef = "time_trt") + 
    prior(uniform(0,100), class = "sigma") + 
    prior(cauchy(0, 10^6), class = "sd", group = "case", coef = "Intercept") + 
    prior(cauchy(0, 10^6), class = "sd", group = "case", coef = "time") +
    prior(normal(0, 0.2), class = "ar")
  
  Laski_brm <- 
    suppressWarnings(
      brm(
        bf(outcome ~ 1 + time + trt + time_trt + (time | case) +
             arma(time = time, gr = case, p = 1, q = 0),
           center = FALSE),
        data = Laski_dat,
        prior = prior,
        chains = 2, iter = 4000, thin = 10, cores = 1, 
        save_pars = save_pars(all = TRUE),
        seed = 20230417)
    )
  
  default_AB <- default_times(design = "MBP", 
                              case = case, phase = treatment, session = time, 
                              data = Laski)
  consts <- calc_consts(estimation = "Bayes", design = "MBP", center = 4,
                        FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = NULL,
                        FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
                        corStruct = "AR1", varStruct = "hom",
                        A = default_AB$A, B = default_AB$B)
  
  res_g_mlm_Bayes <- g_mlm_Bayes(mod = Laski_brm, p_const = consts$p_const, r_const = consts$r_const)
  
  es_mode_bayes <- getmode(res_g_mlm_Bayes$es_num_vec / res_g_mlm_Bayes$es_denom_vec)
  
  # MLE
  
  fit_MLE <- lme(fixed = outcome ~ 1 + time + trt + time_trt, 
                 random = ~ 1 + time | case, 
                 correlation = corAR1(0.01, ~ time | case), 
                 method = "ML",
                 data = Laski_dat,
                 control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  p_const <- c(0,0,1,9)
  r_const <- c(1,18,81,0,1)
  ES_MLE <- g_mlm(fit_MLE, p_const = p_const, r_const = r_const, infotype = "expected")
  
})
