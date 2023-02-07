skip_if_not_installed("nlme")

library(nlme)


test_that("calc_BCSMD() effect size does not depend on centering time for Alber-Morgan data.", {
  data(AlberMorgan)
  
  cent <- 2
  dat <- preprocess_SCD(design = "MBP", 
                        case = case, 
                        phase = condition, 
                        session = session, 
                        outcome = outcome, 
                        center = cent, 
                        data = AlberMorgan)
  
  # Fit the model
  fit_RML <- lme(fixed = outcome ~ 1 + I(session^1) + trt + I(session_trt^1), 
                 random = ~ 1 + I(session^1) | case, 
                 correlation = corAR1(0.01, ~ session | case), 
                 data = dat,
                 control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  
  # Calculate effect size with g_mlm()
  A <- 5
  B <- 31
  p_const <- c(rep(0L, length(c(0,1))), (B - A)^as.integer(c(0,1))) 
  r_const <- c(c(1, 2 * (B - cent), (B - cent)^2), c(), 
               c(0), c(), 1L) # specify whether using random effects, cor struct, var struct, and level-1 errors
  
  ES1 <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected")
  
  ES2 <- calc_BCSMD(design = "MBP",
                     case = case, 
                     phase = condition, 
                     session = session, 
                     outcome = outcome, 
                     center = cent, 
                     data = AlberMorgan,
                     FE_base = c(0,1),
                     RE_base = c(0,1),
                     FE_trt = c(0,1),
                     RE_trt = NULL,
                     A = 5, B = 31, 
                    summary = FALSE)
  
  expect_equal(ES1$g_AB, ES2$g_AB, tol = 1e-5)
  expect_equal(ES1$SE_g_AB, ES2$SE_g_AB, tol = 1e-5)
  
  ES3 <- calc_BCSMD(design = "MBP",
                    case = case, 
                    phase = condition, 
                    session = session, 
                    outcome = outcome, 
                    center = 20, 
                    data = AlberMorgan,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    FE_trt = c(0,1),
                    RE_trt = NULL,
                    A = 5, B = 31, 
                    summary = FALSE)

  expect_equal(ES2$g_AB, ES3$g_AB, tol = 1e-5)
  expect_equal(ES2$SE_g_AB, ES3$SE_g_AB, tol = 1e-5)
  
})


test_that("calc_BCSMD() effect size does not depend on centering time for Peltier data.", {
  data(Peltier)
  
  cent <- 13
  
  dat <- preprocess_SCD(design = "MBP", 
                        case = case, 
                        phase = condition, 
                        session = session, 
                        outcome = outcome, 
                        center = cent, 
                        data = Peltier)
  
  # Fit the model
  suppressWarnings(
    fit_RML <- lme(fixed = outcome ~ 1 + I(session^1) + trt + I(session_trt^1) + I(session_trt^2), 
                   random = ~ 1 + I(session^1) | case,  
                   data = dat,
                   control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  )

  # Calculate effect size with g_mlm()
  A <- 10
  B <- 16
  p_const <- c(rep(0L, length(c(0,1))), (B - A)^as.integer(c(0,1,2))) 
  r_const <- c(c(1, 2 * (B - cent), (B - cent)^2), c(), 
               c(), c(), 1L) # specify whether using random effects, cor struct, var struct, and level-1 errors
  
  ES1 <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected")
  
  ES2 <- calc_BCSMD(design = "MBP",
                    case = case, 
                    phase = condition, 
                    session = session, 
                    outcome = outcome, 
                    center = cent, 
                    data = Peltier,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    FE_trt = c(0,1,2),
                    corStruct = "IID",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES1$g_AB, ES2$g_AB, tol = 1e-5)
  expect_equal(ES1$SE_g_AB, ES2$SE_g_AB, tol = 1e-5)
  
  ES3 <- calc_BCSMD(design = "MBP",
                    case = case, 
                    phase = condition, 
                    session = session, 
                    outcome = outcome, 
                    center = 30, 
                    data = Peltier,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    FE_trt = c(0,1,2),
                    corStruct = "IID",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES2$g_AB, ES3$g_AB, tol = 1e-5)
  expect_equal(ES2$SE_g_AB, ES3$SE_g_AB, tol = 1e-5)
  
})


test_that("calc_BCSMD() effect size does not depend on centering time for Bryant data.", {
  data(Bryant2018)
  
  cent <- 13
  
  dat <- preprocess_SCD(design = "CMB", 
                        case = case, 
                        phase = treatment, 
                        session = session, 
                        outcome = outcome, 
                        cluster = group,
                        center = cent,
                        data = Bryant2018)
  
  # Fit the model
  suppressWarnings(
    fit_RML <- lme(fixed = outcome ~ 1 + I(session^1) + trt + I(session_trt^1), 
                   random = list(group = ~ 1 + I(session^1), case = ~ 1 + I(session^1) + I(session_trt^1)), 
                   correlation = corAR1(0.01, ~ session | group / case), 
                   data = dat,
                   control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  )
  
  # Calculate effect size with g_mlm()
  A <- 1
  B <- 59
  p_const <- c(rep(0L, length(c(0,1))), (B - A)^as.integer(c(0,1))) 
  r_const <- c(c(1,2 * (B - cent),(B - cent)^2), c(), 
               c(1,2 * (B - cent),(B - cent)^2), c(0,0,0), 
               c(0), c(), 1L) # specify whether using random effects, cor struct, var struct, and level-1 errors

  ES1 <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected")
  
  ES2 <- calc_BCSMD(design = "CMB",
                    case = case, 
                    phase = treatment, 
                    session = session, 
                    outcome = outcome, 
                    cluster = group,
                    center = cent, 
                    data = Bryant2018,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0,1),
                    FE_trt = c(0,1),
                    RE_trt = c(1),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES1$g_AB, ES2$g_AB, tol = 1e-5)
  expect_equal(ES1$SE_g_AB, ES2$SE_g_AB, tol = 1e-5)
  
  ES3 <- calc_BCSMD(design = "CMB",
                    case = case, 
                    phase = treatment, 
                    session = session, 
                    outcome = outcome, 
                    cluster = group,
                    center = cent, 
                    data = Bryant2018,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0),
                    FE_trt = c(0,1),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)
  
  ES4 <- calc_BCSMD(design = "CMB",
                    case = case, 
                    phase = treatment, 
                    session = session, 
                    outcome = outcome, 
                    cluster = group,
                    center = 1, 
                    data = Bryant2018,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0),
                    FE_trt = c(0,1),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES3$g_AB, ES4$g_AB, tol = 1e-3)
  expect_equal(ES3$SE_g_AB, ES4$SE_g_AB, tol = 1e-3)
  
  ES5 <- calc_BCSMD(design = "CMB",
                    case = case, 
                    phase = treatment, 
                    session = session, 
                    outcome = outcome, 
                    cluster = group,
                    center = 40, 
                    data = Bryant2018,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0),
                    FE_trt = c(0,1),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)

  expect_equal(ES3$g_AB, ES5$g_AB, tol = 1e-3)
  expect_equal(ES3$SE_g_AB, ES5$SE_g_AB, tol = 1e-3)
  
})

test_that("calc_BCSMD() effect size does not depend on centering time for Thiemann data.", {
  data(Thiemann2001)
  
  cent <- 20
  
  dat <- preprocess_SCD(design = "RMBB", 
                        case = case, 
                        series = series,
                        phase = treatment, 
                        session = time, 
                        outcome = outcome, 
                        center = cent,
                        data = Thiemann2001)
  
  # Fit the model
  suppressWarnings(
    fit_RML <- lme(fixed = outcome ~ 1 + I(time^1) + trt + I(time_trt^1) + I(time_trt^2), 
                   random = list(case = ~ 1 + trt, series = ~ 1 + I(time^1)), 
                   correlation = corAR1(0.01, ~ time | case / series), 
                   data = dat,
                   control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  )
  
  # Calculate effect size with g_mlm()
  A <- 5
  B <- 30
  p_const <- c(rep(0L, length(c(0,1))), (B - A)^as.integer(c(0,1,2))) 
  r_const <- c(c(1), c(0,0), 
               c(1,2 * (B - cent),(B - cent)^2), c(), 
               c(0), c(), 1L) # specify whether using random effects, cor struct, var struct, and level-1 errors
  
  ES1 <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected")
  
  ES2 <- calc_BCSMD(design = "RMBB", 
                    case = case, 
                    series = series,
                    phase = treatment, 
                    session = time, 
                    outcome = outcome,
                    center = cent, 
                    data = Thiemann2001,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0),
                    FE_trt = c(0,1,2),
                    RE_trt_2 = c(0),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES1$g_AB, ES2$g_AB, tol = 1e-5)
  expect_equal(ES1$SE_g_AB, ES2$SE_g_AB, tol = 1e-5)
  
  ES3 <- calc_BCSMD(design = "RMBB", 
                    case = case, 
                    series = series,
                    phase = treatment, 
                    session = time, 
                    outcome = outcome,
                    center = 10, 
                    data = Thiemann2001,
                    FE_base = c(0,1),
                    RE_base = c(0,1),
                    RE_base_2 = c(0),
                    FE_trt = c(0,1,2),
                    RE_trt_2 = c(0),
                    corStruct = "AR1",
                    A = A, B = B, 
                    summary = FALSE)
  
  expect_equal(ES2$g_AB, ES3$g_AB, tol = 1e-3)
  expect_equal(ES2$SE_g_AB, ES3$SE_g_AB, tol = 1e-3)
  
})

