skip_if_not_installed("nlme")

# check if calc_BCSMD() works

library(nlme)

test_that("The logic for A, B, and D argument in calc_BCSMD() is correct.", {
  
  data(Laski)
  
  # use the default_times function to calculate default A and B
  default_AB <- default_times(design = "MBP", case = case, phase = treatment, session = time, data = Laski)
  default_AB_2 <- default_times(design = "MBP", case = Laski$case, phase = Laski$treatment, session = Laski$time)
  expect_equal(default_AB, default_AB_2)
  default_A <- default_AB$A
  default_B <- default_AB$B
  
  # !is.null(B) & !is.null(D), stop and show error
  expect_error(calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome,
               A = NULL, B = 13, D = 9, data = Laski))
  
  expect_error(calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome,
               A = 4, B = 13, D = 9, data = Laski))
  
  # is.null(A) & is.null(B) & is.null(D), get default A and B
  Las1 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, data = Laski)
  expect_equal(Las1$`Initial treatment time`, default_A)
  expect_equal(Las1$`Follow-up time`, default_B)
  
  # is.null(A) & !is.null(B) & is.null(D), get default A and specified B
  Las2 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, B = 12, data = Laski)
  expect_equal(Las2$`Initial treatment time`, default_A)
  expect_equal(Las2$`Follow-up time`, 12)
  
  # is.null(A) & is.null(B) & !is.null(D), get default A, B = A + D
  Las3 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, D = 8, data = Laski)
  expect_equal(Las3$`Initial treatment time`, default_A)
  expect_equal(Las3$`Follow-up time`, default_A + 8)
  
  # !is.null(A) & is.null(B) & !is.null(D), get specified A, B = A + D
  Las4 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, 
                     A = 3, D = 8, data = Laski)
  expect_equal(Las4$`Initial treatment time`, 3)
  expect_equal(Las4$`Follow-up time`, 3 + 8)
  
  # !is.null(A) & !is.null(B) & is.null(D), get specified A and specified B
  Las5 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, 
                     A = 3, B = 11, data = Laski)
  expect_equal(Las5$`Initial treatment time`, 3)
  expect_equal(Las5$`Follow-up time`, 11)
  
  # !is.null(A) & is.null(B) & is.null(D), get specified A and default B
  Las6 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, A = 3, data = Laski)
  expect_equal(Las6$`Initial treatment time`, 3)
  expect_equal(Las6$`Follow-up time`, default_B)
  
  # if centered
  Las_c <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, 
               center = 4, FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
               A = 3, data = Laski)
   )
  expect_equal(Las_c$`Initial treatment time`, 3)
  expect_equal(Las_c$`Follow-up time`, default_B)
  
})


test_that("The default_times() calculates A and B correctly.", {
  
  # MBP
  data(Laski)
  default_AB <- default_times(design = "MBP", case = case, phase = treatment, session = time, data = Laski)
  default_A <- default_AB$A
  default_B <- default_AB$B
  
  case_base_last <- with(Laski, tapply(time[treatment == "baseline"], case[treatment == "baseline"], max))
  case_trt_range <- with(Laski, tapply(time[treatment == "treatment"], case[treatment == "treatment"], function(x) diff(range(x)) + 1))
  A <- min(case_base_last)
  B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  
  expect_equal(default_A, A)
  expect_equal(default_B, B)
  
  # TR
  data(Anglesea)
  default_AB_Ang <- default_times(design = "TR", case = case, phase = condition, session = session, data = Anglesea)
  expect_true(is.na(default_AB_Ang$A))
  expect_true(is.na(default_AB_Ang$B))
  
  # three-level
  data("Thiemann2001")
  Thiemann2001$caseSeries <- with(Thiemann2001, paste(case, series, sep = "-"))
  default_AB_Thi <- default_times(design = "RMBB", case = case, series = series, phase = treatment, session = time, data = Thiemann2001)
  default_A_Thi <- default_AB_Thi$A
  default_B_Thi <- default_AB_Thi$B
  
  series_base_last <- with(Thiemann2001, tapply(time[treatment == "baseline"], caseSeries[treatment == "baseline"], max))
  series_trt_range <- with(Thiemann2001, tapply(time[treatment == "treatment"], caseSeries[treatment == "treatment"], function(x) diff(range(x)) + 1))
  A_Thi <- min(series_base_last)
  B_Thi <- A_Thi + min(series_trt_range[which(series_base_last == min(series_base_last))])
  
  expect_equal(default_A_Thi, A_Thi)
  expect_equal(default_B_Thi, B_Thi)
  
  data("Bryant2018")
  Bryant2018$clusterCase <- with(Bryant2018, paste(group, case, sep = "-"))
  default_AB_Bry <- default_times(design = "CMB", cluster = group, case = case, phase = treatment, session = session, data = Bryant2018)
  default_A_Bry <- default_AB_Bry$A
  default_B_Bry <- default_AB_Bry$B
  
  cluster_base_last <- with(Bryant2018, tapply(session[treatment == "baseline"], clusterCase[treatment == "baseline"], max))
  cluster_trt_range <- with(Bryant2018, tapply(session[treatment == "treatment"], clusterCase[treatment == "treatment"], function(x) diff(range(x)) + 1))
  A_Bry <- min(cluster_base_last)
  B_Bry <- A_Bry + min(cluster_trt_range[which(cluster_base_last == min(cluster_base_last))])
  
  expect_equal(default_A_Bry, A_Bry)
  expect_equal(default_B_Bry, B_Bry)
  
})

test_that("The calc_BCSMD() works appropriately for treatment reversal designs.", {
  
  data(Anglesea)
  Ang_RML <- lme(fixed = outcome ~ 1 + condition,
                 random = ~ 1 | case,
                 correlation = corAR1(0.01, ~ session | case),
                 data = Anglesea)
  Ang_g <- g_mlm(Ang_RML, p_const = c(0,1), r_const = c(1,0,1))
    
  Ang_BCSMD <- calc_BCSMD(design = "TR",
                          case = case, phase = condition,
                          session = session, outcome = outcome,
                          FE_base = 0, RE_base = 0, FE_trt = 0,
                          summary = FALSE,
                          data = Anglesea)
  Ang_BCSMD$model <- NULL
  
  expect_equal(Ang_g, Ang_BCSMD, check.attributes = FALSE)
  
  expect_error(calc_BCSMD(design = "TR",
                          case = case, phase = condition,
                          session = session, outcome = outcome,
                          FE_base = c(0,1), RE_base = 0, FE_trt = 0,
                          summary = FALSE,
                          data = Anglesea))
  
  expect_error(calc_BCSMD(design = "TR",
                          case = case, phase = condition,
                          session = session, outcome = outcome,
                          FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = 0,
                          summary = FALSE,
                          data = Anglesea))
  
})
