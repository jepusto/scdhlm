skip_if_not_installed("nlme")

# check if calc_BCSMD() works

library(nlme)

test_that("The logic for the A, B, and D argument in calc_BCSMD() is correct.", {
  
  data(Laski)
  
  # use the default_times function to calculate default A and B
  default_AB <- default_times(design = "MBP", case = case, phase = treatment, session = time, data = Laski)
  default_AB_2 <- default_times(design = "MBP", case = Laski$case, phase = Laski$treatment, session = Laski$time)
  expect_equal(default_AB, default_AB_2)
  default_A <- default_AB$A
  default_B <- default_AB$B
  
  # !is.null(B) & !is.null(D), stop and show error
  expect_error(
    calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome,
               A = NULL, B = 13, D = 9, data = Laski)
  )
  
  expect_error(
    calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome,
               A = 4, B = 13, D = 9, data = Laski)
  )
  
  # is.null(A) & is.null(B) & is.null(D), get default A and B
  Las1 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, session = time, outcome = outcome, data = Laski)
  expect_equal(Las1$`Initial treatment time`, default_A)
  expect_equal(Las1$`Follow-up time`, default_B)
  
  # is.null(A) & !is.null(B) & is.null(D), get default A and specified B
  Las2 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, 
                     session = time, outcome = outcome, 
                     B = 12, data = Laski)
  expect_equal(Las2$`Initial treatment time`, default_A)
  expect_equal(Las2$`Follow-up time`, 12)
  
  # is.null(A) & is.null(B) & !is.null(D), get default A, B = A + D
  Las3 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, 
                     session = time, outcome = outcome, 
                     D = 8, data = Laski)
  expect_equal(Las3$`Initial treatment time`, default_A)
  expect_equal(Las3$`Follow-up time`, default_A + 8)
  
  # !is.null(A) & is.null(B) & !is.null(D), get specified A, B = A + D
  Las4 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, 
                     session = time, outcome = outcome, 
                     A = 3, D = 8, data = Laski)
  expect_equal(Las4$`Initial treatment time`, 3)
  expect_equal(Las4$`Follow-up time`, 3 + 8)
  
  # !is.null(A) & !is.null(B) & is.null(D), get specified A and specified B
  Las5 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, 
                     session = time, outcome = outcome, 
                     A = 3, B = 11, data = Laski)
  expect_equal(Las5$`Initial treatment time`, 3)
  expect_equal(Las5$`Follow-up time`, 11)
  
  # !is.null(A) & is.null(B) & is.null(D), get specified A and default B
  Las6 <- calc_BCSMD(design = "MBP", case = case, phase = treatment, 
                     session = time, outcome = outcome, 
                     A = 3, data = Laski)
  expect_equal(Las6$`Initial treatment time`, 3)
  expect_equal(Las6$`Follow-up time`, default_B)
  
  # if centered
  Las_c <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment, 
               session = time, outcome = outcome, 
               center = 4, 
               FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
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
  Ang1_RML <- lme(fixed = outcome ~ 1 + condition,
                  random = ~ 1 | case,
                  correlation = corAR1(0.01, ~ session | case),
                  data = Anglesea)
  Ang1_g <- g_mlm(Ang1_RML, p_const = c(0,1), r_const = c(1,0,1))
    
  Ang1_BCSMD <- calc_BCSMD(design = "TR",
                           case = case, phase = condition,
                           session = session, outcome = outcome,
                           FE_base = 0, RE_base = 0, FE_trt = 0,
                           summary = FALSE,
                           data = Anglesea)
  Ang1_BCSMD$model <- NULL
  
  expect_equal(Ang1_g, Ang1_BCSMD, check.attributes = FALSE)

  Ang2_RML <- lme(fixed = outcome ~ 1 + condition,
                  random = ~ condition | case,
                  correlation = corAR1(0.01, ~ session | case),
                  data = Anglesea)
  Ang2_g <- g_mlm(Ang2_RML, p_const = c(0,1), r_const = c(1,0,0,0,1))
  
  Ang2_BCSMD <- calc_BCSMD(design = "TR",
                           case = case, phase = condition,
                           session = session, outcome = outcome,
                           FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = 0,
                           summary = FALSE,
                           data = Anglesea)
  Ang2_BCSMD$model <- NULL
  
  expect_equal(Ang2_g, Ang2_BCSMD, check.attributes = FALSE)
  
  expect_error(calc_BCSMD(design = "TR",
                          case = case, phase = condition,
                          session = session, outcome = outcome,
                          FE_base = c(0,1), RE_base = 0, FE_trt = 0,
                          summary = FALSE,
                          data = Anglesea))
  
  expect_error(calc_BCSMD(design = "TR",
                          case = case, phase = condition,
                          session = session, outcome = outcome,
                          FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = 1,
                          summary = FALSE,
                          data = Anglesea))
  
})


test_that("calc_BCSMD() returns same result as g_mlm() for Laski data.", {
  
  data(Laski)
  
  # simple model
  Laski_RML1 <- lme(fixed = outcome ~ treatment, 
                    random = ~ 1 | case, 
                    correlation = corAR1(0.01, ~ time | case), 
                    data = Laski)
  Laski_g1_mlm <- g_mlm(Laski_RML1, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE)
  
  ## g_mlm VS calc_BCSMD
  Laski_calc_BCSMD <- calc_BCSMD(design = "MBP",
                                 case = case, phase = treatment,
                                 session = time, outcome = outcome,
                                 FE_base = 0, RE_base = 0, FE_trt = 0,
                                 summary = FALSE,
                                 data = Laski)
  Laski_calc_BCSMD$model <- NULL
  expect_equal(Laski_g1_mlm, Laski_calc_BCSMD, check.attributes = FALSE)
  
  Laski_calc_BCSMD_summary <- calc_BCSMD(design = "MBP",
                                         case = case, phase = treatment,
                                         session = time, outcome = outcome,
                                         FE_base = 0, RE_base = 0, FE_trt = 0,
                                         data = Laski)
  expect_equal(Laski_g1_mlm$g_AB, Laski_calc_BCSMD_summary$`BC-SMD estimate`)
  expect_equal(Laski_g1_mlm$SE_g_AB, Laski_calc_BCSMD_summary$`Std. Error`)
  expect_equal(Laski_g1_mlm$nu, Laski_calc_BCSMD_summary$`Degrees of freedom`)
  
  
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
  Laski_g2_mlm <- suppressWarnings(g_mlm(Laski_RML2, p_const = c(0,0,1,D), r_const = c(1,18,81,0,1), returnModel = TRUE))
  
  
  ## g_mlm VS calc_BCSMD
  Laski_BCSMD2 <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment,
               session = time, outcome = outcome, center = 4,
               FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
               summary = FALSE,
               data = Laski)
  )
  
  Laski_BCSMD2$model <- NULL
  expect_equal(Laski_g2_mlm, Laski_BCSMD2, check.attributes = FALSE)
  
  Laski_BCSMD2_summary <- suppressWarnings(
    calc_BCSMD(design = "MBP", case = case, phase = treatment,
               session = time, outcome = outcome, center = 4,
               FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
               data = Laski)
  )
  
  expect_equal(Laski_g2_mlm$g_AB, Laski_BCSMD2_summary$`BC-SMD estimate`)
  expect_equal(Laski_g2_mlm$SE_g_AB, Laski_BCSMD2_summary$`Std. Error`)
  expect_equal(Laski_g2_mlm$nu, Laski_BCSMD2_summary$`Degrees of freedom`)
  
})


test_that("calc_BCSMD() returns the same result as g_mlm() for Bryant 2018 data.", {
  
  data(Bryant2018)
  Bryant_RML1 <- lme(fixed = outcome ~ treatment,
                     random = ~ 1 | group/case,
                     correlation = corAR1(0.01, ~ session | group/case),
                     data = Bryant2018)
  
  # simple model
  suppressWarnings(expect_error(g_REML(Bryant_RML1, p_const = c(0, 1), r_const = c(1, 0, 1, 1)))) # g_REML not available for 3-level data
  Bry_g1_mlm <- g_mlm(Bryant_RML1, p_const = c(0, 1), r_const = c(1, 1, 0, 1), infotype = "expected", returnModel = TRUE)
  
  Bry_BCSMD <- calc_BCSMD(design = "CMB",
                          cluster = group, case = case, phase = treatment,
                          session = session, outcome = outcome,
                          treatment_name = "treatment",
                          FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                          summary = FALSE,
                          data = Bryant2018)
  Bry_BCSMD$model <- NULL
  expect_equal(Bry_g1_mlm, Bry_BCSMD, check.attributes = FALSE)
  
  Bry_BCSMD_summary <- calc_BCSMD(design = "CMB",
                                  cluster = group, case = case, phase = treatment,
                                  session = session, outcome = outcome,
                                  treatment_name = "treatment",
                                  FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
                                  data = Bryant2018)
  expect_equal(Bry_g1_mlm$g_AB, Bry_BCSMD_summary$`BC-SMD estimate`)
  expect_equal(Bry_g1_mlm$SE_g_AB, Bry_BCSMD_summary$`Std. Error`)
  expect_equal(Bry_g1_mlm$nu, Bry_BCSMD_summary$`Degrees of freedom`)
  
  expect_equal(CI_g(Bry_g1_mlm)[1], Bry_BCSMD_summary$`95% CI (lower)`)
  expect_equal(CI_g(Bry_g1_mlm)[2], Bry_BCSMD_summary$`95% CI (upper)`)
  

  # complex model
  default_AB <- default_times(design = "CMB",
                              cluster = group, case = case, phase = treatment, session = session, 
                              data = Bryant2018)
  D <- default_AB$B - default_AB$A
  dat <- preprocess_SCD(design = "CMB", 
                        cluster = group, case = case, phase = treatment,
                        session = session, outcome = outcome, center = default_AB$B,
                        data = Bryant2018)
  Bry_RML2 <- suppressWarnings(lme(fixed = outcome ~ session + treatment + session_trt,
                                   random = list(group = ~ 1, case = ~ session + session_trt), 
                                   correlation = corAR1(0.01, ~ session | group/case),
                                   data = dat,
                                   control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE)))
  Bry_g2_mlm <- g_mlm(Bry_RML2, p_const = c(0,0,1,D), r_const = c(1,1,0,0,0,0,0,0,1), 
                      infotype = "expected", returnModel = TRUE)
  
  Bry_BCSMD2 <- suppressWarnings(
    calc_BCSMD(design = "CMB",
               cluster = group, case = case, phase = treatment,
               session = session, outcome = outcome, center = default_AB$B,
               treatment_name = "treatment",
               FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0, 
               FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
               summary = FALSE,
               data = Bryant2018)
  )
  
  Bry_BCSMD2$model <- NULL
  expect_equal(Bry_g2_mlm, Bry_BCSMD2, check.attributes = FALSE)
  
  Bry_BCSMD2_summary <- suppressWarnings(
    calc_BCSMD(design = "CMB",
               cluster = group, case = case, phase = treatment,
               session = session, outcome = outcome, center = default_AB$B,
               treatment_name = "treatment",
               FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0, 
               FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
               data = Bryant2018)
  )
  
  expect_equal(Bry_g2_mlm$g_AB, Bry_BCSMD2_summary$`BC-SMD estimate`)
  expect_equal(Bry_g2_mlm$SE_g_AB, Bry_BCSMD2_summary$`Std. Error`)
  expect_equal(Bry_g2_mlm$nu, Bry_BCSMD2_summary$`Degrees of freedom`)
  expect_equal(CI_g(Bry_g2_mlm)[1], Bry_BCSMD2_summary$`95% CI (lower)`)
  expect_equal(CI_g(Bry_g2_mlm)[2], Bry_BCSMD2_summary$`95% CI (upper)`)
  
})


test_that("The batch_calc_BCSMD() works for MBP design.", {
  
  # single calculator
  data("Laski")
  
  # model 1: basic model without time trends
  Laski1_single <- 
    calc_BCSMD(
      design = "MBP",
      case = case, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      data = Laski
    )
  
  # model 2: varying intercepts, fixed treatment effects, varying trends
  Laski2_single <- 
    suppressWarnings(
      calc_BCSMD(
        design = "MBP",
        case = case, phase = treatment,
        session = time, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
        data = Laski)
    )
  
  data("AlberMorgan")
  
  Alber1_single <- 
    calc_BCSMD(
      design = "MBP",
      case = case, phase = condition,
      session = session, outcome = outcome,
      FE_base = 0, RE_base = 0, FE_trt = 0,
      data = AlberMorgan
    )
  
  Alber2_single <- 
    suppressWarnings(
      calc_BCSMD(
        design = "MBP",
        case = case, phase = condition,
        session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
        data = AlberMorgan)
    )

  # batch calculator
  Laski$studyID <- "Laski"
  names(Laski) <- c("case", "outcome", "session", "condition", "studyID")
  AlberMorgan$studyID <- "AlberMorgan"
  AlberMorgan <- AlberMorgan[, c(1, 4, 3, 2, 5)]
  dat_MBP <- rbind(Laski, AlberMorgan)

  res_AB <- 
    dat_MBP %>% 
    dplyr::group_by(studyID) %>%
    dplyr::summarise(
      default_times(
        design = "MBP",
        case = case,
        phase = condition,
        session = session,
        cluster = NULL,
        series = NULL
      ),
      .groups = 'drop'
    ) %>% 
    dplyr::mutate(variable = rep(c("range", "A", "B"), length(unique(dat_MBP$studyID)))) %>% 
    dplyr::rename(value = `default_times(...)`)
  
  res_A <- res_AB %>% dplyr::filter(variable == "A") %>% dplyr::pull(value) %>% unlist() %>% as.vector()
  res_B <- res_AB %>% dplyr::filter(variable == "B") %>% dplyr::pull(value) %>% unlist() %>% as.vector()
  
  res1_batch <- 
    suppressWarnings(
      batch_calc_BCSMD(
        data = dat_MBP, grouping = studyID, design = "MBP",
        case = case, phase = condition, session = session, outcome = outcome,
        FE_base = 0, RE_base = 0, FE_trt = 0
      ) 
    )
  
  expect_equal(res1_batch$`Initial treatment time`, res_A)
  expect_equal(res1_batch$`Follow-up time`, res_B)

  res2_batch <- 
    suppressWarnings(
      batch_calc_BCSMD(
        data = dat_MBP, grouping = studyID, design = "MBP",
        case = case, phase = condition, session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1)
      )
    )
  
  # model 1
  expect_equal(Laski1_single, as.data.frame(res1_batch[2, 2:12], ))
  expect_equal(Alber1_single, as.data.frame(res1_batch[1, 2:12], ))
  
  # model 2
  expect_equal(Laski2_single, as.data.frame(res2_batch[2, 2:12], ))
  expect_equal(Alber2_single, as.data.frame(res2_batch[1, 2:12], ))
  
  
  # with specified A, B
  Alber3_single <- 
    suppressWarnings(
      calc_BCSMD(
        design = "MBP",
        case = case, phase = condition,
        session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1), A = 5, B = 12,
        data = AlberMorgan)
    )
  
  res3_batch <- 
    suppressWarnings(
      batch_calc_BCSMD(
        data = dat_MBP, grouping = studyID, design = "MBP",
        case = case, phase = condition, session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1), A = 5, B = 12
      )
    )
  
  expect_equal(Alber3_single, as.data.frame(res3_batch[1, 2:12], ))
  expect_equal(res3_batch$`Initial treatment time`[1], 5)
  expect_equal(res3_batch$`Follow-up time`[1], 12)

  # with specified D
  Alber4_single <- 
    suppressWarnings(
      calc_BCSMD(
        design = "MBP",
        case = case, phase = condition,
        session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1), A = 5, B = 14,
        data = AlberMorgan)
    )
  
  res4_batch <- 
    suppressWarnings(
      batch_calc_BCSMD(
        data = dat_MBP, grouping = studyID, design = "MBP",
        case = case, phase = condition, session = session, outcome = outcome,
        FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1), D = 9
      )
    )
  
  expect_equal(Laski2_single, as.data.frame(res4_batch[2, 2:12], ))
  expect_equal(Alber4_single, as.data.frame(res4_batch[1, 2:12], ))
  
  expect_equal(res4_batch$`Initial treatment time`, res_A)
  expect_equal(res4_batch$`Follow-up time`, res_A + 9)
  
})
  

test_that("The batch_calc_BCSMD() works for RMBB design.", {
  
  data(Thiemann2001)
  data(Thiemann2004)
  dat_RMBB <- rbind(Thiemann2001, Thiemann2004)
  
  # basic model without trends
  
  Thi1_single_2001 <- 
    calc_BCSMD(
      design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
      data = Thiemann2001
    )
  
  Thi1_single_2004 <- 
    calc_BCSMD(
      design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
      data = Thiemann2004
    )
  
  Thi1_batch <- 
    batch_calc_BCSMD(
      data = dat_RMBB, grouping = Study_ID, design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0
    )
  
  expect_equal(Thi1_single_2001, as.data.frame(Thi1_batch[1, 2:12], ))
  expect_equal(Thi1_single_2004, as.data.frame(Thi1_batch[2, 2:12], ))
  
  # complex model: 
  # varying intercepts at series and case level, varying trends at series level, fixed treatment effects
  
  Thi2_single_2001 <- 
    calc_BCSMD(
      design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0, FE_trt = c(0,1), 
      data = Thiemann2001
    )
  
  Thi2_single_2004 <- 
    calc_BCSMD(
      design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0, FE_trt = c(0,1), 
      data = Thiemann2004
    )
  
  Thi2_batch <- 
    batch_calc_BCSMD(
      data = dat_RMBB, grouping = Study_ID, design = "RMBB",
      case = case, series = series, phase = treatment,
      session = time, outcome = outcome,
      FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0, FE_trt = c(0,1)
    )
  
  expect_equal(Thi2_single_2001, as.data.frame(Thi2_batch[1, 2:12], ))
  expect_equal(Thi2_single_2004, as.data.frame(Thi2_batch[2, 2:12], ))
  
})
