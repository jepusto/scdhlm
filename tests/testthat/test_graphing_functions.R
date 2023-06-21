context("Graphing functions")

test_that("graph_SCD works for design = 'TR'", {
  
  skip_if_not_installed("ggplot2")

  Spriggs <- read.csv("../testdata/BC-SMD-Calculations.csv")  

  # Clean data
  dat <- preprocess_SCD(design = "TR", 
                        case = Participant, 
                        phase = Phase, 
                        session = Session, 
                        outcome = PercentageCorrect, 
                        round_session = FALSE, 
                        treatment_name = "B", 
                        data = Spriggs)
  
  # Fit the model
  fit_RML <- lme(fixed = PercentageCorrect ~ 1 + trt, 
                 random = ~ 1 | Participant, 
                 correlation = corAR1(0.01, ~ Session | Participant), 
                 data = dat,
                 control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  
  # Calculate effect size with g_mlm()
  p_const <- c(0,1)
  r_const <- c(1,0,1)
  
  ES_RML <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected")
  
  # Graph 
  p <- graph_SCD(design = "TR", 
                 case = Participant, phase = Phase, 
                 session = Session, outcome = PercentageCorrect, model_fit = fit_RML,  
                 data = dat)
  
  expect_s3_class(p, "ggplot")
  expect_invisible(print(p))
  
})

test_that("graph_SCD works for design = 'TR'", {
  
  skip_if_not_installed("ggplot2")
  
  data("Anglesea")
  
  Ang_graph1 <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, 
                        design="TR", treatment_name = NULL, model_fit=NULL, data=Anglesea)
  expect_s3_class(Ang_graph1, "ggplot")
  expect_invisible(print(Ang_graph1))
  
  Ang_graph2 <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, 
                         design="TR", treatment_name = "treatment", model_fit=NULL, data=Anglesea)
  expect_s3_class(Ang_graph2, "ggplot")
  expect_invisible(print(Ang_graph2))
  
  Ang_graph3 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, 
                          outcome=Anglesea$outcome, design="TR", treatment_name = "treatment", model_fit=NULL)
  expect_s3_class(Ang_graph3, "ggplot")
  expect_invisible(print(Ang_graph3))
  
  Ang_graph4 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, 
                         outcome=Anglesea$outcome, design="TR", model_fit=NULL)
  expect_s3_class(Ang_graph4, "ggplot")
  expect_invisible(print(Ang_graph4))
  
  Ang_case <- Anglesea$case
  Ang_condition <- Anglesea$condition
  Ang_session <- Anglesea$session
  Ang_outcome <- Anglesea$outcome
  
  Ang_graph5 <- graph_SCD(case=Ang_case, phase=Ang_condition, session=Ang_session, 
                          outcome=Ang_outcome, design="TR")
  expect_s3_class(Ang_graph5, "ggplot")
  expect_invisible(print(Ang_graph5))
  
  expect_equivalent(Ang_graph1$data, Ang_graph2$data)
  
  keys <- c("scales","theme","coordinates")
  expect_equal(Ang_graph1[keys], Ang_graph3[keys])
  expect_equal(Ang_graph1[keys], Ang_graph4[keys])
  expect_equal(Ang_graph1[keys], Ang_graph5[keys])
  
})

test_that("graph_SCD works for design = 'MBP'", {
  
  skip_if_not_installed("ggplot2")

  data("Laski")
  
  Laski_RML <- lme(fixed = outcome ~ 1 + treatment,
                   random = ~ 1 | case, 
                   correlation = corAR1(0, ~ time | case), 
                   data = Laski)

  Laski_graph1 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                            design="MBP", treatment_name = "treatment", model_fit=Laski_RML, data=Laski)
  expect_s3_class(Laski_graph1, "ggplot")
  expect_invisible(print(Laski_graph1))
  
  Laski_graph2 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                            design="MBP", treatment_name = "treatment", model_fit=Laski_RML)
  expect_s3_class(Laski_graph2, "ggplot")
  expect_invisible(print(Laski_graph2))
  
  keys <- setdiff(names(Laski_graph1), c("data","plot_env", "labels","layers"))
  expect_equal(Laski_graph1[keys], Laski_graph2[keys])
  

  Laski_clean <- preprocess_SCD(design = "MBP", case=case, phase=treatment, session=time, outcome=outcome, data = Laski)
  Laski_trend <- lme(fixed = outcome ~ 1 + time_trt,
                   random = ~ 1 | case, 
                   correlation = corAR1(0, ~ time | case), 
                   data = Laski_clean)
  
  Laski_graph3 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                            design="MBP", treatment_name = "treatment", model_fit=Laski_trend, data=Laski_clean)
  expect_s3_class(Laski_graph3, "ggplot")
  expect_invisible(print(Laski_graph3))
  
  Laski_graph4 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                            design="MBP", treatment_name = "treatment", model_fit=Laski_trend)
  expect_s3_class(Laski_graph4, "ggplot")
  expect_invisible(print(Laski_graph4))
  
  keys <- setdiff(names(Laski_graph1), c("plot_env", "labels","layers"))
  expect_equal(Laski_graph3[keys], Laski_graph4[keys])
  
})

test_that("graph_SCD works for design = 'RMBB'", {
  
  skip_if_not_installed("ggplot2")

  data("Thiemann2001")
  Thiemann2001_RML <- lme(outcome ~ 1 + time_c + treatment + trt_time,
                          random = ~ 1 | case / series,
                          data = Thiemann2001)
  
  # graph using data = 
  Thiemann_graph1 <- graph_SCD(design = "RMBB",
                               case = case, series = series,
                               phase = treatment, session = time, outcome = outcome,
                               treatment_name = "treatment", 
                               data = Thiemann2001)
  expect_s3_class(Thiemann_graph1, "ggplot")
  expect_invisible(print(Thiemann_graph1))
  
  # graph using vectors only
  Thiemann_graph2 <- graph_SCD(design = "RMBB",
                               case = Thiemann2001$case, series = Thiemann2001$series,
                               phase = Thiemann2001$treatment, session = Thiemann2001$time, 
                               outcome = Thiemann2001$outcome,
                               treatment_name = "treatment", model_fit = Thiemann2001_RML)
  expect_s3_class(Thiemann_graph2, "ggplot")
  expect_invisible(print(Thiemann_graph2))
  keys <- c("scales","theme","coordinates")
  expect_equal(Thiemann_graph1[keys], Thiemann_graph2[keys])
  
  # graph with model_fit = Thiemann2001_RML (with data)
  Thiemann_graph3 <- graph_SCD(design = "RMBB",
                               case = case, series = series,
                               phase = treatment, session = time, outcome = outcome,
                               treatment_name = "treatment", 
                               data = Thiemann2001, model_fit = Thiemann2001_RML)
  expect_s3_class(Thiemann_graph3, "ggplot")
  expect_invisible(print(Thiemann_graph3))
  
  # graph with model_fit = Thiemann2001_RML (without data)
  Thiemann_graph4 <- graph_SCD(design = "RMBB",
                               case = case, series = series,
                               phase = treatment, session = time, outcome = outcome,
                               treatment_name = "treatment", model_fit = Thiemann2001_RML)
  expect_s3_class(Thiemann_graph4, "ggplot")
  expect_invisible(print(Thiemann_graph4))
  
  keys <- setdiff(names(Thiemann_graph3), c("data","plot_env", "labels", "layers"))
  expect_equal(Thiemann_graph3[keys], Thiemann_graph4[keys])
  
})

test_that("graph_SCD works for design = 'CMB'", {
  
  skip_if_not_installed("ggplot2")
  
  data("Bryant2018")
  
  Bryant2018_RML <-lme(fixed = outcome ~ treatment,
                       random = ~ 1 | group / case,
                       correlation = corAR1(0, ~ session | group / case),
                       weights = varIdent(form = ~ 1 | treatment),
                       data = Bryant2018,
                       na.action = na.omit)

  # graph using data = 
  Bry_graph1 <- graph_SCD(design = "CMB",
                          cluster = group, case = case,
                          phase = treatment, session = session, outcome = outcome,
                          treatment_name = "treatment", 
                          data = Bryant2018)
  expect_s3_class(Bry_graph1, "ggplot")
  expect_invisible(print(Bry_graph1))
  
  # graph using vectors only
  Bry_graph2 <- graph_SCD(design = "CMB",
                          cluster = Bryant2018$group, case = Bryant2018$case,
                          phase = Bryant2018$treatment, session = Bryant2018$session, 
                          outcome = Bryant2018$outcome,
                          treatment_name = "treatment", model_fit = Bryant2018_RML)
  expect_s3_class(Bry_graph2, "ggplot")
  expect_invisible(print(Bry_graph2))
  keys <- c("scales","theme","coordinates")
  expect_equal(Bry_graph1[keys], Bry_graph2[keys])
  
  # graph with model_fit (with data)
  Bry_graph3 <- graph_SCD(design = "CMB",
                          cluster = group, case = case,
                          phase = treatment, session = session, outcome = outcome,
                          treatment_name = "treatment", 
                          data = Bryant2018,
                          model_fit = Bryant2018_RML)
  expect_s3_class(Bry_graph3, "ggplot")
  expect_invisible(print(Bry_graph3))
  
  # graph with model_fit (without data)
  Bry_graph4 <- graph_SCD(design = "CMB",
                          cluster = group, case = case,
                          phase = treatment, session = session, outcome = outcome,
                          treatment_name = "treatment", model_fit = Bryant2018_RML)
  expect_s3_class(Bry_graph4, "ggplot")
  expect_invisible(print(Bry_graph4))
  
  keys <- setdiff(names(Bry_graph3), c("data","plot_env", "labels", "layers"))
  expect_equal(Bry_graph3[keys], Bry_graph4[keys])
  
})


test_that("graph_SCD works with hypothetical newdata", {
  
  data("Anglesea")
  Anglesea_clean <- preprocess_SCD(design = "TR", 
                        case = case, 
                        phase = phase, 
                        session = session, 
                        outcome = outcome, 
                        data = Anglesea)
  
  # Fit the model
  suppressWarnings(
    Ang_RML <- lme(fixed = outcome ~ 1 + trt, 
                   random = ~ 1 + trt | case, 
                   correlation = corAR1(0.01, ~ session | case), 
                   data = Anglesea_clean,
                   control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
  )

  Ang_graph1 <- graph_SCD(case=case, phase=phase, session=session, outcome=outcome, 
                          design="TR", treatment_name = NULL, model_fit=Ang_RML)
  expect_s3_class(Ang_graph1, "ggplot")
  expect_invisible(print(Ang_graph1))
  
  Anglesea_mod <- Anglesea_clean
  Anglesea_mod$phase <- factor(1, levels = levels(Anglesea_clean$phase))
  Anglesea_mod$trt <- 0
  
  Ang_graph2 <- graph_SCD(case=case, phase=phase, session=session, outcome=outcome, 
                          design="TR", treatment_name = NULL, newdata = Anglesea_mod, model_fit=Ang_RML)
  expect_s3_class(Ang_graph2, "ggplot")
  expect_invisible(print(Ang_graph2))
  
  Ang_graph3 <- graph_SCD(case=case, phase=phase, session=session, outcome=outcome, 
                          design="TR", treatment_name = NULL, data = Anglesea_clean, newdata = Anglesea_mod, model_fit=Ang_RML)
  expect_s3_class(Ang_graph3, "ggplot")
  expect_invisible(print(Ang_graph3))
  
  data("Laski")
  
  Laski_RML <- lme(fixed = outcome ~ 1 + treatment,
                   random = ~ 1 | case, 
                   correlation = corAR1(0, ~ time | case), 
                   data = Laski)
  Laski_mod <- Laski
  Laski_mod$treatment <- factor("baseline", levels = levels(Laski$treatment))
  
  Laski_graph <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                            design="MBP", treatment_name = "treatment", model_fit=Laski_RML, newdata=Laski_mod)
  expect_s3_class(Laski_graph, "ggplot")
  expect_invisible(print(Laski_graph))
  
  
  data("Thiemann2001")
  Thiemann2001_RML <- lme(outcome ~ 1 + time_c + treatment + trt_time,
                          random = ~ 1 | case / series,
                          data = Thiemann2001)
  Thiemann2001_mod <- Thiemann2001
  Thiemann2001_mod$treatment <- factor("baseline", levels = levels(Thiemann2001$treatment))
  Thiemann2001_mod$trt_time <- 0
  
  
  Thiemann_graph <- graph_SCD(design = "RMBB",
                               case = case, series = series,
                               phase = treatment, session = time, outcome = outcome,
                               treatment_name = "treatment", 
                               model_fit = Thiemann2001_RML,
                               newdata = Thiemann2001_mod)
  expect_s3_class(Thiemann_graph, "ggplot")
  expect_invisible(print(Thiemann_graph))
  
  data("Bryant2018")
  
  Bryant2018_RML <-lme(fixed = outcome ~ treatment,
                       random = ~ 1 | group / case,
                       correlation = corAR1(0, ~ session | group / case),
                       weights = varIdent(form = ~ 1 | treatment),
                       data = Bryant2018,
                       na.action = na.omit)
  Bryant2018_mod <- Bryant2018
  Bryant2018_mod$treatment <- factor("baseline", levels = levels(Bryant2018$treatment))
  
  Bry_graph <- graph_SCD(design = "CMB",
                          cluster = group, case = case,
                          phase = treatment, session = session, outcome = outcome,
                          treatment_name = "treatment", 
                          model_fit = Bryant2018_RML, newdata = Bryant2018_mod)
  expect_s3_class(Bry_graph, "ggplot")
  expect_invisible(print(Bry_graph))
  
})


