context("Graphing functions")


## Actual Test


test_that("graph_SCD works with example dataset.", {
  
  skip(message = "Auxiliary dataset not included in package.")
  
  Kattenberg <- read.csv("auxilliary/Kattenberg-data.csv")
  
  Kat_graph <- graph_SCD(case = ID_participant, 
                         phase = Daytype,
                         session = Workday,
                         outcome = SRL_SCORE_MEAN,
                         design = "TR",
                         data = Kattenberg)  
  
  expect_s3_class(Kat_graph, "ggplot")
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
  
  keys <- setdiff(names(Laski_graph1), c("plot_env", "labels"))
  expect_equal(Laski_graph1[keys], Laski_graph2[keys])
  
})

test_that("graph_SCD works for design = 'RMBB'", {
  
  skip_if_not_installed("ggplot2")

  data("Thiemann2001")
  Thiemann2001_RML <- lme(outcome ~ 1 + time_c + treatment + trt_time,
                          random = ~ 1 | case / series,
                          data = Thiemann2001)
  
  # graph using data = 
  # graph using vectors only
  # graph with model_fit = Thiemann2001_RML (without data)
  
})

test_that("graph_SCD works for design = 'CMB'", {
  
  skip_if_not_installed("ggplot2")
  
  data("Bryant2018")
  
  Bryant2018_RML <-lme(fixed = outcome ~ treatment,
                       random = ~ 1 | school / case,
                       correlation = corAR1(0, ~ session | school / case),
                       weights = varIdent(form = ~ 1 | treatment),
                       data = Bryant2018)

  # graph using data = 
  # graph using vectors only
  # graph with model_fit = Bryant2018_RML (without data)
  
})




