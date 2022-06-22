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


data("Anglesea")
data("AlberMorgan")
data("Laski")
data("Thiemann2001")
data("Bryant2018")

Laski_RML <- lme(fixed = outcome ~ 1 + treatment,
                 random = ~ 1 | case, 
                 correlation = corAR1(0, ~ time | case), 
                 data = Laski)

Thiemann2001_RML <- lme(outcome ~ 1 + time_c + treatment + trt_time,
                        random = ~ 1 | case / series,
                        data = Thiemann2001)
  
Bryant2018_RML <-lme(fixed = outcome ~ treatment,
                     random = ~ 1 | school / case,
                     correlation = corAR1(0, ~ session | school / case),
                     weights = varIdent(form = ~ 1 | treatment),
                     data = Bryant2018)

test_that("graph is a ggplot2 graph", {
  
  Ang_graph1 <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, 
                        design="TR", treatment_name = NULL, model_fit=NULL, data=Anglesea)
  expect_s3_class(Ang_graph1, "ggplot")
  
  Ang_graph2 <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, 
                         design="TR", treatment_name = "treatment", model_fit=NULL, data=Anglesea)
  expect_s3_class(Ang_graph2, "ggplot")
  
  Ang_graph3 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, 
                         outcome=Anglesea$outcome, design="TR", treatment_name = "treatment", model_fit=NULL)
  expect_s3_class(Ang_graph3, "ggplot")
  
  Ang_graph4 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, 
                         outcome=Anglesea$outcome, design="TR", model_fit=NULL)
  expect_s3_class(Ang_graph4, "ggplot")
  
  keys <- setdiff(names(Ang_graph1), c("plot_env", "labels"))
  expect_equal(Ang_graph1$keys, Ang_graph2$keys)
  expect_equal(Ang_graph1$keys, Ang_graph3$keys)
  expect_equal(Ang_graph1$keys, Ang_graph4$keys)
  
  
  Laski_graph1 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                         design="MBP", treatment_name = "treatment", model_fit=Laski_RML, data=Laski)
  expect_s3_class(Laski_graph1, "ggplot")
  
  Laski_graph2 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                         design="MBP", treatment_name = "treatment", model_fit=Laski_RML)
  expect_s3_class(Laski_graph2, "ggplot")
  
  expect_equal(Laski_graph1$keys, Laski_graph2$keys)
  
})

