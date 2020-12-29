context("Graphing functions")


## Actual Test

data("Anglesea")
data("AlberMorgan")
data("Laski")

Laski_RML <- lme(fixed = outcome ~ 1 + treatment,
                 random = ~ 1 | case, 
                 correlation = corAR1(0, ~ time | case), 
                 data = Laski)


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
                         design="MB", treatment_name = "treatment", model_fit=Laski_RML, data=Laski)
  expect_s3_class(Laski_graph1, "ggplot")
  
  Laski_graph2 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, 
                         design="MB", treatment_name = "treatment", model_fit=Laski_RML)
  expect_s3_class(Laski_graph2, "ggplot")
  
  expect_equal(Laski_graph1$keys, Laski_graph2$keys)
  
})

