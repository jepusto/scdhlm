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
  
  my_graph <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, design="TR", treatment_name = NULL, model_fit=NULL,  data=Anglesea)
  expect_s3_class(my_graph, "ggplot")
  
  my_graph1 <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, design="TR", treatment_name = "treatment", model_fit=NULL,  data=Anglesea)
  expect_s3_class(my_graph1, "ggplot")
  
  expect_equal(my_graph$data, my_graph1$data)
  expect_equal(my_graph$mapping, my_graph1$mapping)
  expect_equal(my_graph$theme, my_graph1$theme)
  
  my_graph2 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, outcome=Anglesea$outcome, design="TR", treatment_name = "treatment",model_fit=NULL)
  expect_s3_class(my_graph2, "ggplot")
  
  my_graph3 <- graph_SCD(case=Anglesea$case, phase=Anglesea$condition, session=Anglesea$session, outcome=Anglesea$outcome, design="TR", model_fit=NULL)
  expect_s3_class(my_graph3, "ggplot")
  
  expect_equal(my_graph2$data, my_graph3$data)
  expect_equal(my_graph2$mapping, my_graph3$mapping)
  expect_equal(my_graph2$theme, my_graph3$theme)
  
  my_graph4 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, design="MB", treatment_name = "treatment", model_fit=Laski_RML,  data=Laski)
  expect_s3_class(my_graph4, "ggplot")
  
  my_graph5 <- graph_SCD(case=case, phase=treatment, session=time, outcome=outcome, design="MB", treatment_name = "treatment", model_fit=Laski_RML)
  expect_s3_class(my_graph5, "ggplot")
  
  expect_equal(my_graph4$data, my_graph5$data)
  expect_equal(my_graph4$mapping, my_graph5$mapping)
  expect_equal(my_graph4$theme, my_graph5$theme)
  
})

