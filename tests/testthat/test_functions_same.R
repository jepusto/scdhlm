library(testthat)

data(Saddler)

Saddler_quality <- subset(Saddler, measure=="writing quality")



test_that("effect_size_MB results are the same as effect_size_MB_new for the Saddler data", {
  
  

  data1 <- effect_size_MB(Saddler_quality$outcome, Saddler_quality$treatment, Saddler_quality$case, Saddler_quality$time)
  
  data2 <- effect_size_MB(outcome, treatment, case, time, data = Saddler_quality)
  
  
  
 # data2 <- effect_size_MB_new(Saddler_quality$outcome, Saddler_quality$treatment, Saddler_quality$case, Saddler_quality$time)
  
 # data3 <- effect_size_MB_new(outcome, treatment, case, time, data = Saddler_quality)
  

    expect_equal(data1, data2)
 # expect_equal(data1, data3)
 
})




test_that("effect_size_ABk results are the same as effect_size_ABk_new for the Saddler data", {
  
  
  data(Anglesea)
  
  Anglesea <- as.data.frame(Anglesea)
  
  
  data3 <- effect_size_ABk(outcome = Anglesea$outcome, treatment = Anglesea$condition, id = Anglesea$case,phase = Anglesea$phase, time = Anglesea$session)
  
  data4 <- effect_size_ABk(data = Anglesea, outcome = outcome, treatment = condition, id = case, phase = phase, time = session)
  
  
 # data4 <- effect_size_ABk_new(data = Anglesea, outcome = outcome, treatment = condition, id = case, phase = phase, time = session)
  
  expect_equal(data3, data4)
  
  
  
})
  
  
  

