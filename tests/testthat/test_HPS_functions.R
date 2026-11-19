
data(Thorne)
data(Lambert)
Lambert <- subset(Lambert, measure == "disruptive behavior")

test_that("Order doesn't matter for ABk designs", {
  
  Lambert_orig <- with(Lambert, effect_size_ABk(outcome, treatment, case, phase, time))
  Lambert_new <- with(Lambert[sample.int(nrow(Lambert)),], effect_size_ABk(outcome, treatment, case, phase, time))
  expect_identical(Lambert_orig, Lambert_new)
  
  Anglesea_orig <- with(Anglesea, effect_size_ABk(outcome, condition, case, phase, session))
  Anglesea_new <- with(Anglesea[sample.int(nrow(Anglesea)),], effect_size_ABk(outcome, condition, case, phase, session))
  expect_identical(Anglesea_orig, Anglesea_new)
  
  Thorne$phase_pair <- with(Thorne, substr(phase_id, 2, 2))
  Thorne_A <- subset(Thorne, measure == "Academic Engagement")
  Thorne_A_orig <- with(Thorne_A, effect_size_ABk(outcome, phase_indicator, case, phase_pair, session))
  Thorne_A_new <- with(Thorne_A[sample.int(nrow(Thorne_A)),], effect_size_ABk(outcome, phase_indicator, case, phase_pair, session))
  expect_identical(Thorne_A_orig, Thorne_A_new)
  
  Thorne_B <- subset(Thorne, measure == "Inappropriate Verbalizations")
  Thorne_B_orig <- with(Thorne_B, effect_size_ABk(outcome, phase_indicator, case, phase_pair, session))
  Thorne_B_new <- with(Thorne_B[sample.int(nrow(Thorne_B)),], effect_size_ABk(outcome, phase_indicator, case, phase_pair, session))
  expect_identical(Thorne_B_orig, Thorne_B_new)
  
})

test_that("Order doesn't matter for multiple baseline designs", {
  
  Saddler_1 <- subset(Saddler, measure=="writing quality")
  Saddler_orig <- with(Saddler_1, effect_size_MB(outcome, treatment, case, time))
  Saddler_new <- with(Saddler_1[sample.int(nrow(Saddler_1)),], effect_size_MB(outcome, treatment == "treatment", case, time))
  expect_identical(Saddler_orig, Saddler_new)
  
  Laski_orig <- with(Laski, effect_size_MB(outcome, treatment, case, time))
  Laski_new <- with(Laski[sample.int(nrow(Laski)),], effect_size_MB(outcome, treatment == "treatment", case, time))
  expect_identical(Laski_orig, Laski_new)
  
})

test_that("MB and ABk give similar results", {
  
  S1_MB <- with(subset(Saddler, measure=="writing quality"), effect_size_MB(outcome, treatment == "treatment", case, time))
  S1_ABk <- with(subset(Saddler, measure=="writing quality"), effect_size_ABk(outcome, treatment, case, rep(1, length(outcome)), time))
  expect_equal(S1_MB$D_bar, S1_ABk$D_bar)
  
  Laski_MB <- with(Laski, effect_size_MB(outcome, treatment == "treatment", case, time))
  Laski_ABk <- with(Laski, effect_size_ABk(outcome, treatment, case, rep(1, length(outcome)), time))
  expect_equal(Laski_MB$D_bar, Laski_ABk$D_bar)
  
})

test_that("summary and print work for effect_size_ABK() and effect_size_MB() objects", {
  Lambert_ES <- effect_size_ABk(outcome = outcome, treatment = treatment, id = case, 
                                phase = phase, time = time, data= Lambert)
  expect_output(summary(Lambert_ES))
  expect_output(print(Lambert_ES))
  
  quality_ES <- effect_size_MB(outcome, treatment, case, time, 
                               data= subset(Saddler, measure=="writing quality"))
  expect_output(summary(quality_ES))
  expect_output(print(quality_ES))
})

test_that("effect_size_ABk works for BAB designs", {
  data(Carson)
  CarsonHPS <- with(Carson, effect_size_ABk(outcome, treatment, case, phase, time))
  expect_true(is.list(CarsonHPS))
})

test_that("auto_SS() works.", {
  expect_equal(auto_SS(x = 4), rep(NA_real_, 2L))
  expect_type(auto_SS(rnorm(10)), "double")
  expect_length(auto_SS(rnorm(16)), 2L)
})

test_that("CI_g() works for HPS estimates.", {
  Lambert_ES <- effect_size_ABk(outcome, treatment, case, phase, time, data = Lambert)
  CI_sym <- CI_g(Lambert_ES)
  expect_length(CI_sym, 2L)
  expect_type(CI_sym, "double")
  
  CI_asym <- CI_g(Lambert_ES, symmetric = FALSE)
  expect_length(CI_asym, 2L)
  expect_type(CI_asym, "double")

  expect_all_false(CI_sym == CI_asym)  

})

test_that("effect_size_ABk works with treatment factors", {
  
})

test_that("effect_size_ABk works with id factors", {
  
})

test_that("effect_size_ABk works with phase factors", {
  
})

test_that("effect_size_MB works with treatment factors", {
  
})

test_that("effect_size_MB works with id factors", {
  
})

test_that("Vectorized version of effect_size_ABk returns identical results", {
  
})

test_that("Vectorized version of effect_size_MB returns identical results", {
  
})

