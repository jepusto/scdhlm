context("(AB)^k designs")

test_that("effect_size_ABk works for BAB designs", {
  data(Carson)
  CarsonHPS <- with(Carson, effect_size_ABk(outcome, treatment, case, phase, time))
  expect_true(is.list(CarsonHPS))
})
