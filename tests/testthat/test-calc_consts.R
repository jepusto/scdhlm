skip_if_not_installed("nlme")

library(nlme)

test_that("calc_consts() error messages work", {
  
  expect_error(
    calc_consts(
      estimation = "HPS", design = "MBP", 
      center = 4, 
      FE_base = c(0,1), RE_base = c(0),
      FE_trt = c(0,1), RE_trt = c(1),
      corStruct = "AR1",
      varStruct = "hom",
      A = 4, B = 10
    )
  )
  
  expect_error(
    calc_consts(
      estimation = "RML", design = "MBP", 
      center = 4, 
      FE_base = c(0,1), RE_base = c(0),
      FE_trt = c(0,1), RE_trt = c(1),
      corStruct = "nothing",
      varStruct = "hom",
      A = 4, B = 10
    )
  )
  
  expect_error(
    calc_consts(
      estimation = "RML", design = "MBP", 
      center = 4, 
      FE_base = c(0,1), RE_base = c(0),
      FE_trt = c(0,1), RE_trt = c(1),
      corStruct = "IID",
      varStruct = "crazy",
      A = 4, B = 10
    )
  )

})

test_that("calc_consts() handles corStruct and varStruct", {
  
  FE_base <- c(0, sort(sample(1:4, 2)))
  RE_base <- c(0, sort(sample(FE_base[-1], 2)))
  RE_base2 <- sort(unique(c(0, sample(RE_base, 1))))
  FE_trt <- sort(sample(0:3, 2))
  RE_trt <- sample(FE_trt, 1)
  RE_trt2 <- RE_trt
  A <- sample(1:10, 1)
  B <- A + sample(5:20, 1)
  center <- sample(A:B, 1)
  
  structs <- 
    expand.grid(
      design = c("MBP","TR","RMBB", "CMB","CMBB"),
      corStruct = c("IID","AR1","MA1"),
      varStruct = c("hom","het")
    )
  
  consts <- mapply(
    calc_consts, 
    corStruct = structs$corStruct,
    varStruct = structs$varStruct,
    design = structs$design,
    estimation = "RML", 
    center = center, 
    FE_base = FE_base, RE_base = RE_base, RE_base_2 = RE_base2,
    FE_trt = FE_trt, RE_trt = RE_trt, RE_trt_2 = RE_trt,
    A = A, B = B,
    SIMPLIFY = FALSE
  )  

})