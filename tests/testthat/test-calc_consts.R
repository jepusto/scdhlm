skip_if_not_installed("nlme")

library(nlme)

test_that("calc_consts() error messages work", {
  
  expect_error(
    calc_consts(
      method = "HPS", design = "MBP", 
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
      method = "RML", design = "MBP", 
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
      method = "RML", design = "MBP", 
      center = 4, 
      FE_base = c(0,1), RE_base = c(0),
      FE_trt = c(0,1), RE_trt = c(1),
      corStruct = "IID",
      varStruct = "crazy",
      A = 4, B = 10
    )
  )

})

test_that("calc_consts() returns vectors with expected structure", {
  
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
      design = c("MBP","TR","RMBB","CMB"),
      corStruct = c("IID","AR1","MA1"),
      varStruct = c("hom","het")
    )
  
  consts <- mapply(
    calc_consts, 
    method = "RML", 
    design = structs$design,
    corStruct = structs$corStruct,
    varStruct = structs$varStruct,
    MoreArgs = list(
      center = center, 
      FE_base = FE_base, RE_base = RE_base, RE_base_2 = RE_base2,
      FE_trt = FE_trt, RE_trt = RE_trt, RE_trt_2 = RE_trt,
      A = A, B = B
    ),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )  

  structs$p_const <- lapply(consts, \(x) x$p_const)
  p_const <- c(rep(0L, length(FE_base)), (B - A)^FE_trt)
  expect_true(all(sapply(structs$p_const, identical, y = p_const)))
  
  structs$r_const <- lapply(consts, \(x) x$r_const)
  structs_simple <- subset(structs, design %in% c("MBP","TR"))
  rA <- with(structs_simple, unique(r_const[corStruct=="IID" & varStruct=="hom"])[[1]])
  rB <- with(structs_simple, unique(r_const[corStruct!="IID" & varStruct=="hom"])[[1]]) 
  rC <- with(structs_simple, unique(r_const[corStruct=="IID" & varStruct=="het"])[[1]])
  rD <- with(structs_simple, unique(r_const[corStruct!="IID" & varStruct=="het"])[[1]]) 
  expect_identical(rA, rB[-(length(rB)-1)])  
  expect_identical(rB, rC)  
  expect_identical(rC, rD[-(length(rD)-1)])  
  expect_identical(rA, rD[-(length(rD)-(1:2))])  

  structs_complex <- subset(structs, design %in% c("RMBB","CMB"))
  rG <- with(structs_complex, unique(r_const[corStruct=="IID" & varStruct=="hom"])[[1]])
  rH <- with(structs_complex, unique(r_const[corStruct!="IID" & varStruct=="hom"])[[1]]) 
  rI <- with(structs_complex, unique(r_const[corStruct=="IID" & varStruct=="het"])[[1]])
  rJ <- with(structs_complex, unique(r_const[corStruct!="IID" & varStruct=="het"])[[1]]) 
  expect_identical(rG, rH[-(length(rH)-1)])  
  expect_identical(rH, rI)  
  expect_identical(rI, rJ[-(length(rJ)-1)])  
  expect_identical(rG, rJ[-(length(rJ)-(1:2))])  

  r_dim1 <- length(RE_base2) * (length(RE_base2) + 1L) / 2L
  r_dim2 <- (length(RE_base2) + length(RE_trt2)) * (length(RE_base2) + length(RE_trt2) + 1L) / 2L
  r_dim3 <- length(RE_base) * (length(RE_base) + 1L) / 2L
  r_dim4 <- (length(RE_base) + length(RE_trt)) * (length(RE_base) + length(RE_trt) + 1L) / 2L

  structs_simple$rconst_base <- lapply(structs_simple$r_const, \(x) x[1:r_dim3])  
  structs_simple$rconst_trt <- lapply(structs_simple$r_const, \(x) x[(r_dim3 + 1):r_dim4])  
  
  structs_complex$rconst_base2 <- lapply(structs_complex$r_const, \(x) x[1:r_dim1])  
  structs_complex$rconst_trt2 <- lapply(structs_complex$r_const, \(x) x[(r_dim1 + 1):r_dim2])  
  structs_complex$rconst_base <- lapply(structs_complex$r_const, \(x) x[r_dim2 + 1:r_dim3])
  structs_complex$rconst_trt <- lapply(structs_complex$r_const, \(x) x[r_dim2 + (1+r_dim3):r_dim4])
  
  if (B != center) {
    expect_gt(min(unlist(structs_simple$rconst_base)), 0)
    expect_gt(min(unlist(structs_complex$rconst_base)), 0)
    expect_gt(min(unlist(structs_complex$rconst_base2)), 0)
  }
  
  expect_identical(min(unlist(structs_simple$rconst_trt)), 0)
  expect_identical(max(unlist(structs_simple$rconst_trt)), 0)
  expect_identical(min(unlist(structs_complex$rconst_trt)), 0)
  expect_identical(max(unlist(structs_complex$rconst_trt)), 0)
  expect_identical(min(unlist(structs_complex$rconst_trt2)), 0)
  expect_identical(max(unlist(structs_complex$rconst_trt2)), 0)
  
})
