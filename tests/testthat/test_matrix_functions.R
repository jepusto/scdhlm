
# Construct vcov matrices and components
N <- 7
T_i <- 2 + rpois(N, lambda = 9)
ID <- rep(letters[1:N], T_i) |> factor(levels = rev(letters[1:N]))
time_it <- lapply(T_i, \(x) 1:x) |> unlist()
phi <- 0.8
rho <- 0.4

# As full matrices
Tau_full <- HLM_AR1_corr(id_fac = ID, time = time_it, phi = 0, rho = rho)
AR_full <- HLM_AR1_corr(id_fac = ID, time = time_it, phi = phi, rho = 0)
vcov_full <- HLM_AR1_corr(id_fac = ID, time = time_it, phi = phi, rho = rho)

# As block-diagonal matrices
Tau_block <- tapply(time_it, ID, \(x) diag(1 - rho, nrow = length(x)) + rho)
AR_block <- tapply(time_it, ID, \(x) phi^as.matrix(dist(x)))
vcov_block <- tapply(time_it, ID, \(x) rho + (1 - rho) * (phi^as.matrix(dist(x))))
  
test_that("block-matrix arithemetic works.", {
  
  # unblock
  
  expect_equal(Tau_full, unblock(Tau_block, block = ID))  
  expect_equal(AR_full, unblock(AR_block, block = ID))
  expect_equal(vcov_full, unblock(vcov_block, block = ID))
  
  # sum_blockblock
  
  expect_equal(
    Tau_full + AR_full, 
    unblock(sum_blockblock(Tau_block, AR_block), block = ID)
  )
  
  
  # prod_blockblock 
  
  prod_full <- Tau_full %*% AR_full
  prod_block <- prod_blockblock(Tau_block, AR_block)
  
  expect_equal(prod_full, unblock(prod_block, block = ID))
  
  # product_trace
  
  trace_full <- product_trace(Tau_full, AR_full)
  trace_block <- product_trace_blockblock(Tau_block, AR_block)
  
  expect_equal(trace_full, sum(trace_block))
  expect_equal(trace_full, sum(diag(prod_full)))
  expect_equal(trace_block, sapply(prod_block, \(x) sum(diag(x))))
  
  
  # block_minus_matrix and matrix_minus_block
  
  expect_equal(
    block_minus_matrix(vcov_block, Tau_full, block = ID),
    matrix_minus_block(vcov_full, Tau_block, block = ID)  
  )
  
  
  # prod_blockmatrix and prod_matrixblock
  
  expect_equal(
    prod_blockmatrix(Tau_block, AR_full, block = ID),
    prod_matrixblock(Tau_full, AR_block, block = ID)
  )
  
  expect_equal(
    prod_blockmatrix(Tau_block, AR_full, block = ID),
    unblock(prod_blockblock(Tau_block, AR_block), block = ID)
  )
  
  Tau_block_anon <- rev(Tau_block)
  names(Tau_block_anon) <- NULL
  AR_block_anon <- rev(AR_block)
  names(AR_block_anon) <- NULL
  
  expect_equal(
    prod_blockmatrix(Tau_block_anon, AR_full),
    prod_matrixblock(Tau_full, AR_block_anon)
  )
  
  prod_block <- prod_blockblock(Tau_block_anon, AR_block_anon)
  names(prod_block) <- 1:N
  
  expect_equal(
    prod_blockmatrix(Tau_block_anon, AR_full),
    unblock(prod_block)
  )
  
})


test_that("check_convergence() works.", {
  
  dat <- preprocess_SCD(design = "MBP", 
                        case = case, phase = treatment, session = time, outcome = outcome, 
                        center = 4, data = Laski)
  
  
  c_fit <- lme(
    fixed = outcome ~ time + treatment + time_trt,
    random = ~ 1 | case, 
    correlation = corAR1(0.01, ~ time | case), 
    data = dat,
    control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE)
  )
  
  c_fit_conv <- check_convergence(c_fit)
  expect_true(c_fit_conv)
  
  expect_warning({
    m_fit <- update(c_fit, random = ~ time | case)
    check_convergence(m_fit)
  })
  

})
