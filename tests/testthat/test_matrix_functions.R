
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
AR_block <- tapply(time_it, ID, \(x) AR1_corr(phi, x))
vcov_block <- tapply(time_it, ID, \(x) rho + (1 - rho) * AR1_corr(phi, x))
  
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
  
  expect_equal(
    block_minus_matrix(rev(vcov_block), Tau_full),
    matrix_minus_block(vcov_full, rev(Tau_block))  
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


test_that("AR1_ matrix construction works.", {
  expect_equivalent(
    AR_block, 
    AR1_corr_block(phi = phi, block = ID)  
  )
  
  AR_inv_block <- lapply(table(ID), AR1_corr_inv, phi = phi)
  AR_block_inv <- lapply(AR_block, solve)
  expect_equivalent(AR_inv_block, AR_block_inv)
  
})

