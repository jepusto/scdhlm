test_that("CI_SMD_single() respects the bound argument.", {
  
  CI_symmetric <- CI_SMD_symmetric(
    delta = 0.5, kappa = 1, nu = 4, V_delta = 0.5^2, 
    cover = 0.9
  )
  
  CI1 <- CI_SMD_single(
    delta = 0.5, kappa = 1, nu = 4, V_delta = 0.5^2, 
    cover = 0.9, bound = 35
  )
  
  CI2 <- CI_SMD_single(
    delta = 0.5, kappa = 1, nu = 4, V_delta = 0.5^2, 
    cover = 0.9, bound = 0.35
  )
  
  expect_equal(CI_symmetric, CI2)
  expect_all_false(CI1 == CI2)  
})

test_that("CI_SMD() works with vector arguments.", {

  set.seed(20261117)
  
  N <- 6
  delta_i <- rnorm(N, mean = 0.5, sd = 0.4)
  kappa_i <- rgamma(N, shape = 1.8, scale = 0.3)
  nu_i <- rpois(N, 8)

  nu_i_trunc <- pmax(nu_i, 2.001)
  V_delta_i <- kappa_i^2 * nu_i_trunc / (nu_i_trunc - 2) + delta_i^2 * (nu_i_trunc / (nu_i_trunc - 2) - 1 / J(nu_i_trunc)^2)
  
  CI_symmetric <- mapply(CI_SMD_symmetric,
    delta = delta_i, kappa = kappa_i, nu = nu_i_trunc, V_delta = V_delta_i,
    MoreArgs = list(cover = 0.9)
  )
  
  CI1 <- CI_SMD(
    delta = delta_i, kappa = kappa_i, nu = nu_i,
    cover = 0.9
  )
  
  CI2 <- CI_SMD(
    delta = delta_i, kappa = kappa_i, nu = nu_i,
    cover = 0.9, bound = 2
  )
  
  CIs_equal <- abs(CI1 - CI2) < 1e-6
  
  CI_sym_mod <- CI_symmetric * rbind(kappa_i, kappa_i)
  
  expect_equal(CI_sym_mod[!CIs_equal], CI2[!CIs_equal])
  expect_equal(CI1[CIs_equal], CI2[CIs_equal])  
  
  cover <- coverage(0.7, CI1)
  expect_identical(cover, CI1[1,] < 0.7 & 0.7 < CI1[2,])

  
})

