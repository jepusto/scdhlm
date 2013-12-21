set.seed(1)
#------------------------------------
# Design parameters
#------------------------------------
iterations <- 10000
beta <- c(0,1,0,0)
phi <- seq(-7L, 7L, 2) / 10L
rho <- seq(0.0, 0.8, 0.2)
tau2_ratio <- c(0.1, 0.5)
tau_corr <- 0
p_const <- c(0,1,0,7)
m <- c(3,4,5,6,9,12)
n <- c(8, 16)

print(lengths <- c(length(phi), length(rho), length(tau2_ratio), length(tau_corr), length(m), length(n)))
print(combos <- prod(lengths))
parms <- expand.grid(phi = phi, rho = rho, tau2_ratio = tau2_ratio, tau_corr = tau_corr, m = m, n=n)[sample(combos),]
head(parms)


# #--------------------------------------------------------
# # run simulations in parallel on Windows via SNOW
# #--------------------------------------------------------
# 
# library(plyr)
# library(snow)
# library(foreach)
# library(iterators)
# library(doSNOW)
# library(rlecuyer)
# 
# cluster <- makeCluster(4, type = "SOCK")
# registerDoSNOW(cluster)
# 
# # set up parallel random number generator
# clusterSetupRNGstream(cluster, 20131003)
# 
# # execute simulations
# system.time(MB4_array <- maply(parms, .fun = simulate_MB4, 
#                                 iterations = iterations, beta = beta, p_const = p_const,
#                                 .drop=FALSE, .parallel=TRUE,
#                                 .paropts = list(.packages="scdhlm")))
# stopCluster(cluster)
# 
# 
# ##------------------------------------------------
# ## reshape and save results
# ##------------------------------------------------
# 
# library(reshape)
# 
# names(dimnames(MB4_array))[7:8] <- c("stat","moment")
# MB4results <- cast(melt(MB4_array), ... ~ moment)
# attr(MB4results, "iterations") <- iterations
# attr(MB4results, "beta") <- beta
# attr(MB4results, "p_const") <- p_const
# save(MB4results, file="data/MB4results.RData", compress="xz")


#--------------------------------------
# Analyze results
#--------------------------------------
data(MB4results)