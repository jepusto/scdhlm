set.seed(1)
#------------------------------------
# Design parameters
#------------------------------------
iterations <- 20000
beta <- c(0,1,0,0)
phi <- seq(-7L, 7L, 2) / 10L
rho <- seq(0.0, 0.8, 0.2)
tau1_ratio <- c(0.1, 0.5)
tau_corr <- 0
m <- 3:6
n <- c(8, 16)

print(lengths <- c(length(phi), length(rho), length(tau1_ratio), length(tau_corr), length(m), length(n)))
print(combos <- prod(lengths))
parms <- expand.grid(phi = phi, rho = rho, tau1_ratio = tau1_ratio, tau_corr = tau_corr, m = m, n=n)[sample(combos),]
head(parms)


#--------------------------------------------------------
# run simulations in parallel on Windows via SNOW
#--------------------------------------------------------

library(plyr)
library(parallel)
library(foreach)
library(iterators)
library(doSNOW)
library(rlecuyer)

cluster <- makeCluster(detectCores(), type = "SOCK")
registerDoSNOW(cluster)

# set up parallel random number generator
clusterSetupRNGstream(cluster, 20131003)

# execute simulations
system.time(MB2_array <- maply(parms, .fun = simulate_MB2, 
                                iterations = iterations, beta = beta, 
                                .drop=FALSE, .parallel=TRUE,
                                .paropts = list(.packages="scdhlm")))
stopCluster(cluster)


##------------------------------------------------
## reshape and save results
##------------------------------------------------
library(reshape)

names(dimnames(MB2_array))[7:8] <- c("stat","moment")
MB2results <- cast(melt(MB2_array), ... ~ moment)
attr(MB2results, "iterations") <- iterations
attr(MB2results, "beta") <- beta
save(MB2results, file="data/MB2results.RData", compress="xz")