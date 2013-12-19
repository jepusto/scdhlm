
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

parms <- expand.grid(phi = phi, rho = rho, tau1_ratio = tau1_ratio, tau_corr = tau_corr, m = m, n=n)
print(lengths <- c(length(phi), length(rho), length(tau1_ratio), length(tau_corr), length(m), length(n)))
prod(lengths)
dim(parms)
head(parms)
#--------------------------------------
# run simulations in serial
#--------------------------------------

library(plyr)
library(scdhlm)
set.seed(19810112)
system.time(MB2results <- maply(parms, .fun = simulate_MB2, 
                                iterations = iterations, beta = beta, .drop=FALSE, .progress = "text"))
attr(MB2results, "iterations") <- iterations
attr(MB2results, "beta") <- beta
save(MB2results, file="data/MB2-results.RData")


#--------------------------------------------------------
# run simulations in parallel on Windows via SNOW
#--------------------------------------------------------

library(plyr)
library(snow)
library(foreach)
library(iterators)
library(doSNOW)
library(rlecuyer)

cluster <- makeCluster(8, type = "SOCK")
registerDoSNOW(cluster)

# set up parallel random number generator
clusterSetupRNGstream(cluster, 20131003)

# execute simulations
system.time(MB2results <- maply(parms, .fun = simulate_MB2, 
                                iterations = iterations, beta = beta, 
                                .drop=FALSE, .parallel=TRUE,
                                .paropts = list(.packages="scdhlm")))
stopCluster(cluster)
attr(MB2results, "iterations") <- iterations
attr(MB2results, "beta") <- beta
save(MB2results, file="data/MB2-results.RData")


#-------------------------------------------------------------
# run simulations in parallel on Mac via multicore
#-------------------------------------------------------------

# set up multicore
library(parallel)
library(foreach)
library(iterators)
library(doParallel)
registerDoParallel(cores=detectCores())

library(plyr)
library(scdhlm)

# execute simulations
system.time(MB2results <- maply(parms, .fun = simulate_MB2, 
                                iterations = iterations, beta = beta, 
                                .drop=FALSE, .parallel=TRUE,
                                .paropts = list(.packages="scdhlm")))
attr(MB2results, "iterations") <- iterations
attr(MB2results, "beta") <- beta
save(MB2results, file="data/MB2-results.RData")

#--------------------------------------
# plot results
#--------------------------------------