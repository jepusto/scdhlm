
#------------------------------------
# Design parameters
#------------------------------------
iterations <- 10
beta <- c(0,1,0,0)
phi <- seq(-7L, 7L, 2) / 10L
rho <- seq(0.0, 0.8, 0.2)
m <- 3:6
n <- c(8, 16)
  
parms <- expand.grid(phi = phi, rho = rho, m = m, n=n)
print(lengths <- c(length(phi), length(rho), length(m), length(n)))
prod(lengths)
dim(parms)

#--------------------------------------
# run simulations in serial
#--------------------------------------

library(plyr)
library(scdhlm)
set.seed(20110325)
system.time(MB1results <- maply(parms, .fun = compare_RML_HPS, 
                             iterations = iterations, beta = beta, .drop=FALSE, .progress = "text"))
save(MB1results, file="data/MB1-results.RData")


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
system.time(MB1results <- maply(parms, .fun = compare_RML_HPS, 
                             iterations = iterations, beta = beta, 
                             .drop=FALSE, .parallel=TRUE,
                             .paropts = list(.packages="scdhlm")))
stopCluster(cluster)
save(MB1results, file="data/MB1-results.RData")


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
system.time(MB1results <- maply(parms, .fun = compare_RML_HPS, 
                                iterations = iterations, beta = beta, 
                                .drop=FALSE, .parallel=TRUE))

save(MB1results, file="data/MB1-results.RData")

#--------------------------------------
# plot results
#--------------------------------------