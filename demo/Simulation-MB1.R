set.seed(1)
#------------------------------------
# Design parameters
#------------------------------------
iterations <- 20
beta <- c(0,1,0,0)
phi <- seq(-7L, 7L, 2) / 10L
rho <- seq(0.0, 0.8, 0.2)
m <- 3:6
n <- c(8, 16)

print(lengths <- c(length(phi), length(rho), length(m), length(n)))
print(combos <- prod(lengths))
parms <- expand.grid(phi = phi, rho = rho, m = m, n=n)[sample(combos),]
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
clusterSetupRNGstream(cluster, 20131220)

# execute simulations
system.time(MB1_array <- maply(parms, .fun = compare_RML_HPS, 
                             iterations = iterations, beta = beta, 
                             .drop=FALSE, .parallel=TRUE,
                             .paropts = list(.packages="scdhlm")))
stopCluster(cluster)


##------------------------------------------------
## reshape and save results
##------------------------------------------------
library(reshape)

names(dimnames(MB1_array))[7:8] <- c("stat","moment")
dimnames(MB1_array)$stat[c(5,19)] <- c("kappa_RML","kappa_HPS")

MB1results <- cast(melt(MB1_array), ... ~ moment)
attr(MB1results, "iterations") <- iterations
attr(MB1results, "beta") <- beta
save(MB1results, file="data/MB1results.RData", compress="xz")
