library(scdhlm)
rm(list=ls())

source("inst/shiny-examples/scdhlm/mappings.R")
source("inst/shiny-examples/scdhlm/graphing-functions.R")
source("inst/shiny-examples/scdhlm/helper-functions.R")
source("inst/shiny-examples/scdhlm/lme-fit.R")

datFile <- read.table("auxilliary/bc_smd_anxiety_2pm.txt", 
                      header=TRUE, sep="\t", quote="",
                      stringsAsFactors = FALSE)

phases <- levels(as.factor(datFile$phase))

case_vec <- datFile$case
caseID <- factor(case_vec, levels = unique(case_vec))
session <- as.numeric(datFile$session)
phaseID <- as.factor(datFile$phase)
outcome <- as.numeric(datFile$outcome)
dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
trt_phase <- phases[2]
dat$trt <- as.numeric(dat$phase==trt_phase)
dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))


FE_base <- c(0)
RE_base <- c(0)
FE_trt <- c(0)
RE_trt <- c(0)

lme_fit_TR(dat = dat, FE_base = FE_base, RE_base = RE_base,
           FE_trt = FE_trt, RE_trt = RE_trt, phi_init = 0)
lme_fit_TR(dat = dat, FE_base = FE_base, RE_base = RE_base,
           FE_trt = FE_trt, RE_trt = RE_trt, phi_init = 0.01)

A <- 5
B <- 15
design <- "TR"
res <- effect_size_RML(design = design, dat = dat, 
                       FE_base = FE_base, RE_base = RE_base,
                       FE_trt = FE_trt, RE_trt = RE_trt, 
                       A = A, B = B, phi_init = 0.01)
