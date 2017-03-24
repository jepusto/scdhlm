library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)
rm(list=ls())
source("inst/shiny-examples/scdhlm/mappings.R")
source("inst/shiny-examples/scdhlm/graphing-functions.R")
source("inst/shiny-examples/scdhlm/helper-functions.R")
source("inst/shiny-examples/scdhlm/lme-fit.R")

datFile <- read.table("auxilliary/Alber-Morgan-2007.csv", 
                      header=TRUE, sep=",", quote="",
                      stringsAsFactors = FALSE)

phases <- levels(as.factor(datFile$phase))

case_vec <- datFile$case
caseID <- factor(case_vec, levels = unique(case_vec))
session <- as.numeric(datFile$session)
phaseID <- as.factor(datFile$phase)
outcome <- as.numeric(datFile$outcome)
dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
trt_phase <- "treatment"
dat$trt <- as.numeric(dat$phase==trt_phase)
dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))


FE_base <- c(0,1)
RE_base <- c(0,1)
FE_trt <- c(0)
RE_trt <- c(0)

model_fit <- lme_fit_MB(dat = dat,
                        FE_base = FE_base,
                        RE_base = RE_base,
                        FE_trt = FE_trt,
                        RE_trt = RE_trt,
                        center = 5)

A <- 5
B <- 15
res <- effect_size_RML(design = "MB", dat = dat, 
                       FE_base = FE_base, RE_base = RE_base,
                       FE_trt = FE_trt, RE_trt = RE_trt, 
                       A = A, B = B)
