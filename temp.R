library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(nlme)


rm(list=ls())
wd <- setwd("C:/Users/jep2963/Box Sync/Tier 3 Second Paper")
ranges_to_read <- list(School1 = list(rows = c(2,4:9), cols = 1:23),
                       School2 = list(rows = c(10,12:16), cols = 1:22),
                       School3 = list(rows = c(17,19:26), cols = 1:23),
                       School4 = list(rows = c(27,29:39), cols = 1:25),
                       School5 = list(rows = c(40,42:44), cols = 1:26))

read_ranges <- function(rng) {
  
  read.xlsx("2010_2011Tier III Single Subject graph of ACs updated 2-28-2014.xlsx",
            sheet = "AC Scores by Child",
            rows = rng$rows[1] + c(0,1), 
            cols = rng$cols, colNames = TRUE) %>%
    gather("S","Measure",1:ncol(.)) ->
    measure_dat 
  
  read.xlsx("2010_2011Tier III Single Subject graph of ACs updated 2-28-2014.xlsx",
            sheet = "AC Scores by Child",
            rows = rng$rows, cols = rng$cols, colNames = TRUE) %>%
    gather("S","AC", rng$cols[5:length(rng$cols)]) %>%
    left_join(measure_dat, by = "S") %>%
    rename(school = X1, group = X2, key_math_std_score = X3, student = X4)
}

lapply(ranges_to_read, read_ranges) %>%
  bind_rows() ->
  AC_dat

setwd(wd)









devtools::load_all()

AC_dat %>%
  group_by(student) %>%
  mutate(group = paste0(school, "-",group),
         phase = substr(S, 1, 1),
         session = as.integer(substr(S, 2, 3)),
         session = ifelse(phase!="M", session, session + 59),
         session_trt = pmax(0, session - max(session[phase=="B"]))) %>%
  arrange(school, group, student, session) %>%
  filter(!is.na(AC)) ->
  AC_clean

AC_BT <- filter(AC_clean, phase != "M")

m_fit <- lme(AC ~ session + session_trt,
    random = list(~ 1 | group, ~ session + session_trt | student),
    correlation = corAR1(0, ~ session),
    data = AC_BT,
    control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE))
p_const <- c(0,0,10)
r_const <- c(1,0,1,1,0,0,0,0,1)
X_design = model.matrix(m_fit, data = m_fit$data)
Z_design = model.matrix(m_fit$modelStruct$reStruct, data = m_fit$data)
block = nlme::getGroups(m_fit)
times = attr(m_fit$modelStruct$corStruct, "covariate")
returnModel=TRUE



# basic model estimates
p_beta <- sum(nlme::fixed.effects(m_fit) * p_const)               # p'Beta
theta <- extract_varcomp(m_fit)                                   # full theta vector
r_theta <- sum(unlist(theta) * r_const)                           # r'theta
delta_AB <- p_beta / sqrt(r_theta)                                # delta_AB              
kappa_sq <- (t(p_const) %*% vcov(m_fit) %*% p_const) / r_theta    # kappa^2
cnvg_warn <- !is.null(attr(m_fit,"warning"))                      # indicator that RML estimation has not converged
