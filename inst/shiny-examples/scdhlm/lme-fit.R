# rm(list=ls())
# source("inst/shiny-examples/scdhlm/example-mappings.R")


lme_fit_MB <- function(dat, FE_base, RE_base, FE_trt, RE_trt) {
  require(nlme)
  # sort the data
  dat <- dat[order(dat$case, dat$session),]
  
  session_FE <- if (is.null(FE_base)) "0" else paste(ifelse(0 %in% FE_base, "1", "0"),
                                                      paste(paste0("I(session^",FE_base[FE_base != 0],")"), collapse = " + "),
                                                      sep = " + ")
  trt_FE <- if (is.null(FE_trt)) NULL else paste(ifelse(0 %in% FE_trt, "trt",NULL), 
                                                 paste(paste0("I(session_trt^", FE_trt[FE_trt != 0], ")"), collapse = " + "), 
                                                 sep = " + ")
  fixed <- as.formula(paste("outcome ~",paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- if (is.null(RE_base)) NULL else paste(ifelse(0 %in% RE_base, "1", "0"),
                                                      paste(paste0("I(session^",RE_base[RE_base != 0],")"), collapse = " + "),
                                                      sep = " + ")
  trt_RE <- if (is.null(RE_trt)) NULL else paste(ifelse(0 %in% RE_trt, "trt",NULL), 
                                                 paste(paste0("I(session_trt^", RE_trt[RE_trt != 0], ")"), collapse = " + "),
                                                 sep = " + ")
  random <- as.formula(paste("~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(0, ~ session),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit)
}

lme_fit_TR <- function(dat, FE_base, RE_base, FE_trt, RE_trt) {
  require(nlme)
  
  dat <- dat[order(dat$case, dat$session),]
  
  session_FE <- if (is.null(FE_base)) "0" else ifelse(0 %in% FE_base, "1", "0")
  trt_FE <- if (is.null(FE_trt)) NULL else ifelse(0 %in% FE_trt, "trt", NULL)
  fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- if (is.null(RE_base)) "0" else ifelse(0 %in% RE_base, "1", "0")
  trt_RE <- if (is.null(RE_trt)) NULL else ifelse(0 %in% RE_trt, "trt",NULL)
  random <- as.formula(paste("~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(0, ~ session),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit)
  
}


# input <- list(example = "Saddler")
# data(list = input$example)
# dat <- get(input$example)
# dat <- dat[dat$measure==1,]
# dat <- dat[,exampleMapping[[input$example]]$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# dat$session_trt <- unlist(by(dat, dat$case, function(x) pmax(0, x$session - min(x$session[x$phase==trt_phase]))))
# 
# FE_base <- c(0,2,3)
# RE_base <- c(0,2)
# FE_trt <- c(0,1)
# RE_trt <- NULL
# 
# lme_fit_MB(dat = dat, FE_base, RE_base, FE_trt, RE_trt)
# 
# 
# 
# input <- list(example = "Lambert")
# data(list = input$example)
# dat <- get(input$example)
# dat <- dat[,exampleMapping[[input$example]]$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# 
# lme_fit_TR(dat = dat, FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = 0)
# lme_fit_TR(dat = dat, FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = NULL)
# lme_fit_TR(dat = dat, FE_base = 0, RE_base = NULL, FE_trt = 0, RE_trt = NULL)
