# rm(list=ls())
# source("inst/shiny-examples/scdhlm/example-mappings.R")
# source("inst/shiny-examples/scdhlm/graphing-functions.R")
# source("inst/shiny-examples/scdhlm/helper-functions.R")

write_formula <- function(powers, var_names) {
  var_name_1 <- if (var_names[1] == "NULL") NULL else var_names[1]
  if (is.null(powers)) {
    var_name_1
  } else {
    paste(c(if (0 %in% powers) var_names[2] else var_name_1, 
            paste0("I(",var_names[3],"^",powers,")")[powers != 0]), 
          collapse = " + ")
  }
}

lme_fit_MB <- function(dat, FE_base, RE_base, FE_trt, RE_trt, center = 0) {
  require(nlme)
  # sort the data
  dat <- dat[order(dat$case, dat$session),]
  dat$session <- dat$session - center
  
  session_FE <- write_formula(FE_base, c("0","1","session"))
  trt_FE <- write_formula(FE_trt, c("NULL", "trt", "session_trt"))
  fixed <- as.formula(paste("outcome ~",paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- write_formula(RE_base, c("0","1","session"))
  trt_RE <- write_formula(RE_trt, c("NULL","trt","session_trt"))
  random <- as.formula(paste("~ ",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(0, ~ session),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit,
       converged = if (is.null(E)) W else E)
}

lme_fit_TR <- function(dat, FE_base, RE_base, FE_trt, RE_trt, ...) {
  require(nlme)
  
  dat <- dat[order(dat$case, dat$session),]
  
  session_FE <- if (is.null(FE_base) | !(0 %in% FE_base)) "0" else "1"
  trt_FE <- if (is.null(FE_trt) | !(0 %in% FE_trt)) NULL else "trt"
  fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- if (is.null(RE_base) | !(0 %in% RE_base)) "0" else "1"
  trt_RE <- if (is.null(RE_trt) | !(0 %in% RE_trt)) NULL else "trt"
  random <- as.formula(paste("~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(0, ~ session),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit,
       converged = if (is.null(E)) W else E)
  
}


# input <- list(example = "Schutte")
# data(list = input$example)
# dat <- get(input$example)
# dat <- dat[,exampleMapping[[input$example]]$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
# 
# FE_base <- c(0)
# RE_base <- c(0)
# FE_trt <- c(0)
# RE_trt <- NULL
# center <- default_times(dat)$A
# 
# lme_fit <- lme_fit_MB(dat = dat, FE_base, RE_base, FE_trt, RE_trt, center = center)
# dat$fitted <- predict(lme_fit$fit)
# graph_SCD(dat = dat, design = "MB")
# last_plot() + geom_line(data = dat, aes(session, fitted), size = 0.8)
# 
# cases <- levels(dat$case)
# range <- default_times(dat)$range
# A <- default_times(dat)$A
# B <- default_times(dat)$B
# sessions <- seq(range[1], range[2])
# dat_RCT <- data.frame(case = rep(cases, each = length(sessions)),
#                       session = sessions,
#                       trt = c(rep(0, A - range[1] + 1), rep(1, range[2] - range[1] - A + 1)),
#                       session_trt = c(rep(0, A - range[1] + 1), 0:(range[2] - range[1] - A)))
# dat_RCT$outcome <- predict(lme_fit$fit, newdata = dat_RCT)
# dat_RCT$phase <- levels(dat$phase)[dat_RCT$trt + 1]
# graph_SCD(dat_RCT, design = "MB")  
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
# lme_fit <- lme_fit_TR(dat = dat, FE_base = 0, RE_base = NULL, FE_trt = 0, RE_trt = NULL)
