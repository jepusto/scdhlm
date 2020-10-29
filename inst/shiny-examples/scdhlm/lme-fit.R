# rm(list=ls())
# source("inst/shiny-examples/scdhlm/mappings.R")
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

lme_fit_MB <- function(dat, FE_base, RE_base, FE_trt, RE_trt, center = 0, phi_init = 0.01) {
  require(nlme)
  # sort the data
  dat <- dat[order(dat$case, dat$session),]
  dat$session <- dat$session - center
  
  session_FE <- write_formula(FE_base, c("0","1","session"))
  trt_FE <- write_formula(FE_trt, c("NULL", "trt", "session_trt"))
  fixed <- as.formula(paste("outcome ~",paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- write_formula(RE_base, c("0","1","session"))
  trt_RE <- write_formula(RE_trt, c("NULL","trt","session_trt"))
  random <- as.formula(paste(" ~ ",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(phi_init, ~ session | case),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit,
       converged = if (is.null(E)) W else E)
}

lme_fit_TR <- function(dat, FE_base, RE_base, FE_trt, RE_trt, phi_init = 0.01, ...) {
  require(nlme)
  
  dat <- dat[order(dat$case, dat$session),]
  
  session_FE <- if (is.null(FE_base) | !(0 %in% FE_base)) "0" else "1"
  trt_FE <- if (is.null(FE_trt) | !(0 %in% FE_trt)) NULL else "trt"
  fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- if (is.null(RE_base) | !(0 %in% RE_base)) "0" else "1"
  trt_RE <- if (is.null(RE_trt) | !(0 %in% RE_trt)) NULL else "trt"
  random <- as.formula(paste(" ~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = corAR1(phi_init, ~ session | case),
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit,
       converged = if (is.null(E)) W else E)
  
}


#---------------------------------------------------------------
# calculate effect sizes
#---------------------------------------------------------------

effect_size_RML <- function(design, dat, FE_base, RE_base, FE_trt, RE_trt, A, B, center, phi_init = 0.01) {
  fit_function <- list(MB = "lme_fit_MB", TR = "lme_fit_TR")[[design]]
  m_fit <- do.call(fit_function, 
                   args = list(dat = dat, FE_base = FE_base, RE_base = RE_base,
                               FE_trt = FE_trt, RE_trt = RE_trt, center = center, phi_init = phi_init))
  fixed <- m_fit$fixed
  random <- m_fit$random
  mod <- m_fit$fit
  mod$call$fixed <- fixed
  mod$call$random <- random
  p_const <- c(rep(0L, length(FE_base)), (B - A - 1)^as.integer(FE_trt))
  
  r_dim <- length(RE_base) + length(RE_trt)
  r_const_dim <- r_dim * (r_dim + 1) / 2
  bc_vec <- (B - center)^as.integer(RE_base)
  bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
  r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
  r_const_trt <- rep(0L, r_const_dim - length(r_const_base))
  
  r_const <- c(r_const_base, 
               r_const_trt,
               rep(0, length(mod$modelStruct$corStruct)),
               rep(0, length(mod$modelStruct$varStruct)),
               1L)
  
  g_mlm(mod, p_const = p_const, r_const = r_const, infotype = "expected", returnModel = TRUE)
  
}

# input <- list(example = "Laski")
# design <- "MB"
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
# graph_SCD(dat = dat, design = design)
# last_plot() + geom_line(data = dat, aes(session, fitted), size = 0.8)
# 
# A <- default_times(dat)$A
# B <- default_times(dat)$B
# res <- effect_size_RML(design, dat, FE_base, RE_base, FE_trt, RE_trt, A, B)
# summarize_ES(res, filter_vars = NULL, filter_vals = NULL, design = design, method = "RML",
#              FE_base = FE_base, RE_base = RE_base, FE_trt = FE_trt, RE_trt = RE_trt, A = A, B = B)

# 

# cases <- levels(dat$case)
# range <- default_times(dat)$range
# A <- default_times(dat)$A
# B <- default_times(dat)$B
# sessions <- seq(range[1], range[2])
# dat_RCT <- data.frame(case = rep(cases, each = length(sessions)),
#                       session = sessions,
#                       trt = as.integer(sessions > A),
#                       session_trt = ifelse(sessions > A, sessions - A - 1, 0))
# dat_RCT <- lme_fit_MB(dat, FE_base = FE_base, RE_base = RE_base,
#                       FE_trt = FE_trt, RE_trt = RE_trt, center = B, newdata = dat_RCT)
# dat_RCT <- dat_RCT$preds
# dat_RCT$phase <- levels(dat$phase)[dat_RCT$trt + 1]
# graph_SCD(dat_RCT, design = "MB")

# 
# input <- list(example = "Lambert")
# design <- "TR"
# data(list = input$example)
# dat <- get(input$example)
# dat <- dat[,exampleMapping[[input$example]]$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# 
# lme_fit_TR(dat = dat, FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = 0)
# lme_fit <- lme_fit_TR(dat = dat, FE_base = 0, RE_base = 0, FE_trt = 0, RE_trt = NULL)
# lme_fit_TR(dat = dat, FE_base = 0, RE_base = NULL, FE_trt = 0, RE_trt = NULL)
# dat$fitted <- predict(lme_fit$fit)
# 
# FE_base <- 0
# RE_base <- 0
# FE_trt <- 0
# RE_trt <- NULL
# A <- default_times(dat)$A
# B <- default_times(dat)$B
# res <- effect_size_RML(design, dat, FE_base, RE_base, FE_trt, RE_trt, A, B)
# summarize_ES(res, filter_vars = NULL, filter_vals = NULL, design = design, method = "RML",
#              FE_base = FE_base, RE_base = RE_base, FE_trt = FE_trt, RE_trt = RE_trt, A = A, B = B)
