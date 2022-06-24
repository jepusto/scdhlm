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

lme_fit_MB <- function(design, dat, FE_base, RE_base, RE_base_2, FE_trt, RE_trt, RE_trt_2, 
                       corStruct = "AR(1)", varStruct = "hom", center = 0, phi_init = 0.01) {
  
  require(nlme)
  
  # sort the data
  if (design == "RMBB") {
    dat <- dat[order(dat$case, dat$series, dat$session),]
  } else if (design == "CMB") {
    dat <- dat[order(dat$cluster, dat$case, dat$session),]
  } else {
    dat <- dat[order(dat$case, dat$session),]
  }
  
  dat$session <- dat$session - center
  
  session_FE <- write_formula(FE_base, c("0","1","session"))
  trt_FE <- write_formula(FE_trt, c("NULL", "trt", "session_trt"))
  fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- write_formula(RE_base, c("0","1","session"))
  trt_RE <- write_formula(RE_trt, c("NULL","trt","session_trt"))
  if (design %in% c("MBP", "TR")) {
    random <- as.formula(paste(" ~ ",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  } else {
    session_RE_2 <- write_formula(RE_base_2, c("0","1","session"))
    trt_RE_2 <- write_formula(RE_trt_2, c("NULL","trt","session_trt"))
    if (design == "RMBB") {
      random <- eval(parse(text = paste0("list(case = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                         ", series = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
    } else {
      random <- eval(parse(text = paste0("list(cluster = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                         ", case = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
    }
    
  }
  
  nesting_str <- switch(design, 
                        MBP = "case",
                        RMBB = "case/series",
                        CMB = "cluster/case")
  
  cor_struct <- switch(corStruct,
                       `MA(1)` = eval(parse(text = paste0("corARMA(0, ~ session | ",nesting_str,", p = 0, q = 1)"))),
                       `AR(1)` = eval(parse(text = paste0("corAR1(", phi_init, ", ~ session | ", nesting_str, ")"))),
                       `IID` = NULL)

  if (varStruct == "het") {
    var_struct <- eval(parse(text = "varIdent(form = ~ 1 | phase)"))
  } else {
    var_struct <- NULL
  }
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = cor_struct,
                 weights = var_struct,
                 data = dat, 
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  
  list(fixed = fixed,
       random = random,
       fit = RML_fit,
       converged = if (is.null(E)) W else E)
}

lme_fit_TR <- function(dat, FE_base, RE_base, FE_trt, RE_trt, corStruct = "AR(1)", varStruct = "hom", phi_init = 0.01, ...) {
  require(nlme)
  
  dat <- dat[order(dat$case, dat$session),]
  
  session_FE <- if (is.null(FE_base) | !(0 %in% FE_base)) "0" else "1"
  trt_FE <- if (is.null(FE_trt) | !(0 %in% FE_trt)) NULL else "trt"
  fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
  
  session_RE <- if (is.null(RE_base) | !(0 %in% RE_base)) "0" else "1"
  trt_RE <- if (is.null(RE_trt) | !(0 %in% RE_trt)) NULL else "trt"
  random <- as.formula(paste(" ~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  
  cor_struct <- switch(corStruct,
                       `MA(1)` = eval(parse(text = paste0("corARMA(0, ~ session | case, p = 0, q = 1)"))),
                       `AR(1)` = eval(parse(text = paste0("corAR1(", phi_init, ", ~ session | case)"))),
                       `IID` = NULL)
  
  if (varStruct == "het") {
    var_struct <- eval(parse(text = "varIdent(form = ~ 1 | phase)"))
  } else {
    var_struct <- NULL
  }
  
  W <- TRUE
  E <- NULL
  RML_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = cor_struct,
                 weights = var_struct,
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

effect_size_RML <- function(design, dat, FE_base, RE_base, RE_base_2, FE_trt, RE_trt, RE_trt_2, 
                            corStruct, varStruct, A, B, center = 0, phi_init = 0.01) {
  
  fit_function <- switch(design, 
                         MBP = "lme_fit_MB", 
                         RMBB = "lme_fit_MB", 
                         CMB = "lme_fit_MB", 
                         TR = "lme_fit_TR")
  
  m_fit <- do.call(fit_function, 
                   args = list(design = design, dat = dat, 
                               FE_base = FE_base, RE_base = RE_base, RE_base_2 = RE_base_2,
                               FE_trt = FE_trt, RE_trt = RE_trt, RE_trt_2 = RE_trt_2,
                               corStruct = corStruct, varStruct = varStruct,
                               center = center, phi_init = phi_init))
  fixed <- m_fit$fixed
  random <- m_fit$random
  mod <- m_fit$fit
  mod$call$fixed <- fixed
  mod$call$random <- random
  p_const <- c(rep(0L, length(FE_base)), (B - A)^as.integer(FE_trt))
  
  r_dim <- length(RE_base) + length(RE_trt)
  r_const_dim <- r_dim * (r_dim + 1) / 2
  bc_vec <- (B - center)^as.integer(RE_base)
  bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
  r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
  r_const_trt <- rep(0L, r_const_dim - length(r_const_base))
  
  if (design %in% c("MBP", "TR")) {
    r_const <- c(r_const_base,
                 r_const_trt,
                 rep(0L, length(mod$modelStruct$corStruct)),
                 rep(0L, length(mod$modelStruct$varStruct)),
                 1L)
  } else {
    
    r_dim2 <- length(RE_base_2) + length(RE_trt_2)
    r_const_dim2 <- r_dim2 * (r_dim2 + 1) / 2
    bc_vec2 <- (B - center)^as.integer(RE_base_2)
    bc_mat2 <- 2 * tcrossprod(bc_vec2) - diag(bc_vec2^2)
    r_const_base2 <- bc_mat2[upper.tri(bc_mat2, diag = TRUE)]
    r_const_trt2 <- rep(0L, r_const_dim2 - length(r_const_base2))
    
    r_const <- c(r_const_base,
                 r_const_trt,
                 r_const_base2,
                 r_const_trt2,
                 rep(0L, length(mod$modelStruct$corStruct)),
                 rep(0L, length(mod$modelStruct$varStruct)),
                 1L)
  }
  
  g_mlm(mod = mod, p_const = p_const, r_const = r_const, infotype = "expected")
  
}
