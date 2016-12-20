
#---------------------------------------------------------------
# calculate session-by-treatment interaction
#---------------------------------------------------------------

session_by_treatment <- function(x, trt_phase) {
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}


#---------------------------------------------------------------
# calculate phase-pairs based on phases and session numbering
#---------------------------------------------------------------

phase_pairs <- function(x) {
  conditions <- levels(as.factor(x$phase))
  n <- length(x$phase)
  phase <- x$phase[order(x$session)]
  y <- rep(1,n)
  for (i in 2:n) {
    (i <- i + 1)
    (which_lev <- match(phase[i-1], conditions))
    (which_conditions <- conditions[c(which_lev, which_lev + 1)])
    !(phase[i] %in% which_conditions)
    (y[i] <- y[i - 1] + !(phase[i] %in% which_conditions))
  }
  y[order(order(x$session))]
}

#---------------------------------------------------------------
# calculate phase border times
#---------------------------------------------------------------

phase_lines <- function(x) {
  phase <- x$phase[order(x$session)]
  n <- length(phase)
  switches <- which(phase[2:n] != phase[1:(n-1)])
  (x$session[switches] + x$session[switches + 1]) / 2
}

phase_lines_by_case <- function(x) {
  phase_line <- by(x, x$case, phase_lines)
  data.frame(case = rep(names(phase_line), lengths(phase_line)), phase_time = as.vector(unlist(phase_line)))
}


#---------------------------------------------------------------
# calculate timing defaults
#---------------------------------------------------------------

default_times <- function(x) {
  range <- range(x$session)
  case_base_last <- with(x, tapply(session[trt==0], case[trt==0], max))
  case_trt_range <- with(x, tapply(session[trt==1], case[trt==1], function(x) diff(range(x)) + 1))
  A <- min(case_base_last)
  B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  list(range = range, A = A, B = B)
}

#---------------------------------------------------------------
# model validation
#---------------------------------------------------------------

validate_specification <- function(FE_base, RE_base, FE_trt, RE_trt) {
  
  errors <- vector(mode = "character")
  if (!("0" %in% FE_base)) {
    errors <- c(errors, "Model must include a fixed effect for baseline level.")
  }
  if (!("0" %in% RE_base)) {
    errors <- c(errors, "Model must include a random effect for baseline level.")
  }
  if (length(FE_trt)==0) {
    errors <- c(errors, "Model must include at least one fixed effect for the treatment phase.")
  }
  
  if (length(errors)==0) {
    return(NULL)
  } else if (length(errors) == 1) {
    error_string <- paste("<b>Error:</b>", errors, "<br/>")
  } else {
    error_string <- paste("<b>Errors:</b> <br/>", paste(errors, collapse = "<br/>"), "<br/>")
  } 
  
  return(HTML(error_string))
}

#---------------------------------------------------------------
# effect size report table
#---------------------------------------------------------------

summarize_ES <- function(res, filter_vars, filter_vals, 
                         design, method, 
                         FE_base, RE_base, FE_trt, RE_trt,
                         A, B, coverage = 95L) {

  if (method=="RML") {
    ES_summary <- data.frame(
      ES = res$g_AB,
      SE = sqrt(res$V_g_AB)
    )
    res$rho <- with(res, Tau[1] / (Tau[1] + sigma_sq))
  } else {
    ES_summary <- data.frame(
      ES = res$delta_hat,
      SE = sqrt(res$V_delta_hat)
    )
  }
  
  CI <- CI_g(res, cover = coverage / 100L)
  
  ES_summary$CI_L <- CI[1]
  ES_summary$CI_U <- CI[2]
  ES_summary$df <- res$nu
  ES_summary$phi <- res$phi
  ES_summary$rho <- res$rho
  ES_summary$design <- names(design_names[which(design_names==design)])
  ES_summary$method <- names(estimation_names[which(estimation_names==method)])
  ES_summary$baseline <- paste0("F:", paste(FE_base, collapse = ""), 
                                " R:", paste(RE_base, collapse = ""))
  ES_summary$trt <- paste0("F:", paste(FE_trt, collapse = ""), 
                           " R:", paste(RE_trt, collapse = ""))
  
  if (method=="RML" & design=="MB") {
    ES_summary$A <- A
    ES_summary$B <- B
  } else {
    ES_summary$A <- NA
    ES_summary$B <- NA
  }
  
  CI_names <- paste0(coverage, "% CI ", c("(lower)", "(upper)"))
  
  row.names(ES_summary) <- NULL
  names(ES_summary) <- c("BC-SMD estimate","Std. Error", CI_names,
                         "Degrees of freedom","Auto-correlation","Intra-class correlation",
                         "Study design","Estimation method",
                         "Baseline specification", "Treatment specification",
                         "Initial treatment time","Follow-up time")
  
  if (!is.null(filter_vals)) {
    filter_vals <- lapply(filter_vals, paste, sep = ", ")
    names(filter_vals) <- substr(filter_vars,8,nchar(filter_vars))
    filter_vals <- as.data.frame(filter_vals)
    ES_summary <- cbind(ES_summary, filter_vals)
  } 
  
  ES_summary  
}

