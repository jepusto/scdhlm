
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
  case_trt_range <- with(x, tapply(session[trt==1], case[trt==1], function(x) diff(range(x))))
  A <- min(case_base_last)
  B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  list(range = range, A = A, B = B)
}

#---------------------------------------------------------------
# effect size report table
#---------------------------------------------------------------

summarize_ES <- function(res, filter_vars, filter_vals, design, method, A, B) {

  if (method=="RML") {
    ES_summary <- data.frame(
      ES = res$g_AB,
      SE = sqrt(res$V_g_AB),
      df = res$nu,
      phi = res$phi,
      rho = with(res, Tau[1] / (Tau[1] + sigma_sq))
    )
  } else {
    ES_summary <- data.frame(
      ES = res$delta_hat,
      SE = sqrt(res$V_delta_hat),
      df = res$nu,
      phi = res$phi,
      rho = res$rho
    )
  }
  
  ES_summary$method <- names(estimation_names[which(estimation_names==method)])
  ES_summary$design <- names(design_names[which(design_names==design)])
  if (method=="RML" & design=="MB") {
    ES_summary$A <- A
    ES_summary$B <- B
  } else {
    ES_summary$A <- NA
    ES_summary$B <- NA
  }
  
  row.names(ES_summary) <- NULL
  names(ES_summary) <- c("BC-SMD estimate","Std. Error",
                         "Degrees-of-freedom","Auto-correlation","Intra-class correlation",
                         "Study design","Estimation method","Initial treatment time","Follow-up time")
  
  if (!is.null(filter_vals)) {
    filter_vals <- lapply(filter_vals, paste, sep = ", ")
    names(filter_vals) <- substr(filter_vars,8,nchar(filter_vars))
    filter_vals <- as.data.frame(filter_vals)
    ES_summary <- cbind(ES_summary, filter_vals)
  } 
  
  ES_summary  
}

