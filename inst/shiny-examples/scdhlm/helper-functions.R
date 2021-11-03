#---------------------------------------------------------------
# parse code chunks with user-specified arguments
#---------------------------------------------------------------

parse_code_chunk <- function(chunk, args) {
  chunk_path <- system.file("shiny-examples/scdhlm/code-chunks", paste0(chunk,".R"), package = "scdhlm")
  raw_code <- readLines(chunk_path)
  code_chunk <- paste(raw_code, collapse = "\n")
  glue::glue_data(.x = args, code_chunk)
}

#---------------------------------------------------------------
# paste an object in server for code chunks
#---------------------------------------------------------------
paste_object <- function(object) {
  paste("c(", paste(object, collapse = ","), ")", sep = "")
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

validate_specification <- function(FE_base, RE_base, FE_trt, RE_trt, case) {
  
  errors <- vector(mode = "character")
  if (!("0" %in% FE_base)) {
    errors <- c(errors, "<font color='red'>Model must include a fixed effect for baseline level.</font>")
  }
  if (!("0" %in% RE_base)) {
    errors <- c(errors, "<font color='red'>Model must include a random effect for baseline level.</font>")
  }
  if (length(FE_trt)==0) {
    errors <- c(errors, "<font color='red'>Model must include at least one fixed effect for the treatment phase.</font>")
  }
  
  if(nlevels(case) < 3) {
    errors <- c(errors, "<font color='red'>Model must include at least three cases. Currently, you have less than three cases.</font>")
  }
  
  if (length(errors)==0) {
    return(NULL)
  } else if (length(errors) == 1) {
    error_string <- paste("<b><font color='red'>Error:</font></b>", errors, "<br/>")
  } else {
    error_string <- paste("<b><font color='red'>Errors:</font></b><br/>", paste(errors, collapse = "<br/>"), "<br/>")
  } 
  
  return(HTML(error_string))
}

#---------------------------------------------------------------
# effect size report table
#---------------------------------------------------------------

summarize_ES <- function(res, filter_vals, 
                         design, method, 
                         FE_base, RE_base, FE_trt, RE_trt,
                         A, B, coverage = 95L) {
  
  if (method=="RML") {
    ES_summary <- data.frame(
      ES = as.numeric(res$g_AB),
      SE = as.numeric(res$SE_g_AB)
    )
    res$rho <- with(res, theta$Tau[[1]][1] / (theta$Tau[[1]][1] + theta$sigma_sq))
    res$phi <- res$theta$cor_params
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
  
  if (method=="RML" & design=="MB" & !is.null(A) & !is.null(B)) {
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
    ES_summary <- cbind(ES_summary, filter_vals)
  } else {
    ES_summary <- ES_summary
  }
  
  ES_summary  
}
