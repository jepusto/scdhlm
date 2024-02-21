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
# model validation
#---------------------------------------------------------------

validate_specification <- function(design, n_outer_levels, 
                                   FE_base, RE_base, FE_trt, RE_trt, 
                                   RE_base2 = NULL, RE_trt2 = NULL) {
  
  errors <- vector(mode = "character")
  if (!("0" %in% FE_base)) {
    errors <- c(errors, "<font color='red'>Model must include a fixed effect for baseline level.</font>")
  }
  if (!("0" %in% RE_base)) {
    errors <- c(errors, "<font color='red'>Model must include a random effect for baseline level.</font>")
  }
  if (design %in% c("RMBB", "CMB") && !("0" %in% RE_base2)) {
    errors <- c(errors, "<font color='red'>Model must include a random effect for baseline level.</font>")
  }
  if (length(FE_trt)==0) {
    errors <- c(errors, "<font color='red'>Model must include at least one fixed effect for the treatment phase.</font>")
  }

  if (n_outer_levels < 3L) {
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
# calculate effect sizes
#---------------------------------------------------------------

calc_effect_size <- function(model, design, method, 
                             FE_base, RE_base, RE_base_2, FE_trt, RE_trt, RE_trt_2,
                             corStruct, varStruct,
                             A, B, center = 0) {
  
  pr_consts <- calc_consts(method = method, design = design, 
                           FE_base = FE_base, RE_base = RE_base, RE_base_2 = RE_base_2,
                           FE_trt = FE_trt, RE_trt = RE_trt, RE_trt_2 = RE_trt_2,
                           corStruct = corStruct, varStruct = varStruct,
                           A = A, B = B, center = center)
  
  if (method == "Bayes") {
    
    es_res <- scdhlm:::g_mlm_Bayes(model$model,
                                   p_const = pr_consts$p_const,
                                   r_const = pr_consts$r_const,
                                   rconst_base_var_index = pr_consts$rconst_base_var_index)
    es_res$phi <- es_res$autocor_param
    
  } else if (method == "RML") {
    
    es_res <- g_mlm(model$model, 
                    p_const = pr_consts$p_const, 
                    r_const = pr_consts$r_const, 
                    infotype = "expected")
    es_res$CI_L <- model$CI_L
    es_res$CI_U <- model$CI_U
    es_res$phi <- model$phi
    es_res$rho <- model$rho
    es_res$var_param <- model$var_param
    
  }
  
  return(es_res)
  
}


#---------------------------------------------------------------
# effect size report table
#---------------------------------------------------------------

summarize_ES <- function(res, filter_vals, 
                         design, method, 
                         FE_base, RE_base, RE_base_2, 
                         FE_trt, RE_trt, RE_trt_2, 
                         corStruct, varStruct,
                         A, B, coverage = 95L) {
  
  if (method %in% c("RML","Bayes")) {
    
    ES_summary <- data.frame(
      ES = as.numeric(res$g_AB),
      SE = as.numeric(res$SE_g_AB),
      CI_L = res$CI_L,
      CI_U = res$CI_U,
      df = res$nu,
      phi = if (corStruct == "IID") NA_real_ else res$phi,
      var_param = if (varStruct == "hom") NA_real_ else res$var_param
    )
    
  } else {
    
    CI <- CI_g(res, cover = coverage / 100L)
    
    ES_summary <- data.frame(
      ES = res$delta_hat,
      SE = sqrt(res$V_delta_hat),
      CI_L = CI[1],
      CI_U = CI[2],
      df = res$nu,
      phi = res$phi,
      var_param = NA_real_
    )
    
  }

  ES_summary$rho <- res$rho
  ES_summary$design <- names(design_names[which(design_names==design)])
  ES_summary$method <- names(estimation_names[which(estimation_names==method)])
  ES_summary$baseline <- paste0("F:", paste(FE_base, collapse = ""), 
                                " R1:", paste(RE_base, collapse = ""),
                                " R2:", paste(RE_base_2, collapse = ""))
  ES_summary$trt <- paste0("F:", paste(FE_trt, collapse = ""), 
                           " R1:", paste(RE_trt, collapse = ""),
                           " R2:", paste(RE_trt_2, collapse = ""))
  
  if (method %in% c("RML", "Bayes") & design %in% c("MBP", "RMBB", "CMB") & !is.null(A) & !is.null(B)) {
    ES_summary$A <- A
    ES_summary$B <- B
  } else {
    ES_summary$A <- NA
    ES_summary$B <- NA
  }
  
  CI_names <- paste0(coverage, "% CI ", c("(lower)", "(upper)"))
  
  row.names(ES_summary) <- NULL
  names(ES_summary) <- c("BC-SMD estimate","Std. Error", CI_names,
                         "Degrees of freedom",
                         "Auto-correlation", "Variance parameter",
                         "Intra-class correlation",
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
