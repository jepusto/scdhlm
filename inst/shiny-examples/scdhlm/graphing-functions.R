# this is the function I created. I will delete this file after the graphing function gets incorporated with the scdhlm
#package. 
graph_SCD <- function(case, phase, session, outcome, design, treatment_name = NULL, model_fit=NULL, data=NULL) {
  
  phase_pair <-  phase_time  <- NULL
  
  # With model fit
  if (!is.null(model_fit)) {
    
    if (!is.null(data)) {
      outcome_call <- substitute(outcome)
      phase_call <- substitute(phase)
      case_call <- substitute(case)
      session_call <- substitute(session)
      
      env <- list2env(data, parent = parent.frame())
      
      outcome <- eval(outcome_call, env)
      phase <- eval(phase_call, env)
      case <- eval(case_call, env)
      session <- eval(session_call, env)
    }
    
    
    if (is.null(treatment_name)) {
      treatment_name <-  levels(as.factor(phase))[2]
    }
    
    
    
    
    dat <- data.frame(case = factor(case),
                      phase = factor(phase),
                      session_fac = factor(session),
                      outcome, session)
    
    dat$fitted <- predict(model_fit)
    
    trt_phase <- treatment_name
    dat$trt <- as.numeric(dat$phase==trt_phase)
    phase_line_dat <- phase_lines_by_case(dat)
    
    if (design == "MB") {
      dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
    } else {
      dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
    }
    
    
    if (design=="MB") {
      p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = as.factor(phase), shape = as.factor(phase)))  
    } else {
      p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = phase, shape = phase, group = interaction(phase, phase_pair)))
    }
    
    p + 
      ggplot2::geom_point() + 
      ggplot2::geom_line() + 
      ggplot2::facet_grid(case ~ .) + 
      ggplot2::theme_bw() + 
      ggplot2::labs(color = "", shape = "") + 
      ggplot2::geom_vline(data = phase_line_dat, ggplot2::aes(xintercept = phase_time), linetype = "dashed") +
      ggplot2::geom_line(data = dat, ggplot2::aes(session, fitted), size = 0.8)
  }  else { #without model fit
    
    if (!is.null(data)) {
      outcome_call <- substitute(outcome)
      phase_call <- substitute(phase)
      case_call <- substitute(case)
      session_call <- substitute(session)
      
      env <- list2env(data, parent = parent.frame())
      
      outcome <- eval(outcome_call, env)
      phase <- eval(phase_call, env)
      case <- eval(case_call, env)
      session <- eval(session_call, env)
    }
    
    
    if (is.null(treatment_name)) {
      treatment_name <-  levels(as.factor(phase))[2]
    }
    
    
    
    
    dat <- data.frame(case = factor(case),
                      phase = factor(phase),
                      session_fac = factor(session),
                      outcome, session)
    
    trt_phase <- treatment_name
    dat$trt <- as.numeric(dat$phase==trt_phase)
    phase_line_dat <- phase_lines_by_case(dat)
    
    if (design == "MB") {
      dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
    } else {
      dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
    }
    
    if (design=="MB") {
      p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = as.factor(phase), shape = as.factor(phase)))  
    } else {
      p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = phase, shape = phase, group = interaction(phase, phase_pair)))
    }
    
    p + 
      ggplot2::geom_point() + 
      ggplot2::geom_line() + 
      ggplot2::facet_grid(case ~ .) + 
      ggplot2::theme_bw() + 
      ggplot2::labs(color = "", shape = "") + 
      ggplot2::geom_vline(data = phase_line_dat, ggplot2::aes(xintercept = phase_time), linetype = "dashed")
    
  }
}

# input <- list(example = "Saddler")
# data(list = input$example)
# dat <- get(input$example)
# example_parms <- exampleMapping[[input$example]]
# filter_vars <- example_parms$filters
# input$filter_measure = 1
# if (!is.null(filter_vars)) {
#   subset_vals <- sapply(filter_vars, function(x) dat[[x]] %in% input[[paste0("filter_",x)]])
#   dat <- dat[apply(subset_vals, 1, all),]
# }
# dat <- dat[,example_parms$vars]
# names(dat) <- c("case","session","phase","outcome")
# design <- example_parms$design
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# if (design == "MB") {
#   dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
# } else {
#   dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
# }
# 
# default_times(dat)
# 
# graph_SCD(dat, design)
