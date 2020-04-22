


session_by_treatment <- function(x, trt_phase) {
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}

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

#' @title Graph Single Case Design Data
#' 
#' @description Graphs single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param outcome vector of outcome data or name of variable within \code{data}. May not contain any missing values.
#' @param phase vector of treatment indicators or name of variable within \code{data}. Must be the same length as \code{outcome}.
#' @param case factor vector indicating unique cases or name of variable within \code{data}. Must be the same length as \code{outcome}.
#' @param session vector of measurement occasion times or name of variable within \code{data}. Must be the same length as \code{outcome}.
#' @param data (Optional) dataset to use for analysis. Must be data.frame. 
#' @param design Specify wheter it is a treatment reversal, "TR", or multiple baseline, "MB", design
#' @param treatment_name (Optional) value of the name of the treatment phase
#' @param model_fit (Optional) lme fitted model that adds predicted values to graph
#' 
#' @note If treatment_name is left null it will choose the second level of the phase variable to be the treatment phase.
#' 
#' @export 
#' 
#' @return A ggplot graph 
#' 
#' 
#' @examples
#' data(Anglesea)
#' graph_SCD(case=case, phase=condition, 
#'           session=session, outcome=outcome, 
#'           design="TR", treatment_name = "treatment", 
#'           data=Anglesea)
#'           
#' data(BartonArwood)
#' graph_SCD(case=case, phase=condition, 
#'           session=session, outcome=outcome, 
#'           design="MB", treatment_name = "B",  
#'           data=BartonArwood)
#' 

graph_SCD <- function(case, phase, session, outcome, design, treatment_name = NULL, model_fit=NULL, data=NULL) {
  
  phase_pair <-  phase_time  <- NULL
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
  
  p <- p + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::facet_grid(case ~ .) + 
    ggplot2::theme_bw() + 
    ggplot2::labs(color = "", shape = "") + 
    ggplot2::geom_vline(data = phase_line_dat, ggplot2::aes(xintercept = phase_time), linetype = "dashed")
  
  # With model fit
  if (!is.null(model_fit)) {
    
    dat$fitted <- predict(model_fit)
    
    p <- p + ggplot2::geom_line(data = dat, ggplot2::aes(session, fitted), size = 0.8)
    
  } 
  
  p
  
}

