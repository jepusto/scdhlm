
#' @title Clean Single Case Design Data
#' 
#' @description Clean single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param data dataset to be cleaned. Must be data.frame. 
#' @param case factor vector indicating unique cases or name of variable within \code{data}.
#' @param phase vector of treatment indicators or name of variable within \code{data}.
#' @param session vector of measurement occasion times or name of variable within \code{data}.
#' @param outcome vector of outcome data or name of variable within \code{data}.
#' @param design Specify whether it is a treatment reversal, "TR", or multiple baseline, "MB", design
#' @param center Specify the centering value for session.
#' @param treatment_name (Optional) value of the name of the treatment phase
#' @param round_session Default is rounding session to nearest integer.
#' 
#' @note If treatment_name is left null it will choose the second level of the phase variable to be the treatment phase.
#' 
#' @export 
#' 
#' @return A cleaned SCD dataset that can be used for model fitting and effect size calculation.
#' 
#' @examples
#' data(Laski)
#' preprocess_SCD(data = Laski, case = case, 
#'                phase = treatment, session = time, 
#'                outcome = outcome, design = "MB", center = 4)
#' 
#'           

preprocess_SCD <- function(data, case, phase, session, outcome, design, center, treatment_name = NULL, round_session = TRUE) {
  
  # get variables
  case_call <- substitute(case)
  phase_call <- substitute(phase)
  session_call <- substitute(session)
  outcome_call <- substitute(outcome)
  
  env <- list2env(data, parent = parent.frame())
  case <- eval(case_call, env)
  phase <- eval(phase_call, env)
  session <- eval(session_call, env)
  outcome <- eval(outcome_call, env)
  
  # get treatment name
  if (is.null(treatment_name)) {
    treatment_name <-  levels(as.factor(phase))[2]
  }
  
  # round session
  if (round_session == TRUE) {
    session <- as.integer(round(session))
  } else {
    session <- as.integer(session)
  }
  
  # get the data
  dat <- data.frame(case = factor(case, levels = unique(case)),
                    phase = factor(phase, levels = unique(phase)),
                    session, outcome)
  
  # remove rows with missing outcome variables
  dat <- dat[!is.na(dat$outcome), ]
  
  trt_phase <- treatment_name # create trt_phase variable
  dat$trt <- as.numeric(dat$phase==trt_phase) # create trt variable
  
  if (design == "MB") { 
    dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
    dat$session <- dat$session - center
  } else {
    dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
  }
  
  dat <- droplevels(dat)
  
  return(dat)
  
}
