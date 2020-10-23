
#' @title Clean Single Case Design Data
#' 
#' @description Clean single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param data dataset to be cleaned. Must be a \code{data.frame}. 
#' @param case name of a character or factor vector within \code{data} indicating unique cases.
#' @param phase name of a character or factor vector within \code{data} indicating unique treatment phases.
#' @param session name of numeric vector within \code{data} of measurement times.
#' @param outcome name of numeric vector of outcome data within \code{data}.
#' @param design Character string to specify whether data comes from a treatment reversal, "TR", or multiple baseline, "MB", design.
#' @param center Numeric value for the centering value for session. Default is 0.
#' @param treatment_name (Optional) Character string corresponding to the name of the treatment phase.
#' @param round_session Logical indicating whether to round \code{session} to the nearest integer. Defaults to \code{TRUE}.
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

preprocess_SCD <- function(data, 
                           case, phase, session, outcome, 
                           design, 
                           center = 0, 
                           treatment_name = NULL, 
                           round_session = TRUE) {
  
  if (missing(data) || !inherits(data, "data.frame")) stop("Please specify a data.frame in the data argument.")
  if (missing(case)) stop("Please specify a case variable.")
  if (missing(phase)) stop("Please specify a phase variable.")
  if (missing(session)) stop("Please specify a session variable.")
  if (missing(outcome)) stop("Please specify an outcome variable.")
  if (missing(design)) stop("Please specify a study design of 'MB' (multiple baseline) or 'TR' (treatment reversal).")
  
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
  
  dat$trt <- as.numeric(dat$phase == treatment_name) # create trt variable
  
  if (design == "MB") { 
    dat$session_trt <- unsplit(by(dat, dat$case, session_by_treatment, trt_phase = treatment_name), dat$case)
    dat$session <- dat$session - center
    names(dat)[6] <- paste0(as.character(session_call), "_trt")
  } else {
    dat$phase_pair <- unsplit(by(dat, dat$case, phase_pairs), dat$case)
    names(dat)[6] <- paste0(as.character(phase_call), "_pair")
  }
  
  dat <- droplevels(dat)
  names(dat)[1] <- as.character(case_call)
  names(dat)[2] <- as.character(phase_call)
  names(dat)[3] <- as.character(session_call)
  names(dat)[4] <- as.character(outcome_call)
  
  return(dat)
  
}
