
#' @title Clean Single Case Design Data
#' 
#' @description Clean single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param case vector of case indicators or name of a character or factor vector within \code{data} indicating unique cases.
#' @param phase vector of treatment indicators or name of a character or factor vector within \code{data} indicating unique treatment phases.
#' @param session vector of measurement occasions or name of numeric vector within \code{data} of measurement times.
#' @param outcome vector of outcome data or name of numeric vector of outcome data within \code{data}.
#' @param design Character string to specify whether data comes from a treatment reversal, "TR", or multiple baseline, "MB", design.
#' @param center Numeric value for the centering value for session. Default is 0.
#' @param round_session Logical indicating whether to round \code{session} to the nearest integer. Defaults to \code{TRUE}.
#' @param treatment_name (Optional) character string corresponding to the name of the treatment phase.
#' @param data (Optional) dataset to be cleaned. Must be a \code{data.frame}. 
#' 
#' @note If treatment_name is left null it will choose the second level of the phase variable to be the treatment phase.
#' 
#' @export 
#' 
#' @return A cleaned SCD dataset that can be used for model fitting and effect size calculation.
#' 
#' @examples
#' data(Laski)
#' preprocess_SCD(case = case, phase = treatment,
#'                session = time, outcome = outcome, 
#'                design = "MB", center = 4, data = Laski)
#' 
#'           

preprocess_SCD <- function(case, phase, session, outcome, 
                           design, 
                           center = 0, 
                           round_session = TRUE,
                           treatment_name = NULL, 
                           data = NULL) {
  
  if (missing(case)) stop("Please specify a case variable.")
  if (missing(phase)) stop("Please specify a phase variable.")
  if (missing(session)) stop("Please specify a session variable.")
  if (missing(outcome)) stop("Please specify an outcome variable.")
  if (missing(design)) stop("Please specify a study design of 'MB' (multiple baseline) or 'TR' (treatment reversal).")
  
  case_call <- substitute(case)
  phase_call <- substitute(phase)
  session_call <- substitute(session)
  outcome_call <- substitute(outcome)
  
  if (!is.null(data)) {
    env <- list2env(data, parent = parent.frame())
    case <- eval(case_call, env)
    phase <- eval(phase_call, env)
    session <- eval(session_call, env)
    outcome <- eval(outcome_call, env)
  }
  
  # get treatment name
  if (is.null(treatment_name)) {
    treatment_name <- levels(as.factor(phase))[2]
  }
  
  # get the data
  dat <- data.frame(case = factor(case, levels = unique(case)),
                    phase = factor(phase, levels = unique(phase)),
                    session, outcome)
  
  # round session
  if (round_session == TRUE) {
    dat$session <- as.integer(round(dat$session))
  } else {
    dat$session <- as.integer(dat$session)
  }
  
  unique_sessions <- tapply(dat$session, dat$case, function(x) isTRUE(all.equal(x, unique(x))))
  
  if (!all(unique_sessions, na.rm = TRUE)) {
    
    stop("Session variable contains repeated values. Please ensure that each data point has a unique value within each case.")
    
  }   
  
  dat <- dat[!is.na(dat$outcome), ]
  dat$trt <- as.numeric(dat$phase == treatment_name) # create trt variable

  if (design == "MB") {
    dat$session_trt <- suppressWarnings(
      unsplit(by(dat, dat$case, session_by_treatment, trt_phase = treatment_name), dat$case)
    ) 
    dat$session <- dat$session - center
    if (!is.null(data)) {
      names(dat)[6] <- paste0(as.character(session_call), "_trt")
    } else {
      names(dat)[6] <- paste0(as.character(session_call)[3], "_trt")
    }
  } else {
    dat$phase_pair <- unsplit(by(dat, dat$case, phase_pairs), dat$case)
    if (!is.null(data)) {
      names(dat)[6] <- paste0(as.character(phase_call), "_pair")
    } else {
      names(dat)[6] <- paste0(as.character(phase_call)[3], "_pair")
    }
  }

  dat <- droplevels(dat)

  if (!is.null(data)) {
    names(dat)[1:4] <- c(as.character(case_call), as.character(phase_call),
                         as.character(session_call), as.character(outcome_call))
  } else {
    names(dat)[1:4] <- c(as.character(case_call)[3], as.character(phase_call)[3],
                         as.character(session_call)[3], as.character(outcome_call)[3])
  }
  
  return(dat)
  
}
