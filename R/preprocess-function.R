
#---------------------------------------------------------------
# calculate session-by-treatment interaction
#---------------------------------------------------------------

#' @title Calculate session-by-treatment interactions for a unique case
#'
#' @description Calculate session-by-treatment interactions based on phases and
#'   session numbering.
#'
#' @param phase vector of treatment indicators or a character or factor vector indicating unique treatment phases.
#' @param session numeric vector of measurement occasions.
#' @param trt_phase character string indicating the phase value corresponding to
#'   the treatment condition.
#'   
#' @export
#' 

session_by_treatment <- function(phase, session, trt_phase) {
  pmax(0, session - min(session[phase==trt_phase]) + 1)
}



#---------------------------------------------------------------
# calculate phase-pairs based on phases and session numbering
#---------------------------------------------------------------

#' @title Calculate phase-pairs for a unique case
#' 
#' @description Calculate phase-pairs based on phases and session numbering. 
#' 
#' @param phase vector of treatment indicators or a character or factor vector indicating unique treatment phases.
#' @param session numeric vector of measurement occasions.
#' @export 
#' 
#' @examples 
#' 
#' phases <- rep(c("A","B","A","B"), each = 4)
#' sessions <- 1:length(phases)
#' 
#' phase_pairs(phases, sessions)
#' 
#' phases <- rep(c("A","B","C","A","B","C","D"), each = 4)
#' phase_pairs(phases)
#' 
#' phases <- rep(c("B","A","C","B","A","B","C","A"), each = 4)
#' phase_pairs(phases)
#' 

phase_pairs <- function(phase, session = seq_along(phase)) {
  phase <- as.character(phase[order(session)])
  n <- length(phase)
  y <- rep(1L, n)
  condition_list <- phase[1]
  for (i in 2:n) {
    if (phase[i] == phase[i - 1]) {
      y[i] <- y[i - 1]
    } else if (!(phase[i] %in% condition_list)) {
      y[i] <- y[i - 1]
      condition_list <- c(condition_list, phase[i])
    } else {
      y[i] <- y[i - 1] + 1L
      condition_list <- phase[i]
    }
  }
  y[order(order(session))]
}

#' @title Clean Single Case Design Data
#' 
#' @description Clean single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param center Numeric value for the centering value for session. Default is 0.
#' @param round_session Logical indicating whether to round \code{session} to the nearest integer. Defaults to \code{TRUE}.
#' @inheritParams graph_SCD
#' 
#' @note If treatment_name is left null it will choose the second level of the phase variable to be the treatment phase.
#' 
#' @export 
#' 
#' @return A cleaned SCD dataset that can be used for model fitting and effect size calculation.
#' 
#' @examples
#' data(Laski)
#' preprocess_SCD(design = "MBP", 
#'                case = case, phase = treatment,
#'                session = time, outcome = outcome, 
#'                center = 4, data = Laski)
#'                
#' data(Anglesea) 
#' preprocess_SCD(design="TR",
#'                case=case, phase=condition,
#'                session=session, outcome=outcome,
#'                treatment_name = "treatment",
#'                data=Anglesea)
#'                
#' data(Thiemann2001)
#' preprocess_SCD(design = "RMBB", 
#'                case = case, series = series, phase = treatment, 
#'                session = time, outcome = outcome, 
#'                data = Thiemann2001)
#'           

preprocess_SCD <- function(design, 
                           case, phase, session, outcome, 
                           cluster = NULL, series = NULL,
                           center = 0, 
                           round_session = TRUE,
                           treatment_name = NULL,
                           data = NULL) {
  
  if (missing(case)) stop("Please specify a case variable.")
  if (missing(phase)) stop("Please specify a phase variable.")
  if (missing(session)) stop("Please specify a session variable.")
  if (missing(outcome)) stop("Please specify an outcome variable.")
  if (missing(design)) stop("Please specify a study design of 'MB' (multiple baseline), 
                            'TR' (treatment reversal),
                            'RMBB' (replicated multiple baseline across behaviors),
                            'CMB' (clustered multiple baseline).")
  if (design == "CMB" & missing(cluster)) stop("Please specify a cluster variable.")
  if (design == "RMBB" & missing(series)) stop("Please specify a series variable.")
  
  case_call <- substitute(case)
  phase_call <- substitute(phase)
  session_call <- substitute(session)
  outcome_call <- substitute(outcome)
  cluster_call <- substitute(cluster)
  series_call <- substitute(series)
  
  
  if (!is.null(data)) {
    env <- list2env(data, parent = parent.frame())
    case <- eval(case_call, env)
    phase <- eval(phase_call, env)
    session <- eval(session_call, env)
    outcome <- eval(outcome_call, env)
    cluster <- eval(cluster_call, env)
    series <- eval(series_call, env)
    
    case_name <- as.character(case_call)
    phase_name <- as.character(phase_call)
    session_name <- as.character(session_call)
    outcome_name <- as.character(outcome_call)
    cluster_name <- as.character(cluster_call)
    series_name <- as.character(series_call)
    
  } else {
    case_name <- as.character(case_call)[3]
    phase_name <- as.character(phase_call)[3]
    session_name <- as.character(session_call)[3]
    outcome_name <- as.character(outcome_call)[3]
    cluster_name <- as.character(cluster_call)[3]
    series_name <- as.character(series_call)[3]
  }
  
  # get treatment name
  if (is.null(treatment_name)) {
    treatment_name <- levels(as.factor(phase))[2]
  }
  
  # get the data
  if (design == "RMBB") {
    dat <- data.frame(case = factor(case, levels = unique(case)),
                      series = factor(series, levels = unique(series)),
                      phase = factor(phase, levels = unique(phase)),
                      session, 
                      outcome)
  } else if (design == "CMB") {
    dat <- data.frame(cluster = factor(cluster, levels = unique(cluster)),
                      case = factor(case, levels = unique(case)),
                      phase = factor(phase, levels = unique(phase)),
                      session, 
                      outcome)
  } else {
    dat <- data.frame(case = factor(case, levels = unique(case)),
                      phase = factor(phase, levels = unique(phase)),
                      session, 
                      outcome)
  }
  
  # round session
  if (round_session == TRUE) {
    dat$session <- as.integer(round(dat$session))
  } else {
    dat$session <- as.integer(dat$session)
  }
  
  
  dat <- dat[!is.na(dat$outcome), ]
  dat$trt <- as.numeric(dat$phase == treatment_name) # create trt variable

  by_var <- switch(design,
                   MBP = dat$case,
                   TR = dat$case,
                   RMBB = as.factor(paste(dat$case, dat$series, sep = "-")),
                   CMB = as.factor(paste(dat$cluster, dat$case, sep = "-")))

  session_by <- split(dat$session, by_var)
  phase_by <- split(dat$phase, by_var)
  
  if (design %in% c("MBP","RMBB","CMB")) {
    
    # calculate session-by-treatment interaction
    session_trt_by <- mapply(session_by_treatment, 
                             phase = phase_by, 
                             session = session_by,
                             MoreArgs = list(trt_phase = treatment_name), SIMPLIFY = FALSE)
    dat$session_trt <- unsplit(session_trt_by, by_var)
    dat$session <- dat$session - center
    names(dat)[which(names(dat) == "session_trt")] <- paste0(session_name, "_trt")
    
  } else if (design == "TR") {
    
    # calculate phase pairs for treatment reversal designs
    phase_pairs_by <- mapply(phase_pairs, phase = phase_by, session = session_by, SIMPLIFY = FALSE)
    dat$phase_pair <- unsplit(phase_pairs_by, dat$case)
    names(dat)[which(names(dat) == "phase_pair")] <- paste0(phase_name, "_pair")
    
  }

  dat <- droplevels(dat)
  unique_sessions <- tapply(dat$session, by_var, function(x) isTRUE(all.equal(x, unique(x))))
  
  if (!all(unique_sessions, na.rm = TRUE)) {
    stop("Session variable contains repeated values. Please ensure that each data point has a unique value within each case.")
  }

  if (design %in% c("MBP", "TR")) {
    names(dat)[1:4] <- c(case_name, phase_name, session_name, outcome_name)
  } else if (design == "RMBB") {
    names(dat)[1:5] <- c(case_name, series_name, phase_name, session_name, outcome_name)
  } else if (design == "CMB") {
    names(dat)[1:5] <- c(cluster_name, case_name, phase_name, session_name, outcome_name)
  }
  
  return(dat)
  
}
