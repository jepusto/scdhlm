
#---------------------------------------------------------------
# calculate session-by-treatment interaction
#---------------------------------------------------------------

#' @title Calculate session-by-treatment interactions for a unique case
#'
#' @description Calculate session-by-treatment interactions based on phases and
#'   session numbering.
#'
#' @param x The subset of a single-case dataset for a unique case.
#' @param trt_phase character string indicating the phase value corresponding to
#'   the treatment condition.
#'   
#' @export
#' 

session_by_treatment <- function(x, trt_phase) {
  .Deprecated("preprocess_SCD", msg = "'session_by_treatment()' is deprecated and may be removed in a later version of the package. Please use 'preprocess_SCD()' instead.")
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}

#---------------------------------------------------------------
# calculate phase-pairs based on phases and session numbering
#---------------------------------------------------------------

#' @title Calculate phase-pairs for a unique case
#' 
#' @description Calculate phase-pairs based on phases and session numbering. 
#' 
#' @param x The subset of a single-case dataset for a unique case.
#' @export 
#' 

phase_pairs <- function(x) {
  
  .Deprecated("preprocess_SCD", msg = "'phase_pairs()' is deprecated and may be removed in a later version of the package. Please use 'preprocess_SCD()' instead.")
  
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

#' @title Graph Single Case Design Data
#' 
#' @description Graphs single case design data for treatment reversal and multiple baseline designs. 
#' 
#' @param case vector of case indicators or name of a character or factor vector within \code{data} indicating unique cases.
#' @param phase vector of treatment indicators or name of a character or factor vector within \code{data} indicating unique treatment phases.
#' @param session vector of measurement occasions or name of numeric vector within \code{data} of measurement times.
#' @param outcome vector of outcome data or name of numeric vector of outcome data within \code{data}.
#' @param design Character string to specify whether data comes from a treatment reversal, "TR", or multiple baseline, "MB", design.
#' @param treatment_name (Optional) character string corresponding to the name of the treatment phase.
#' @param model_fit (Optional) lme fitted model that adds predicted values to graph
#' @param data (Optional) dataset to use for analysis. Must be a \code{data.frame}.
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

graph_SCD <- function(case, phase, session, outcome, design, treatment_name = NULL, model_fit = NULL, data = NULL) {
  
  phase_pair <-  phase_time  <- NULL
  
  case_call <- substitute(case)
  phase_call <- substitute(phase)
  session_call <- substitute(session)
  outcome_call <- substitute(outcome)
  
  if (!is.null(model_fit) & is.null(data)) {
    data <- nlme::getData(model_fit)
  }
  
  dat <- do.call(preprocess_SCD,
                 args = list(case = case_call, 
                             phase = phase_call,
                             session = session_call,
                             outcome = outcome_call, 
                             design = design,
                             treatment_name = treatment_name,
                             data = data))
  names(dat)[1:4] <- c("case", "phase", "session", "outcome")
  
  phase_line_dat <- phase_lines_by_case(dat)

  if (design=="MB") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = phase, shape = phase))
  } else {
    names(dat)[6] <- "phase_pair"
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

