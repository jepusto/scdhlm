#---------------------------------------------------------------
# calculate phase border times
#---------------------------------------------------------------

phase_lines <- function(phase, session) {
  phase <- phase[order(session)]
  n <- length(phase)
  switches <- which(phase[2:n] != phase[1:(n-1)])
  (session[switches] + session[switches + 1]) / 2
}

phase_lines_by_case <- function(case, phase, session) {
  
  phases_by <- split(phase, case)
  sessions_by <- split(session, case)
  
  phase_line <- mapply(phase_lines, phase = phases_by, session = sessions_by, SIMPLIFY = FALSE)
  
  case_name <- rep(names(phase_line), times = lengths(phase_line))
  case_name <- factor(case_name, levels = levels(case))
  
  data.frame(case = case_name, phase_time = as.vector(unlist(phase_line)))
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
#' 
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' 
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
#' }

graph_SCD <- function(case, phase, session, outcome, design, treatment_name = NULL, model_fit = NULL, data = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  
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
  
  phase_line_dat <- phase_lines_by_case(dat$case, dat$phase, dat$session)

  if (design=="MB") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = phase, shape = phase))
  } else {
    names(dat)[6] <- "phase_pair"
    p <- ggplot2::ggplot(dat, ggplot2::aes(session, outcome, color = phase, shape = phase, group = interaction(phase, phase_pair)))
  }

  p <- 
    p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(case ~ .) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "", shape = "") +
    ggplot2::geom_vline(data = phase_line_dat, ggplot2::aes(xintercept = phase_time), linetype = "dashed")
  
  if (!is.null(data)) {
    p <- 
      p + 
      ggplot2::xlab(as.character(session_call)) +
      ggplot2::ylab(as.character(outcome_call))
  } else {
    p <- 
      p + 
      ggplot2::xlab(as.character(session_call)[3]) +
      ggplot2::ylab(as.character(outcome_call)[3])
  }

  # With model fit
  if (!is.null(model_fit)) {

    dat$fitted <- predict(model_fit)

    p <- p + ggplot2::geom_line(data = dat, ggplot2::aes(session, fitted), size = 0.8)

  }

  p
  
}

