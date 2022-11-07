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
#' @description Graphs single case design data for treatment reversal and
#'   multiple baseline designs.
#'
#' @param design Character string to specify whether data comes from a treatment
#'   reversal (\code{"TR"}), multiple baseline across participants
#'   (\code{"MBP"}), replicated multiple baseline across behaviors
#'   (\code{"RMBB"}), or clustered multiple baseline across participants (\code{"CMB"}).
#' @param case vector of case indicators or name of a character or factor vector
#'   within \code{data} indicating unique cases.
#' @param phase vector of treatment indicators or name of a character or factor
#'   vector within \code{data} indicating unique treatment phases.
#' @param session vector of measurement occasions or name of numeric vector
#'   within \code{data} of measurement times.
#' @param outcome vector of outcome data or name of numeric vector of outcome
#'   data within \code{data}.
#' @param cluster (Optional) vector of cluster indicators or name of a character
#'   or factor vector within \code{data} indicating clusters.
#' @param series (Optional) vector of series indicators or name of a character
#'   or factor vector within \code{data} indicating series.
#' @param treatment_name (Optional) character string corresponding to the name
#'   of the treatment phase.
#' @param model_fit (Optional) lme fitted model that adds predicted values to
#'   graph
#' @param data (Optional) dataset to use for analysis. Must be a
#'   \code{data.frame}.
#'
#' @note If treatment_name is left null it will choose the second level of the
#'   phase variable to be the treatment phase.
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
#' graph_SCD(design="TR",
#'           case=case, phase=condition,
#'           session=session, outcome=outcome,
#'           treatment_name = "treatment",
#'           data=Anglesea)
#'
#' data(BartonArwood)
#' graph_SCD(design="MBP",
#'           case=case, phase=condition,
#'           session=session, outcome=outcome,
#'           treatment_name = "B",
#'           data=BartonArwood)
#'
#' data(Thiemann2001)
#' graph_SCD(design="RMBB",
#'           case=case, series = series, phase=treatment,
#'           session=time, outcome=outcome,
#'           treatment_name = "treatment",
#'           data=Thiemann2001)
#'
#' data(Bryant2018)
#' graph_SCD(design="CMB",
#'           cluster=group, case=case, phase=treatment,
#'           session=session, outcome=outcome,
#'           treatment_name = "treatment",
#'           data=Bryant2018)
#'           
#' }


graph_SCD <- function(design, case, phase, session, outcome, 
                      cluster = NULL, series = NULL, 
                      treatment_name = NULL, model_fit = NULL, 
                      data = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the ggplot2 package. Please install it.", call. = FALSE)
  }

  
  # preprocess_SCD() using the passed arguments
  mf <- match.call()
  m <- match(c("design", "case", "phase","session","outcome","cluster","series","treatment_name","data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(preprocess_SCD)
  
  if (!is.null(model_fit) & is.null(data)) {
    env <- new.env(parent = parent.frame())
    env$data <- nlme::getData(model_fit)
    mf$data <- quote(data)
  } else {
    env <- parent.frame()
  }
  
  dat <- eval(mf, envir = env)

  design <- match.arg(design, choices = c("MBP","TR","RMBB","CMB"))
  
  phase_pair <- phase_time <- NULL

  case_name <- deparse(substitute(case))
  phase_name <- deparse(substitute(phase))
  session_name <- deparse(substitute(session))
  
  

  if (design %in% c("MBP", "TR")) {
    phase_line_dat <- phase_lines_by_case(dat[[case_name]], dat[[phase_name]], dat[[session_name]])
    names(phase_line_dat)[1] <- case_name
  } else if (design == "RMBB") {
    series_name <- deparse(substitute(series))
    dat$caseSeries <- as.factor(paste(dat[[case_name]], dat[[series_name]], sep = "-"))
    phase_line_dat <- phase_lines_by_case(dat$caseSeries, dat[[phase_name]], dat[[session_name]])
    names(phase_line_dat)[1] <- "caseSeries"
  } else if (design == "CMB") {
    cluster_name <- deparse(substitute(cluster))
    dat$clusterCase <- as.factor(paste(dat[[cluster_name]], dat[[case_name]], sep = "-"))
    phase_line_dat <- phase_lines_by_case(dat$clusterCase, dat[[phase_name]], dat[[session_name]])
    phase_line_dat[[cluster_name]] <- sub("\\-.*", "", phase_line_dat$case)
    phase_line_dat <- do.call(rbind, by(phase_line_dat, phase_line_dat[[cluster_name]], function(x) x[which.min(x$phase_time), ] ))
    phase_line_dat <- phase_line_dat[,-1]
  }

  if (design=="MBP") {
    p <- 
      ggplot2::ggplot(dat, ggplot2::aes({{session}}, {{outcome}}, color = {{phase}}, shape = {{phase}})) +
      ggplot2::facet_wrap(ggplot2::sym(case_name), ncol = 1)
  } else if (design == "TR") {
    names(dat)[6] <- "phase_pair"
    p <- 
      ggplot2::ggplot(dat, ggplot2::aes({{session}}, {{outcome}}, color = {{phase}}, shape = {{phase}}, group = interaction({{phase}}, phase_pair))) +
      ggplot2::facet_wrap(ggplot2::sym(case_name), ncol = 1)
  } else if (design == "RMBB") {
    p <- 
      ggplot2::ggplot(dat, ggplot2::aes({{session}}, {{outcome}}, color = {{case}}, group = {{phase}})) +
      ggplot2::facet_wrap(~ caseSeries, dir = "v", ncol = length(unique(dat[[case_name]])), 
                          labeller = ggplot2::labeller(caseSeries = ggplot2::label_wrap_gen(25)))
  } else if (design == "CMB") {
    p <- 
      ggplot2::ggplot(dat, ggplot2::aes({{session}}, {{outcome}}, shape = {{phase}}, color = {{case}})) +
      ggplot2::facet_wrap(ggplot2::sym(cluster_name), ncol = 1) + 
      ggplot2::guides(color = "none")
  } else {
    stop("The `design` argument must be one of 'TR', 'MBP', 'RMBB', or 'CMB'.")
  }

  p <- 
    p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "", shape = "", x = session_name) + 
    ggplot2::geom_vline(data = phase_line_dat, ggplot2::aes(xintercept = phase_time, color = NULL, group = NULL), linetype = "dashed")

  # With model fit
  if (!is.null(model_fit)) {
    
    dat$fitted <- if(design == "CMB") predict(model_fit, level = 1) else predict(model_fit)

    p <- p + ggplot2::geom_line(data = dat, ggplot2::aes(y = fitted), linewidth = 0.8)

  }

  p
  
}

