# Calculate timing defaults
default_times <- function(x) {
  range <- range(x$session)
  case_base_last <- with(x, tapply(session[trt==0], case[trt==0], max))
  case_trt_range <- with(x, tapply(session[trt==1], case[trt==1], function(x) diff(range(x)) + 1))
  A <- min(case_base_last)
  B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  list(range = range, A = A, B = B)
}

write_formula <- function(powers, var_names) {
  var_name_1 <- if (var_names[1] == "NULL") NULL else var_names[1]
  if (is.null(powers)) {
    var_name_1
  } else {
    paste(c(if (0 %in% powers) var_names[2] else var_name_1, 
            paste0("I(",var_names[3],"^",powers,")")[powers != 0]), 
          collapse = " + ")
  }
}

#' @title A wrapper function for calculating design comparable effect sizes
#'
#' @description Clean single case design data for treatment reversal and
#'   multiple baseline designs. Fit a multi-level model using restricted maximum
#'   likelihood estimation. Estimate a standardized mean difference effect size.
#'
#' @inheritParams preprocess_SCD
#' @param FE_base numerical value indicating the fixed effect at the baseline
#'   phase. Take value from 0 (the default) up to 6. The value of 0 represents
#'   including level as a fixed effect. The value of 1 to 6 represents including
#'   linear trend to sextic trend as a fixed effect.
#' @param RE_base numerical value indicating the random effect at the baseline
#'   phase. Take value from 0 (the default) up to 6. The value of 0 represents
#'   including level as a random effect. The value of 1 to 6 represents
#'   including linear trend to sextic trend as a random effect.
#' @param RE_base_2 numerical value indicating the random effect at the baseline
#'   phase for the cluster level in clustered multiple baseline design across
#'   participants or for the case level in replicated multiple baseline across
#'   behaviors. Take value from 0 (the default) up to 6. The value of 0
#'   represents including level as a random effect. The value of 1 to 6
#'   represents including linear trend to sextic trend as a random effect.
#' @param FE_trt numerical value indicating the fixed effect at the treatment
#'   phase. Take value from 0 (the default) up to 6. The value of 0 represents
#'   including change in level as a fixed effect. The value of 1 to 6 represents
#'   including change in linear trend to sextic trend as a fixed effect.
#' @param RE_trt numerical value indicating the random effect at the treatment
#'   phase. Take value from 0 (the default) up to 6. The value of 0 represents
#'   including change in level as a random effect. The value of 1 to 6
#'   represents including change in linear trend to sextic trend as a random
#'   effect.
#' @param RE_trt_2 numerical value indicating the random effect at the treatment
#'   phase for the cluster level in clustered multiple baseline design across
#'   participants or for the case level in replicated multiple baseline across
#'   behaviors. Take value from 0 (the default) up to 6. The value of 0
#'   represents including change in level as a random effect. The value of 1 to
#'   6 represents including change in linear trend to sextic trend as a random
#'   effect.
#' @param corStruct (Optional) character string indicating the correlation
#'   structure of session-level errors. Include \code{"AR1"} (default),
#'   \code{"MA1"}, and \code{"IID"}.
#' @param varStruct (Optional) character string indicating the
#'   heteroscedasticity structure of session-level errors. Include \code{"hom"}
#'   (default) and \code{"het"}.
#' @param A the time point immediately before the start of treatment.
#' @param B the time point at which outcomes are measured in the hypothetical
#'   between-group design.
#' @param cover confidence level.
#' @param bound numerical tolerance for non-centrality parameter in
#'   \code{\link[stats]{qt}}.
#' @param symmetric If \code{TRUE} (the default), use a symmetric confidence
#'   interval. If \code{FALSE}, use a non-central t approximation to obtain an
#'   asymmetric confidence interval.
#' @param summary Logical indicating whether to return a data frame with effect
#'   size estimates and other information. If \code{TRUE} (default), return a
#'   dataframe containing the effect size estimate, standard error, confidence
#'   interval, and other information. If \code{FALSE}, return a list with effect
#'   size estimate, degrees of freedom, and other information.
#' @param ... further arguments.
#'
#' @export
#'
#' @return A data frame containing the design-comparable effect size estimate,
#'   standard error, confidence interval, and other information, or a list of
#'   `g_mlm()` object.
#'
#' @examples
#' data(Laski)
#' calc_BCSMD(design = "MBP",
#'            case = case, phase = treatment,
#'            session = time, outcome = outcome,
#'            center = 4,
#'            FE_base = 0, RE_base = 0, FE_trt = 0,
#'            data = Laski)
#'
#' data(Anglesea)
#' calc_BCSMD(design = "TR",
#'            case = case, phase = condition,
#'            session = session, outcome = outcome,
#'            treatment_name = "treatment",
#'            FE_base = 0, RE_base = 0, FE_trt = 0,
#'            data = Anglesea)
#'
#' data(Thiemann2001)
#' calc_BCSMD(design = "RMBB",
#'            case = case, series = series, phase = treatment,
#'            session = time, outcome = outcome,
#'            FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
#'            data = Thiemann2001)
#'
#' data(Bryant2018)
#' calc_BCSMD(design = "CMB",
#'            cluster = group, case = case, phase = treatment,
#'            session = session, outcome = outcome,
#'            treatment_name = "treatment",
#'            FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
#'            data = Bryant2018)
#'
#' 

calc_BCSMD <- function(design, 
                       case, phase, session, outcome, 
                       cluster = NULL, series = NULL,
                       center = 0, 
                       round_session = TRUE,
                       treatment_name = NULL,
                       FE_base = 0, RE_base = 0, RE_base_2 = NULL, FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                       corStruct = "AR1", varStruct = "hom",
                       A = NULL, B = NULL, 
                       cover = 95, bound = 35, symmetric = TRUE,
                       summary = TRUE, 
                       data = NULL, ...) {
  
  # clean data
  design <- match.arg(design, choices = c("MBP","TR","RMBB","CMB"))
  
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
  
  env <- if (!is.null(data)) list2env(data, parent = parent.frame()) else env <- parent.frame()
  case <- eval(case_call, env)
  phase <- eval(phase_call, env)
  session <- eval(session_call, env)
  outcome <- eval(outcome_call, env)
  cluster <- eval(cluster_call, env)
  series <- eval(series_call, env)
  
  dat <- preprocess_SCD(design = design, cluster = cluster, series = series, center = center, 
                        case = case, phase = phase, session = session, outcome = outcome, 
                        round_session = round_session, treatment_name = treatment_name)
  
  if (design == "MBP") {
    names(dat) <- c("case", "phase", "session", "outcome", "trt", "session_trt")
  } else if (design == "TR") {
    names(dat) <- c("case", "phase", "session", "outcome", "trt", "phase_pair")
  } else if (design == "RMBB") {
    names(dat) <- c("case", "series", "phase", "session", "outcome", "trt", "session_trt")
  } else {
    names(dat) <- c("cluster", "case", "phase", "session", "outcome", "trt", "session_trt")
  }
  
  # fit the model
  if (design %in% c("MBP", "RMBB", "CMB")) {
    session_FE <- write_formula(FE_base, c("0","1","session"))
    trt_FE <- write_formula(FE_trt, c("NULL", "trt", "session_trt"))
    fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
    
    session_RE <- write_formula(RE_base, c("0","1","session"))
    trt_RE <- write_formula(RE_trt, c("NULL","trt","session_trt"))
    if (design == "MBP") {
      random <- as.formula(paste(" ~ ",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
    } else {
      session_RE_2 <- write_formula(RE_base_2, c("0","1","session"))
      trt_RE_2 <- write_formula(RE_trt_2, c("NULL","trt","session_trt"))
      if (design == "RMBB") {
        random <- eval(parse(text = paste0("list(case = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                           ", series = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
      } else {
        random <- eval(parse(text = paste0("list(cluster = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                           ", case = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
      }
    }
  } else if (design == "TR") {
    session_FE <- if (is.null(FE_base) | !(0 %in% FE_base)) "0" else "1"
    trt_FE <- if (is.null(FE_trt) | !(0 %in% FE_trt)) NULL else "trt"
    fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
    
    session_RE <- if (is.null(RE_base) | !(0 %in% RE_base)) "0" else "1"
    trt_RE <- if (is.null(RE_trt) | !(0 %in% RE_trt)) NULL else "trt"
    random <- as.formula(paste(" ~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
  }

  nesting_str <- switch(design, 
                        MBP = "case",
                        TR = "case",
                        RMBB = "case/series",
                        CMB = "cluster/case")
  
  cor_struct <- switch(corStruct,
                       `MA1` = eval(parse(text = paste0("corARMA(0, ~ session | ", nesting_str, ", p = 0, q = 1)"))),
                       `AR1` = eval(parse(text = paste0("corAR1(", 0.01, ", ~ session | ", nesting_str, ")"))),
                       `IID` = NULL)
  
  if (varStruct == "het") {
    var_struct <- eval(parse(text = "varIdent(form = ~ 1 | phase)"))
  } else if (varStruct == "hom") {
    var_struct <- NULL
  }

  W <- TRUE
  E <- NULL
  m_fit <- withCallingHandlers(
    tryCatch(lme(fixed = fixed, random = random,
                 correlation = cor_struct,
                 weights = var_struct,
                 data = dat,
                 control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE)),
             error = function(e) E <<- e),
    warning = function(w) W <<- w)
  m_fit$call$fixed <- fixed
  m_fit$call$random <- random
  
  # calculate effect size
  if (is.null(A)) A <- default_times(dat)$A
  if (is.null(B)) B <- default_times(dat)$B

  p_const <- c(rep(0L, length(FE_base)), (B - A)^as.integer(FE_trt))
  
  r_dim <- length(RE_base) + length(RE_trt)
  r_const_dim <- r_dim * (r_dim + 1) / 2
  bc_vec <- (B - center)^as.integer(RE_base)
  bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
  r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
  r_const_trt <- rep(0L, r_const_dim - length(r_const_base))
  
  if (design %in% c("MBP", "TR")) {
    r_const <- c(r_const_base,
                 r_const_trt,
                 rep(0L, length(m_fit$modelStruct$corStruct)),
                 rep(0L, length(m_fit$modelStruct$varStruct)),
                 1L)
  } else {
    
    r_dim2 <- length(RE_base_2) + length(RE_trt_2)
    r_const_dim2 <- r_dim2 * (r_dim2 + 1) / 2
    bc_vec2 <- (B - center)^as.integer(RE_base_2)
    bc_mat2 <- 2 * tcrossprod(bc_vec2) - diag(bc_vec2^2)
    r_const_base2 <- bc_mat2[upper.tri(bc_mat2, diag = TRUE)]
    r_const_trt2 <- rep(0L, r_const_dim2 - length(r_const_base2))
    
    r_const <- c(r_const_base,
                 r_const_trt,
                 r_const_base2,
                 r_const_trt2,
                 rep(0L, length(m_fit$modelStruct$corStruct)),
                 rep(0L, length(m_fit$modelStruct$varStruct)),
                 1L)
  }

  g_RML <- g_mlm(m_fit, p_const = p_const, r_const = r_const, infotype = "expected")

  if (summary) {

    ES_summary <- data.frame(
      ES = as.numeric(g_RML$g_AB),
      SE = as.numeric(g_RML$SE_g_AB)
    )

    if (design %in% c("RMBB", "CMB")) {
      rho_level2 <- round(with(g_RML, (theta$Tau[[1]][1] + theta$Tau[[2]][1]) /
                                 (theta$Tau[[1]][1] + theta$Tau[[2]][1] + theta$sigma_sq)), 3)
      rho_level3 <- round(with(g_RML, theta$Tau[[2]][1] /
                                 (theta$Tau[[1]][1] + theta$Tau[[2]][1] + theta$sigma_sq)), 3)
      g_RML$rho <- paste0("Level2:", rho_level2, "  Level3:", rho_level3)
    } else {
      g_RML$rho <- with(g_RML, theta$Tau[[1]][1] / (theta$Tau[[1]][1] + theta$sigma_sq))
    }

    g_RML$phi <- g_RML$theta$cor_params
    g_RML$var_param <- g_RML$theta$var_params
    
    CI <- CI_g(g_RML, cover = cover / 100L, bound = bound, symmetric = symmetric)
    
    ES_summary$CI_L <- if (CI[1] < 100 & CI[1] > -100) CI[1] else format(CI[1], scientific = TRUE)
    ES_summary$CI_U <- if (CI[2] < 100 & CI[2] > -100) CI[2] else format(CI[2], scientific = TRUE)
    ES_summary$df <- g_RML$nu
    ES_summary$phi <- g_RML$phi
    ES_summary$var_param <- if (varStruct == "het") g_RML$var_param else NA_real_
    ES_summary$rho <- g_RML$rho
    
    if (design %in% c("MBP", "RMBB", "CMB") & !is.null(A) & !is.null(B)) {
      ES_summary$A <- A
      ES_summary$B <- B
    } else {
      ES_summary$A <- NA
      ES_summary$B <- NA
    }
    
    CI_names <- paste0(cover, "% CI ", c("(lower)", "(upper)"))
    row.names(ES_summary) <- NULL
    names(ES_summary) <- c("BC-SMD estimate","Std. Error", CI_names,
                           "Degrees of freedom","Auto-correlation", "Variance parameter", "Intra-class correlation",
                           "Initial treatment time","Follow-up time")
    
    return(ES_summary)

  } else {

    return(g_RML)

  }
  
}
