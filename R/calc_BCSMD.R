# Calculate timing defaults

#' @title Calculate default initial treatment time and follow-up time.
#'
#' @description Calculate the default initial treatment time and follow-up time
#'   that are used to define and estimate the between-case standardized mean
#'   differences for multiple baseline designs and variations.
#'
#' @inheritParams graph_SCD
#'
#' @note If treatment_name is left null, it will choose the second level of the
#'   phase variable to be the treatment phase.
#'
#' @export
#'
#' @return A list of time range, default initial treatment time, and default
#'   follow-up time.
#'
#'
#' @examples
#' data(Laski)
#' default_times(design = "MBP", 
#'               case = case, phase = treatment, session = time, 
#'               data = Laski)
#'
#' data(Thiemann2001)
#' default_times(design = "RMBB", 
#'               case = case, series = series, 
#'               phase = treatment, session = time, 
#'               data = Thiemann2001)
#'
#' data(Bryant2018)
#' default_times(design = "CMB", 
#'               cluster = group, case = case, 
#'               phase = treatment, session = session, 
#'               data = Bryant2018)
#'               

default_times <- function(design,
                          case, phase, session, cluster = NULL, series = NULL, 
                          treatment_name = NULL, data = NULL) {
  
  design <- match.arg(design, choices = c("MBP","TR","RMBB","CMB"))
  
  if (missing(case)) stop("Please specify a case variable.")
  if (missing(phase)) stop("Please specify a phase variable.")
  if (missing(session)) stop("Please specify a session variable.")
  if (missing(design)) stop("Please specify a study design of 'MB' (multiple baseline), 
                            'RMBB' (replicated multiple baseline across behaviors),
                            'CMB' (clustered multiple baseline).")
  if (design == "CMB" & missing(cluster)) stop("Please specify a cluster variable.")
  if (design == "RMBB" & missing(series)) stop("Please specify a series variable.")
  
  case_call <- substitute(case)
  phase_call <- substitute(phase)
  session_call <- substitute(session)
  cluster_call <- substitute(cluster)
  series_call <- substitute(series)
  
  env <- if (!is.null(data)) list2env(data, parent = parent.frame()) else env <- parent.frame()
  case <- eval(case_call, env)
  phase <- eval(phase_call, env)
  session <- eval(session_call, env)
  cluster <- eval(cluster_call, env)
  series <- eval(series_call, env)
  
  if (is.null(treatment_name)) {
    treatment_name <- levels(as.factor(phase))[2]
  }
  
  # set the index for each design type
  if (design == "RMBB") {
    dat <- data.frame(case = as.factor(case),
                      series = as.factor(series),
                      phase = as.factor(phase),
                      session,
                      index = paste(case, series, sep = "-"))
  } else if (design == "CMB") {
    dat <- data.frame(cluster = as.factor(cluster),
                      case = as.factor(case),
                      phase = as.factor(phase),
                      session,
                      index = paste(cluster, case, sep = "-")) 
  } else {
    dat <- data.frame(case = as.factor(case),
                      phase = as.factor(phase),
                      session,
                      index = case)
  }
  
  dat <- droplevels(dat)
  
  # calculate the default times
  if (design != "TR") {
    range <- range(dat$session)
    case_base_last <- with(dat, tapply(session[phase != treatment_name], index[phase != treatment_name], max))
    case_trt_range <- with(dat, tapply(session[phase == treatment_name], index[phase == treatment_name], function(x) diff(range(x)) + 1))
    A <- min(case_base_last)
    B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  } else {
    range <- A <- B <- NA
  }
    
  return(list(range = range, A = A, B = B))
  
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

# product of every two elements in a sd vector

prod_sd <- function(sd) {
  comb <- combn(sd, 2)
  prod_vec <- apply(comb, 2, prod)
  return(prod_vec)
}


#' @title A convenience function for calculating design comparable effect sizes
#'
#' @description In one call, 1) clean single-case design data for treatment
#'   reversal and multiple baseline designs, 2) fit a multi-level model using
#'   restricted maximum likelihood estimation, and 3) estimate a standardized
#'   mean difference effect size.
#'
#' @inheritParams preprocess_SCD
#' @param FE_base Vector of integers specifying which fixed effect terms to
#'   include in the baseline phase. Setting \code{FE_base = 0} includes only a
#'   level. Setting \code{FE_base = c(0,1)} includes a level and a linear time
#'   trend.
#' @param RE_base Vector of integers specifying which random effect terms to
#'   include in the baseline phase. Setting \code{RE_base = 0} includes only
#'   levels (i.e., random intercepts). Setting \code{RE_base = c(0,1)} includes
#'   random levels and random linear trends.
#' @param RE_base_2 Vector of integers specifying which random effect terms to
#'   include in the baseline phase for the cluster level in clustered multiple
#'   baseline design across participants or for the case level in replicated
#'   multiple baseline across behaviors. Setting \code{RE_base_2 = 0} includes
#'   only levels (i.e., random intercepts). Setting \code{RE_base_2 = c(0,1)}
#'   includes random levels and random linear trends.
#' @param FE_trt Vector of integers specifying which fixed effect terms to
#'   include in the treatment phase. Setting \code{FE_trt = 0} includes only a
#'   change in level. Setting \code{FE_trt = c(0,1)} includes a change in level
#'   and a treatment-by-linear time trend.
#' @param RE_trt Vector of integers specifying which random effect terms to
#'   include in the treatment phase. Setting \code{RE_trt = 0} includes only
#'   random changes in level. Setting \code{RE_trt = c(0,1)} includes random
#'   changes in level and random treatment-by-linear time trends.
#' @param RE_trt_2 Vector of integers specifying which random effect terms to
#'   include in the treatment phase for the cluster level in clustered multiple
#'   baseline design across participants or for the case level in replicated
#'   multiple baseline across behaviors. Setting \code{RE_trt_2 = 0} includes
#'   only random changes in level. Setting \code{RE_trt_2 = c(0,1)} includes
#'   random changes in level and random treatment-by-linear time trends.
#' @param corStruct (Optional) character string indicating the correlation
#'   structure of session-level errors. Options are \code{"AR1"} (default),
#'   \code{"MA1"}, or \code{"IID"}.
#' @param varStruct (Optional) character string indicating the
#'   heteroscedasticity structure of session-level errors. Options are
#'   \code{"hom"} (default) or \code{"het"}, which allows for the session-level
#'   error variances to differ by phase.
#' @param Bayesian Logical indicating whether to apply Bayesian estimation
#'   methods. If \code{FALSE} (default), apply restricted maximum likelihood
#'   estimation. If \code{TRUE}, apply Bayesian estimation methods (i.e., Markov
#'   Chain Monte Carlo) to the parameters of the specified hierarchical linear
#'   model.
#' @param prior (Optional) One or more \code{brmsprior} objects created by
#'   \code{"set_prior"} in the \code{"brms"} R package. Default is \code{prior =
#'   NULL} when flat priors will be applied for Bayesian estimation.
#'   Ignored if \code{Bayesian = FALSE}.
#' @param A The time point immediately before the start of treatment in the
#'   hypothetical between-group design.
#' @param B The time point at which outcomes are measured in the hypothetical
#'   between-group design.
#' @param D Numerical indicating the treatment duration across cases. Note that
#'   \code{B = A + D} and it is not allowed to specify both \code{B} and
#'   \code{D}.
#' @param cover Confidence level.
#' @param bound Numerical tolerance for non-centrality parameter in
#'   \code{\link[stats]{qt}}.
#' @param symmetric If \code{TRUE} (the default), use a symmetric confidence
#'   interval. If \code{FALSE}, use a non-central t approximation to obtain an
#'   asymmetric confidence interval.
#' @param summary Logical indicating whether to return a data frame with effect
#'   size estimates and other information. If \code{TRUE} (default), return a
#'   \code{data.frame} containing the effect size estimate, standard error,
#'   confidence interval (or credible interval if \code{Bayesian == TRUE}), and
#'   other information. If \code{FALSE}, return a list with effect size
#'   estimate, degrees of freedom, and other information.
#' @param ... further arguments.
#'
#' @export
#'
#' @return If \code{summary == TRUE}, a data frame containing the
#'   design-comparable effect size estimate, standard error, confidence interval
#'   (or credible interval if \code{Bayesian == TRUE}), and other information.
#'   If \code{summary == FALSE}, a list containing all elements of a `g_mlm()`
#'   object, plus the fitted `lme()` model.
#'
#' @examples
#' data(Laski)
#'
#' # Change-in-levels model with fixed treatment effect
#' calc_BCSMD(design = "MBP",
#'            case = case, phase = treatment,
#'            session = time, outcome = outcome,
#'            FE_base = 0, RE_base = 0, FE_trt = 0,
#'            data = Laski)
#'
#' calc_BCSMD(design = "MBP",
#'            case = case, phase = treatment,
#'            session = time, outcome = outcome,
#'            FE_base = 0, RE_base = 0, FE_trt = 0,
#'            Bayesian = TRUE,
#'            data = Laski)
#'
#' # Model with linear time trends in baseline and treatment phases,
#' # random baseline slopes, fixed treatment effects
#' calc_BCSMD(design = "MBP",
#'            case = case, phase = treatment,
#'            session = time, outcome = outcome, center = 4,
#'            FE_base = c(0,1), RE_base = c(0,1), 
#'            FE_trt = c(0,1),
#'            data = Laski)
#'
#' calc_BCSMD(design = "MBP",
#'            case = case, phase = treatment,
#'            session = time, outcome = outcome, center = 4,
#'            FE_base = c(0,1), RE_base = c(0,1), FE_trt = c(0,1),
#'            Bayesian = TRUE,
#'            data = Laski)
#'
#'
#' data(Anglesea)
#' calc_BCSMD(design = "TR",
#'            case = case, phase = condition,
#'            session = session, outcome = outcome,
#'            treatment_name = "treatment",
#'            FE_base = 0, RE_base = 0, 
#'            FE_trt = 0,
#'            data = Anglesea)
#'
#'
#' data(Thiemann2001)
#' calc_BCSMD(design = "RMBB",
#'            case = case, series = series, phase = treatment,
#'            session = time, outcome = outcome,
#'            FE_base = 0, RE_base = 0, RE_base_2 = 0, 
#'            FE_trt = 0,
#'            data = Thiemann2001)
#'
#'
#' data(Bryant2018)
#' calc_BCSMD(design = "CMB",
#'            cluster = group, case = case, phase = treatment,
#'            session = session, outcome = outcome, center = 49,
#'            treatment_name = "treatment",
#'            FE_base = c(0,1), RE_base = 0, RE_base_2 = 0,
#'            FE_trt = c(0,1), RE_trt = NULL, RE_trt_2 = NULL,
#'            data = Bryant2018)
#'
#' calc_BCSMD(design = "CMB",
#'            cluster = group, case = case, phase = treatment,
#'            session = session, outcome = outcome, center = 49,
#'            treatment_name = "treatment",
#'            FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
#'            FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
#'            Bayesian = TRUE,
#'            data = Bryant2018)
#'
#'
#' @importFrom brms brm
#' @importFrom brms bf
#' @importFrom brms save_pars
#' @importFrom brms as_draws_matrix
#' @importFrom utils combn

calc_BCSMD <- function(design, 
                       case, phase, session, outcome, 
                       cluster = NULL, series = NULL,
                       center = 0, 
                       round_session = TRUE,
                       treatment_name = NULL,
                       FE_base = 0, RE_base = 0, RE_base_2 = NULL, FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                       corStruct = "AR1", varStruct = "hom",
                       Bayesian = FALSE, prior = NULL,
                       A = NULL, B = NULL, D = NULL,
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
  if (design == "TR" & ( any(c(FE_base, RE_base, FE_trt) != 0) | (!is.null(RE_trt) && any(RE_trt != 0)) | any(!is.null(c(RE_base_2, RE_trt_2))) )) 
    stop("Please use the default specifications for fixed and random effects in treatment reversal design.")
  
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
  
  # Set time points A, B
  if (!is.null(B) & !is.null(D)) {
    stop("Please specify either the 'B' or 'D' argument, or use the default treatment duration by setting both 'B' and 'D' to NULL.")
    
  } else {
    
    default_AB <- default_times(design = design, 
                                case = case, phase = phase, session = session, 
                                cluster = cluster, series = series,
                                treatment_name = treatment_name)
    
    if (is.null(A)) {
      A <- default_AB$A
    } 
    
    if (is.null(B) & is.null(D)) {
      B <- default_AB$B
    } else if (!is.null(D)) {
      B <- A + D
    }
  }
  
  # Model specifications
  
  if (design %in% c("MBP", "RMBB", "CMB")) {
    session_FE <- write_formula(FE_base, c("0","1","session"))
    trt_FE <- write_formula(FE_trt, c("NULL", "trt", "session_trt"))
    fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
    fixed_Bayes <- paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + "))
    
    session_RE <- write_formula(RE_base, c("0","1","session"))
    trt_RE <- write_formula(RE_trt, c("NULL","trt","session_trt"))
    if (design == "MBP") {
      random <- as.formula(paste(" ~ ",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
      random_Bayes <- paste("(",paste(c(session_RE, trt_RE), collapse = " + "), "| case )")
    } else {
      session_RE_2 <- write_formula(RE_base_2, c("0","1","session"))
      trt_RE_2 <- write_formula(RE_trt_2, c("NULL","trt","session_trt"))
      if (design == "RMBB") {
        random <- eval(parse(text = paste0("list(case = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                           ", series = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
        random_Bayes <-paste0("(", paste(c(session_RE_2, trt_RE_2), collapse = " + "), "| case) + (",
                              paste(c(session_RE, trt_RE), collapse = " + "),"| series )")
      } else {
        random <- eval(parse(text = paste0("list(cluster = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                                           ", case = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")))
        random_Bayes <- paste0("(", paste(c(session_RE_2, trt_RE_2), collapse = " + "), "| cluster) + (",
                               paste(c(session_RE, trt_RE), collapse = " + "),"| case )")
      }
    }
  } else if (design == "TR") {
    session_FE <- if (is.null(FE_base) | !(0 %in% FE_base)) "0" else "1"
    trt_FE <- if (is.null(FE_trt) | !(0 %in% FE_trt)) NULL else "trt"
    fixed <- as.formula(paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + ")))
    fixed_Bayes <- paste("outcome ~", paste(c(session_FE, trt_FE), collapse = " + "))
    
    session_RE <- if (is.null(RE_base) | !(0 %in% RE_base)) "0" else "1"
    trt_RE <- if (is.null(RE_trt) | !(0 %in% RE_trt)) NULL else "trt"
    random <- as.formula(paste(" ~",paste(c(session_RE, trt_RE), collapse = " + "), "| case"))
    random_Bayes <- paste("(",paste(c(session_RE, trt_RE), collapse = " + "), "| case)")
  }

  # p_const and bc_mat
  p_const <- c(rep(0L, length(FE_base)), (B - A)^as.integer(FE_trt))
  r_dim <- length(RE_base) + length(RE_trt)
  r_const_dim <- r_dim * (r_dim + 1) / 2
  bc_vec <- (B - center)^as.integer(RE_base)
  bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
  
  if (design %in% c("RMBB", "CMB")) {
    r_dim2 <- length(RE_base_2) + length(RE_trt_2)
    r_const_dim2 <- r_dim2 * (r_dim2 + 1) / 2
    bc_vec2 <- (B - center)^as.integer(RE_base_2)
    bc_mat2 <- 2 * tcrossprod(bc_vec2) - diag(bc_vec2^2)
  } 
  
  # Model estimation
  
  if (Bayesian) {
    
    # correlation structure in brmsformula
    
    nesting_str_Bayes <- switch(design,
                                MBP = "case",
                                TR = "case",
                                RMBB = "case:series",
                                CMB = "cluster:case")
    
    cor_struct_Bayes <- switch(corStruct,
                               `MA1` = paste0("arma(time = session, gr = ", nesting_str_Bayes, ", p = 0, q = 1)"),
                               `AR1` = paste0("arma(time = session, gr = ", nesting_str_Bayes, ", p = 1, q = 0)"),
                               `IID` = NULL)
    
    # r_const and variance components
    
    r_const_base_var <- diag(bc_mat)
    r_const_base_cor <- bc_mat[upper.tri(bc_mat, diag = FALSE)]
    
    if (design %in% c("RMBB", "CMB")) {
      r_const_base_var2 <- diag(bc_mat2)
      r_const_base_cor2 <- bc_mat2[upper.tri(bc_mat, diag = FALSE)]
    } 
    
    # fit the model
    
    m_fit <- brms::brm(
      formula = 
        if (varStruct == "het") {
          bf(as.formula(paste0(fixed_Bayes,"+",random_Bayes,"+",cor_struct_Bayes)), 
             as.formula("sigma ~ trt"), center = FALSE)
        } else {
          bf(as.formula(paste0(fixed_Bayes,"+",random_Bayes,"+",cor_struct_Bayes)), center = FALSE)
        },
      data = dat,
      prior = prior,
      chains = 4,
      iter = 2000,
      warmup = 1000,
      thin = 10,
      save_pars = save_pars(all = TRUE),
      seed = 43073051
    )
    
    # calculate the numerator of BCSMD
    
    posterior_samples_fixed <- as_draws_matrix(m_fit, variable = "^b_", regex = TRUE)
    
    if (varStruct == "het") {
      samples_fixed <- posterior_samples_fixed[,!colnames(posterior_samples_fixed) %in% c("b_sigma_Intercept","b_sigma_trt")]
      sigma_vec <- exp(2*as.vector(posterior_samples_fixed[,"b_sigma_Intercept"]))
    } else {
      samples_fixed <- posterior_samples_fixed 
      sigma_vec <- as.vector(as_draws_matrix(m_fit, variable = "sigma", regex = TRUE))
    }
    
    fixed_pconst <- samples_fixed %*% diag(p_const)
    es_num_vec <- apply(fixed_pconst, 1, sum)
    
    # calculate the denominator of BCSMD
    
    samples_r_sd <- as_draws_matrix(m_fit, variable = "^sd_", regex = TRUE)
    r_base_sd <- samples_r_sd[, 1:length(RE_base)]
    r_base_var_mat <- r_base_sd^2 %*% diag(r_const_base_var)
    r_base_var_sum <- apply(r_base_var_mat, 1, sum)
    rho <- mean(as.vector(r_base_sd[,1])^2/ (as.vector(r_base_sd[,1])^2 + sigma_vec^2))
    
    if (length(RE_base) > 1) {
      samples_r_cor <- as_draws_matrix(m_fit, variable = "^cor_", regex = TRUE)
      r_base_cor <- samples_r_cor[, 1:length(r_const_base_cor)]
      r_base_sd_prod <- do.call(rbind, apply(r_base_sd, 1, prod_sd, simplify = FALSE))
      r_base_cov_sum <- as.vector((r_base_sd_prod * r_base_cor) %*% as.matrix(r_const_base_cor))
    } else {
      r_base_cov_sum <- 0
    }
    
    if (design %in% c("RMBB", "CMB")) {
      
      r_base_sd2 <- samples_r_sd[, (r_dim + 1):(r_dim + length(RE_base_2))]
      r_base_var_mat2 <- r_base_sd2^2 %*% diag(r_const_base_var2)
      r_base_var_sum2 <- apply(r_base_var_mat2, 1, sum)
      
      r_cor_dim <- dim(samples_r_cor)[2]
      r_cor_dim_grp1 <- r_const_dim - r_dim
      
      if (r_cor_dim > r_cor_dim_grp1) {
        r_base_cor2 <- samples_r_cor[, (r_cor_dim_grp1 + 1):(r_cor_dim_grp1 + length(r_const_base_cor2))]
        r_base_sd_prod2 <- do.call(rbind, apply(r_base_sd2, 1, prod_sd, simplify = FALSE))
        r_base_cov_sum2 <- as.vector((r_base_sd_prod2 * r_base_cor2) %*% as.matrix(r_const_base_cor2))
      } else {
        r_base_cov_sum2 <- 0
      }
      
      rho_level2 <- mean((as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2) / 
        (as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2 + sigma_vec^2))
      
      rho_level3 <- mean(as.vector(r_base_sd2[,1])^2 / 
        (as.vector(r_base_sd[,1])^2 + as.vector(r_base_sd2[,1])^2 + sigma_vec^2))
      
      rho <- paste0("Level2:", rho_level2, "  Level3:", rho_level3)
      
    } else {
      
      r_base_var_sum2 <- r_base_cov_sum2 <- 0
      
    } 
    
    es_den_vec <- 
      sqrt(r_base_var_sum + r_base_cov_sum + # 2rd level 
             r_base_var_sum2 + r_base_cov_sum2 +  # 3rd level
             sigma_vec^2 # residual variance
      )
    
    # calculate BCSMD
    es_vec <- es_num_vec / es_den_vec
    
    # get the corStruct and varStruct param
    if (corStruct %in% c("AR1", "MA1")) {
      autocor_draw <- as_draws_matrix(m_fit, variable = "^ar", regex = TRUE)
      autocor_param <- mean(autocor_draw)
    } else {
      autocor_param <- NA_real_
    }
    
    if (varStruct == "het") {
      var_param_draw <- as_draws_matrix(m_fit, variable = "b_sigma_trt", regex = TRUE)
      var_param <- exp(mean(var_param_draw))
    } else {
      var_param <- NA_real_
    }
    
    g <- mean(es_vec)
    SD_g <- sd(es_vec)
    df <- 2 * (mean(es_den_vec))^2 / var(es_den_vec)
    
    # summary table
    if (summary) {
      
      # calculate credible interval
      
      ES_summary <- data.frame(
        ES = g,
        SD = SD_g,
        CI_L = quantile(es_vec, .025),
        CI_U = quantile(es_vec, .975),
        df = df,
        phi = autocor_param,
        var_param = var_param,
        rho = rho
      )
      
      if (design %in% c("MBP", "RMBB", "CMB")) {
        ES_summary$A <- A
        ES_summary$B <- B
      } else {
        ES_summary$A <- NA_real_
        ES_summary$B <- NA_real_
      }
      
      CI_names <- paste0(cover, "% CI ", c("(lower)", "(upper)"))
      row.names(ES_summary) <- NULL
      names(ES_summary) <- c("BC-SMD estimate","Standard deviation", CI_names,
                             "Degrees of freedom","Auto-correlation", "Variance parameter", "Intra-class correlation",
                             "Initial treatment time","Follow-up time")
      
      return(ES_summary)
      
    } else {
      
      res <- c(list(model = m_fit, g = g, SD = SD_g, df = df, 
                    phi = autocor_param, var_param = var_param, rho = rho))
      
      return(res)
      
    }
    
  } else {
    
    # r_const
    
    r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
    r_const_trt <- rep(0L, r_const_dim - length(r_const_base))
    
    if (design %in% c("RMBB", "CMB")) {
      r_const_base2 <- bc_mat2[upper.tri(bc_mat2, diag = TRUE)]
      r_const_trt2 <- rep(0L, r_const_dim2 - length(r_const_base2))
    } 
    
    # determine correlation structure and variance structure
    
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
    
    
    # model fitting
    
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
    converged <- if (is.null(E)) W else E
    
    # calculate effect size
    
    if (design %in% c("MBP", "TR")) {
      r_const <- c(r_const_base,
                   r_const_trt,
                   rep(0L, length(m_fit$modelStruct$corStruct)),
                   rep(0L, length(m_fit$modelStruct$varStruct)),
                   1L)
    } else {
      r_const <- c(r_const_base2,
                   r_const_trt2,
                   r_const_base,
                   r_const_trt,
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
        rho_level3 <- round(with(g_RML, theta$Tau[[1]][1] /
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
      
      if (design %in% c("MBP", "RMBB", "CMB")) {
        ES_summary$A <- A
        ES_summary$B <- B
      } else {
        ES_summary$A <- NA
        ES_summary$B <- NA
      }
      
      ES_summary$converged <- if (isTRUE(converged)) "Yes" else "No"
      
      CI_names <- paste0(cover, "% CI ", c("(lower)", "(upper)"))
      row.names(ES_summary) <- NULL
      names(ES_summary) <- c("BC-SMD estimate","Std. Error", CI_names,
                             "Degrees of freedom","Auto-correlation", "Variance parameter", "Intra-class correlation",
                             "Initial treatment time","Follow-up time","Converged")
      
      return(ES_summary)
      
    } else {
      g_RML$model <- m_fit
      class(g_RML) <- c("enhanced_g_mlm",class(g_RML))
      return(g_RML)
      
    }
    
  }
  
}


#' @title A convenience function for calculating multiple design-comparable
#'   effect sizes from a dataset that compiles data from multiple single-case
#'   design studies
#'
#' @description Calculates standardized mean difference effect sizes for a data
#'   set including one or multiple single-case design studies using the same
#'   design (treatment reversal, multiple baseline/probe across participants,
#'   replicated multiple baseline across behaviors, or clustered multiple
#'   baseline across participants).
#'
#' @param data A data frame containing SCD data for which design-comparable
#'   effect sizes will be calculated.
#' @param grouping A variable name or list of (unquoted) variable names that
#'   uniquely identify each study.
#' @param case A variable name (unquoted) that identifies unique cases within
#'   each \code{grouping} variable.
#' @param phase A variable name (unquoted) that identifies unique treatment
#'   phases.
#' @param session A variable name (unquoted) that contains the measurement times
#'   for each data series.
#' @param outcome A variable name (unquoted) that contains the outcome
#'   measurements for each data series.
#' @param cluster (Optional) variable name (unquoted) that identifies the unique
#'   clusters of cases for \code{CMB} designs.
#' @param series (Optional) variable name (unquoted) that identifies the unique
#'   data series for \code{RMBB} designs.
#'
#' @inheritParams calc_BCSMD
#'
#' @export
#'
#' @return A data frame containing the design-comparable effect size estimate,
#'   standard error, confidence interval, and other information, for each unique
#'   category of \code{grouping} variable(s).
#'
#'
#' @importFrom rlang !!!
#' @importFrom rlang !!
#' @importFrom dplyr .data
#' @importFrom magrittr %>%
#'
#' @examples
#' data(Thiemann2001)
#' data(Thiemann2004)
#' datThiemann <- rbind(Thiemann2001, Thiemann2004)
#'
#' # Change-in-levels model with a fixed treatment effect
#' batch_calc_BCSMD(data = datThiemann,
#'                  grouping = Study_ID,
#'                  design = "RMBB",
#'                  case = case, series = series, phase = treatment,
#'                  session = time, outcome = outcome,
#'                  FE_base = 0, RE_base = 0,
#'                  RE_base_2 = 0, FE_trt = 0)
#'
#' # Models with linear time trends in baseline and treatment phase,
#' # random baseline slope at series level, fixed treatment effects
#' batch_calc_BCSMD(data = datThiemann,
#'                  grouping = Study_ID,
#'                  design = "RMBB",
#'                  case = case, series = series, phase = treatment,
#'                  session = time, outcome = outcome,
#'                  FE_base = c(0,1), RE_base = c(0,1),
#'                  RE_base_2 = 0, FE_trt = c(0,1))
#'
     
batch_calc_BCSMD <- function(data,
                             design, 
                             grouping,
                             case, phase, session, outcome, 
                             cluster = NULL, series = NULL,
                             center = 0, 
                             round_session = TRUE,
                             treatment_name = NULL,
                             FE_base = 0, RE_base = 0, RE_base_2 = NULL, 
                             FE_trt = 0, RE_trt = NULL, RE_trt_2 = NULL,
                             corStruct = "AR1", varStruct = "hom",
                             A = NULL, B = NULL, D = NULL,
                             cover = 95, bound = 35, symmetric = TRUE,
                             ...) {
  
  design <- tryCatch(match.arg(design, c("TR","MBP","RMBB","CMB")),
                     error = function(e) stop("The `design` argument must be a character string specifying 'TR', 'MBP', 'RMBB', or 'CMB'."))
  
  grouping <- rlang::enquos(grouping)
  grouping <- tryCatch(tidyselect::vars_select(names(data), !!!grouping),
                       error = function(e) stop("Grouping variables are not in the dataset."))
  
  case <- tryCatch(tidyselect::vars_pull(names(data), !! rlang::enquo(case)), 
                   error = function(e) stop("The `case` variable is not in the dataset."))
  
  phase <- tryCatch(tidyselect::vars_pull(names(data), !! rlang::enquo(phase)), 
                    error = function(e) stop("The `phase` variable is not in the dataset."))
  
  session <- tryCatch(tidyselect::vars_pull(names(data), !! rlang::enquo(session)), 
                      error = function(e) stop("The `session` variable is not in the dataset."))
  
  outcome <- tryCatch(tidyselect::vars_pull(names(data), !! rlang::enquo(outcome)), 
                      error = function(e) stop("The `outcome` variable is not in the dataset."))
  
  if (tryCatch(!is.null(cluster), error = function(e) TRUE)) {
    cluster <- tryCatch(tidyselect::vars_select(names(data), !!!rlang::enquos(cluster)), 
                        error = function(e) stop("The `cluster` variables are not in the dataset."))
  }
  
  if (tryCatch(!is.null(series), error = function(e) TRUE)) {
    series <- tryCatch(tidyselect::vars_select(names(data), !!!rlang::enquos(series)), 
                       error = function(e) stop("The `series` variables are not in the dataset."))
  }
  
  res <- 
    data %>% 
    dplyr::group_by(!!!rlang::syms(c(grouping))) %>%
    dplyr::summarise(
      calc_BCSMD(
        design = design,
        case = .data[[case]],
        phase = .data[[phase]],
        session = .data[[session]],
        outcome = .data[[outcome]],
        cluster = if (design == "CMB") .data[[cluster]] else NULL,
        series = if (design == "RMBB") .data[[series]] else NULL,
        center = center,
        round_session = round_session,
        treatment_name = treatment_name,
        FE_base = FE_base,
        RE_base = RE_base,
        RE_base_2 = RE_base_2,
        FE_trt = FE_trt,
        RE_trt = RE_trt,
        RE_trt_2 = RE_trt_2,
        corStruct = corStruct,
        varStruct = varStruct,
        A = A,
        B = B,
        D = D,
        cover = cover,
        bound = bound,
        symmetric = symmetric,
        ...
      ),
      .groups = 'drop'
    ) %>% 
    
  
  return(res)
  
}

