#' @importFrom lmeInfo extract_varcomp
#' @importFrom lmeInfo varcomp_vcov
#' @importFrom lmeInfo g_mlm
#' @importFrom lmeInfo CI_g

#' @title lmeInfo
#' @description 
#' Functions imported from the lmeInfo package.
#' 
#' \itemize{
#'   \item \code{\link[lmeInfo]{extract_varcomp}}
#'   \item \code{\link[lmeInfo]{g_mlm}}
#'   \item \code{\link[lmeInfo]{varcomp_vcov}}
#' }
#' 
#' @name lmeInfo
#' @aliases extract_varcomp varcomp_vcov g_mlm
#' @export extract_varcomp g_mlm varcomp_vcov
#' @md
#' 
NULL


#' @title Calculates a confidence interval for a standardized mean difference
#'   effect size
#'
#' @description Calculates a confidence interval given a \code{g_REML}, a
#'   \code{g_HPS}, or a \code{g_mlm} object using either a central t
#'   distribution (for a symmetric interval) or a non-central t distribution
#'   (for an asymmetric interval).
#'
#' @param g an estimated effect size object of class \code{g_REML}, class
#'   \code{g_HPS}, or class \code{g_mlm}.
#' @param bound numerical tolerance for non-centrality parameter in
#'   \code{\link[stats]{qt}}.
#' @param symmetric If \code{TRUE} (the default), use a symmetric confidence
#'   interval. If \code{FALSE}, use a non-central t approximation to obtain an
#'   asymmetric confidence interval.
#'   
#' @name CI_g
#' @export CI_g
#'
#' @return A vector of upper and lower confidence bounds.
#'
#' @examples
#' data(Laski)
#' Laski_RML <- lme(fixed = outcome ~ treatment,
#'                  random = ~ 1 | case,
#'                  correlation = corAR1(0, ~ time | case),
#'                  data = Laski)
#' Laski_g_REML <- g_REML(Laski_RML, p_const = c(0,1), r_const = c(1,0,1), returnModel = FALSE)
#' CI_g(Laski_g_REML, symmetric = TRUE)
#' CI_g(Laski_g_REML, symmetric = FALSE)
#'
#' Laski_HPS <- with(Laski, effect_size_MB(outcome, treatment, case, time))
#' CI_g(Laski_HPS, symmetric = FALSE)
#'
#' Laski_g_mlm <- g_mlm(Laski_RML, p_const = c(0,1), r_const = c(1,0,1), returnModel = TRUE)
#' CI_g(Laski_g_mlm, symmetric = FALSE)
#' 
NULL