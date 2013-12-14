\name{g_REML}
\alias{g_REML}
\title{Calculates adjusted REML effect size}
\usage{
  g_REML(m_fit, p_const, r_const,
    X_design = model.matrix(m_fit, data = m_fit$data),
    Z_design = model.matrix(m_fit$modelStruct$reStruct, data = m_fit$data),
    block = nlme::getGroups(m_fit),
    times = attr(m_fit$modelStruct$corStruct, "covariate"))
}
\arguments{
  \item{m_fit}{Fitted model of class lme, with AR(1)
  correlation structure at level 1.}

  \item{p_const}{Vector of constants for calculating
  numerator of effect size. Must be the same length as
  fixed effects in \code{m_fit}.}

  \item{r_const}{Vector of constants for calculating
  denominator of effect size. Must be the same length as
  the number of variance component parameters in
  \code{m_fit}.}

  \item{X_design}{(Optional) Design matrix for fixed
  effects. Will be extracted from \code{m_fit} if not
  specified.}

  \item{Z_design}{(Optional) Design matrix for random
  effects. Will be extracted from \code{m_fit} if not
  specified.}

  \item{block}{(Optional) Factor variable describing the
  blocking structure. Will be extracted from \code{m_fit}
  if not specified.}

  \item{times}{(Optional) list of times used to describe
  AR(1) structure. Will be extracted from \code{m_fit} if
  not specified.}
}
\value{
  A list with the following components \tabular{ll}{
  \code{p_beta} \tab Numerator of effect size \cr
  \code{r_theta} \tab Squared denominator of effect size
  \cr \code{delta_AB} \tab Unadjusted (REML) effect size
  estimate \cr \code{nu} \tab Estimated denominator degrees
  of freedom \cr \code{g_AB} \tab Corrected effect size
  estimate \cr \code{V_g_AB} \tab Approximate variance
  estimate \cr \code{cnvg_warn} \tab Indicator that model
  did not converge \cr \code{sigma_sq} \tab Estimated
  level-1 variance \cr \code{phi} \tab Estimated
  autocorrelation \cr \code{Tau} \tab Vector of level-2
  variance components \cr \code{I_E_inv} \tab Expected
  information matrix \cr }
}
\description{
  Estimates a design-comparable standardized mean
  difference effect size based on data from a multiple
  baseline design, using adjusted REML method as described
  in Pustejovsky, Hedges, & Shadish (2013). Note that the
  data must contain one row per measurement occasion per
  case.
}
\references{
  Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R.
  (2013). Design-comparable effect sizes in multiple
  baseline designs: A general approach to modeling and
  estimation.
}

