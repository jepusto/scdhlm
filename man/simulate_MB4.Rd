\name{simulate_MB4}
\alias{simulate_MB4}
\title{Simulate Model MB4 from Pustejovsky (2013)}
\usage{
  simulate_MB4(iterations, beta, rho, phi, tau2_ratio,
    tau_corr, p_const, design, m, n, MB = TRUE)
}
\arguments{
  \item{iterations}{number of independent iterations of the
  simulation}

  \item{beta}{vector of fixed effect parameters}

  \item{rho}{intra-class correlation parameter}

  \item{phi}{autocorrelation parameter}

  \item{tau2_ratio}{ratio of trend variance to intercept
  variance}

  \item{tau_corr}{correlation between case-specific trends
  and intercepts}

  \item{p_const}{vector of constants for calculating
  numerator of effect size}

  \item{design}{design matrix. If not specified, it will be
  calculated based on \code{m}, \code{n}, and \code{MB}.}

  \item{m}{number of cases. Not used if \code{design} is
  specified.}

  \item{n}{number of measurement occasions. Not used if
  \code{design} is specified.}

  \item{MB}{If true, a multiple baseline design will be
  used; otherwise, an AB design will be used. Not used if
  \code{design} is specified.}
}
\value{
  A list with the following components \tabular{ll}{
  \code{means} \tab Expected value of statistics generated
  using REML method \cr \code{var} \tab Variance of
  statistics generated using REML method \cr }
}
\description{
  Simulates data from a linear mixed effects model, then
  calculates REML effect size estimator as described in
  Pustejovsky (2013).
}
\examples{
simulate_MB4(iterations = 10, beta = c(0,1,0,0), rho = 0.8, phi = 0.5, tau2_ratio = 0.5, tau_corr = 0,
             p_const = c(0,1,0,7), design = design_matrix(3, 16, treat_times=c(5,9,13), center = 12))
}
\references{
  Pustejovsky, J.E. (2013). Operationally Comparable Effect
  Sizes for Meta-Analysis of Single-Case Research. Doctoral
  dissertation, Northwestern University.
}

