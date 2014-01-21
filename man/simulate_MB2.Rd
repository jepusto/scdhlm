\name{simulate_MB2}
\alias{simulate_MB2}
\title{Simulate Model MB2 from Pustejovsky (2013)}
\usage{
simulate_MB2(iterations, beta, rho, phi, tau1_ratio, tau_corr, design, m, n,
  MB = TRUE)
}
\arguments{
  \item{iterations}{number of independent iterations of the
  simulation}

  \item{beta}{vector of fixed effect parameters}

  \item{rho}{intra-class correlation parameter}

  \item{phi}{autocorrelation parameter}

  \item{tau1_ratio}{ratio of treatment effect variance to
  intercept variance}

  \item{tau_corr}{correlation between case-specific
  treatment effects and intercepts}

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
A matrix reporting the mean and variance of the effect size
estimates and various associated statistics.
}
\description{
Simulates data from a linear mixed effects model, then
calculates REML effect size estimator as described in
Pustejovsky (2013).
}
\examples{
set.seed(8)
simulate_MB2(iterations = 10, beta = c(0,1,0,0), rho = 0.4, phi = 0.5, tau1_ratio = 0.5, tau_corr = -0.4, design = design_matrix(m=3, n=8))
set.seed(8)
simulate_MB2(iterations = 10, beta = c(0,1,0,0), rho = 0.4, phi = 0.5, tau1_ratio = 0.5, tau_corr = -0.4, m = 3, n = 8, MB = FALSE)
}
\references{
Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013).
Design-comparable effect sizes in multiple baseline
designs: A general approach to modeling and estimation.
}

