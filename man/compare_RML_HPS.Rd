\name{compare_RML_HPS}
\alias{compare_RML_HPS}
\title{Run simulation comparing REML and HPS estimates}
\usage{
compare_RML_HPS(iterations, beta, rho, phi, design, m, n, MB = TRUE)
}
\arguments{
  \item{iterations}{number of independent iterations of the
  simulation}

  \item{beta}{vector of fixed effect parameters}

  \item{rho}{intra-class correlation parameter}

  \item{phi}{autocorrelation parameter}

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
Simulates data from a simple linear mixed effects model,
then calculates REML and HPS effect size estimators as
described in Pustejovsky, Hedges, & Shadish (2013).
}
\examples{
compare_RML_HPS(iterations=10, beta = c(0,1,0,0), rho = 0.3, phi = 0.5, design=design_matrix(m=3,n=8))
}
\references{
Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013).
Design-comparable effect sizes in multiple baseline
designs: A general approach to modeling and estimation.
}

