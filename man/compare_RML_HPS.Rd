\name{compare_RML_HPS}
\alias{compare_RML_HPS}
\title{Run simulation comparing REML and HPS estimates}
\usage{
  compare_RML_HPS(iterations, design, beta, rho, phi)
}
\arguments{
  \item{iterations}{number of independent iterations of the
  simulation}

  \item{design}{design matrix}

  \item{beta}{vector of fixed effect parameters}

  \item{rho}{intra-class correlation parameter}

  \item{phi}{autocorrelation parameter}
}
\value{
  A list with the following components \tabular{ll}{
  \code{means} \tab Expected value of statistics generated
  using REML and HPS methods \cr \code{var} \tab Variance
  of statistics generated using REML and HPS methods \cr
  \code{cov} \tab Covariance between REML and HPS effect
  size estimates \cr }
}
\description{
  Simulates data from a simple linear mixed effects model,
  then calculates REML and HPS effect size estimators as
  described in Pustejovsky, Hedges, & Shadish (2013).
}
\references{
  Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R.
  (2013). Design-comparable effect sizes in multiple
  baseline designs: A general approach to modeling and
  estimation.
}

