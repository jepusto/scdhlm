\docType{package}
\name{scdhlm}
\alias{scdhlm}
\alias{scdhlm-package}
\title{scdhlm}
\description{
  Standardize mean difference effect size estimation based
  on hierarchical models for single-case designs.
}
\details{
  \pkg{scdhlm} implements methods for estimating a
  design-comparable standardized mean difference effect
  size based on data from a single-case design. The
  following functions are available: \itemize{ \item
  \code{\link{g_REML}} implements the corrected REML
  estimator for a fitted \code{lme} model \item
  \code{\link{effect_size_MB}} implements the HPS estimator
  for the multiple baseline design (as described in Hedges,
  Pustejovsky, & Shadish, 2013) \item
  \code{\link{effect_size_ABk}} implements the HPS
  estimator for the (AB)^k design (as described in Hedges,
  Pustejovsky, & Shadish, 2012) }

  The package also includes the data used in the examples
  from each paper: \itemize{ \item \code{\link{Lambert}}
  \item \code{\link{Anglesea}} \item \code{\link{Saddler}}
  \item \code{\link{Laski}} \item \code{\link{Schutte}} }
}
\author{
  James E. Pustejovsky <jepusto@gmail.com>
}
\references{
  Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R.
  (2012). A standardized mean difference effect size for
  single case designs. \emph{Research Synthesis Methods,
  3}, 224-239. doi:10.1002/jrsm.1052

  Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R.
  (2013). A standardized mean difference effect size for
  multiple baseline designs across individuals.
  \emph{Research Synthesis Methods}, forthcoming.

  Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R.
  (2013). Design-comparable effect sizes in multiple
  baseline designs: A general approach to modeling and
  estimation.
}

