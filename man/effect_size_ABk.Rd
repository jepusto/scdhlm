\name{effect_size_ABk}
\alias{effect_size_ABk}
\title{Calculates HPS effect size}
\usage{
  effect_size_ABk(outcome, treatment, id, time)
}
\arguments{
  \item{outcome}{Vector of outcome data. May not contain
  any missing values.}

  \item{treatment}{Vector of treatment indicators. Must be
  the same length as \code{outcome}.}

  \item{id}{factor vector indicating unique cases. Must be
  the same length as \code{outcome}.}

  \item{time}{vector of measurement occasion times. Must be
  the same length as \code{outcome}.}
}
\value{
  A list with the following components \tabular{ll}{
  \code{g_dotdot} \tab total number of non-missing
  observations \cr \code{K} \tab number of
  time-by-treatment groups containing at least one
  observation \cr \code{D_bar} \tab numerator of effect
  size estimate \cr \code{S_sq} \tab sample variance,
  pooled across time points and treatment groups \cr
  \code{delta_hat_unadj} \tab unadjusted effect size
  estimate \cr \code{phi_hat} \tab corrected estimate of
  first-order auto-correlation \cr \code{sigma_sq_w} \tab
  corrected estimate of within-case variance \cr
  \code{rho_hat} \tab estimated intra-class correlation \cr
  \code{theta} \tab estimated scalar constant \cr \code{nu}
  \tab estimated degrees of freedom \cr \code{delta_hat}
  \tab corrected effect size estimate \cr
  \code{V_delta_hat} \tab estimated variance of
  \code{delta_hat} }
}
\description{
  Calculates the HPS effect size estimator based on data
  from an (AB)^k design, as described in Hedges,
  Pustejovsky, & Shadish (2012). Note that the data must
  contain one row per measurement occasion per subject.
}
\examples{
data(Saddler)
}
\references{
  Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R.
  (2012).  A standardized mean difference effect size for
  single case designs.  Research Synthesis Methods, 3,
  224-239. doi:10.1002/jrsm.1052
}

