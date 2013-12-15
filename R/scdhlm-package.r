#' scdhlm
#'
#' Standardize mean difference effect size estimation based on hierarchical models
#' for single-case designs.
#' 
#' \pkg{scdhlm} implements methods for estimating a design-comparable standardized 
#' mean difference effect size based on data from a single-case design. 
#' The following functions are available:
#' \itemize{
#' \item \code{\link{g_REML}} implements the corrected REML estimator for a fitted \code{lme} model
#' \item \code{\link{effect_size_MB}} implements the HPS estimator for the multiple baseline design 
#' (as described in Hedges, Pustejovsky, & Shadish, 2013)
#' \item \code{\link{effect_size_ABk}} implements the HPS estimator for the (AB)^k design 
#' (as described in Hedges, Pustejovsky, & Shadish, 2012)
#' }
#' 
#' The package also includes the data used in the examples from each paper: 
#' \itemize{
#' \item \code{\link{Lambert}}
#' \item \code{\link{Anglesea}}
#' \item \code{\link{Saddler}}
#' \item \code{\link{Laski}}
#' \item \code{\link{Schutte}}
#' }
#' 
#' @author James E. Pustejovsky <jepusto@@gmail.com>
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:10.1002/jrsm.1052
#' 
#' Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods}, forthcoming.
#' 
#' Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
#' 
#' @name scdhlm
#' @docType package
NULL


#' Example 1 from Hedges, Pustejovsky, & Shadish (2012)
#' 
#' Data from an ABAB design conducted by Lambert, Cartledge, Heward, & Lo (2008). 
#' The variables are as follows:
#' \itemize{
#'   \item \code{case}. Student identifier.  
#'   \item \code{treatment}. Factor indicating treatment or control condition. 
#'         SSR = single-subject responding. RC = response cards.
#'   \item \code{phase}. Study phase (including both control and treatment condition)
#'   \item \code{time}. Measurement occasion.
#'   \item \code{outcome}. Intervals with disruptive behavior, as measured by a partial
#'   interval recording procedure with 10 ten-second intervals per session.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 264 rows and 5 variables
#' @name Lambert
#' @source Lambert, M. C., Cartledge, G., Heward, W. L., & Lo, Y. (2006). 
#' Effects of response cards on disruptive behavior and academic responding 
#' during math lessons by fourth-grade urban students. 
#' \emph{Journal of Positive Behavior Interventions, 8}(2), 88-99.
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:10.1002/jrsm.1052
NULL

#' Example 2 from Hedges, Pustejovsky, & Shadish (2012)
#' 
#' Data from an ABAB design conducted by Anglesea, Hoch, & Taylor (2008). 
#' The variables are as follows:
#' \itemize{
#'   \item \code{case}. Student identifier.  
#'   \item \code{treatment}. Factor indicating treatment or control condition. 
#'         SSR = single-subject responding. RC = response cards.
#'   \item \code{phase}. Study phase (including both control and treatment condition)
#'   \item \code{time}. Measurement occasion.
#'   \item \code{outcome}. Total seconds of eating time.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 55 rows and 5 variables
#' @name Anglesea
#' @source Anglesea, M. M., Hoch, H., & Taylor, B. A. (2008). 
#' Reducing rapid eating in teenagers with autism: Use of a pager prompt. 
#' \emph{Journal of Applied Behavior Analysis, 41}(1), 107-111. 
#' doi:10.1901/jaba.2008.41-107
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:10.1002/jrsm.1052
NULL

#' Example 1 from Hedges, Pustejovsky, & Shadish (2013)
#' 
#' Data from a multiple baseline design conducted by 
#' Saddler, Behforooz, & Asaro, (2008). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Student identifier.  
#'   \item \code{measure}. Outcome measure identifier. 
#'   1 = writing quality. 
#'   2 = T-unit length. 
#'   3 = number of constructions.
#'   \item \code{outcome}. Outcome measure.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{treatment}. Indicator for treatment phase.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 124 rows and 5 variables
#' @name Saddler
#' @source Saddler, B., Behforooz, B., & Asaro, K. (2008). 
#' The effects of sentence-combining instruction on the writing of 
#' fourth-grade students with writing difficulties. 
#' \emph{The Journal of Special Education, 42}(2), 79-90. 
#' doi:10.1177/0022466907310371
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods}, forthcoming.
NULL


#' Example 2 from Hedges, Pustejovsky, & Shadish (2013)
#' 
#' Data from a multiple baseline design conducted by 
#' Laski, Charlop, & Schreibman (1988). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Child identifier.  
#'   \item \code{outcome}. Frequency of child vocalization, as measured by 
#'   a partial interval recording procedure with 60 ten-second intervals per session.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{treatment}. Indicator for treatment phase.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 128 rows and 4 variables
#' @name Laski
#' @source Laski, K. E., Charlop, M. H., & Schreibman, L. (1988). 
#' Training parents to use the natural language paradigm to increase 
#' their autistic children's speech. 
#' \emph{Journal of Applied Behavior Analysis, 21}(4), 391-400.
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods}, forthcoming.
NULL


#' Example from Pustejovsky, Hedges, & Shadish (2013)
#' 
#' Data from a multiple baseline design conducted by 
#' Schutte, Malouff, & Brown (2008). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Participant identifier.  
#'   \item \code{week}. Measurement occasion.
#'   \item \code{treatment}. Factor indicating baseline or treatment phase.
#'   \item \code{fatigue}. Fatigue severity scale scores.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 136 rows and 4 variables
#' @name Schutte
#' @source Schutte, N. S., Malouff, J. M., & Brown, R. F. (2008). 
#' Efficacy of an emotion-focused treatment for prolonged fatigue. 
#' \emph{Behavior Modification, 32}(5), 699-713. doi: 10.1177/0145445508317133
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2013). 
#' Design-comparable effect sizes in multiple baseline designs: A general approach
#' to modeling and estimation.
NULL