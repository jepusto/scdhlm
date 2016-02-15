#' scdhlm
#'
#' Standardize mean difference effect size estimation based on hierarchical models
#' for single-case designs.
#' 
#' \pkg{scdhlm} implements methods for estimating a design-comparable standardized 
#' mean difference effect size based on data from a single-case design. 
#' The following functions are available:
#' \itemize{
#' \item \code{\link{g_REML}} implements the corrected REML estimator for a fitted \code{lme} model,
#' as described in Pustejovsky, Hedges, and Shadish (2014).
#' \item \code{\link{effect_size_MB}} implements the HPS estimator for the multiple baseline design,
#' as described in Hedges, Pustejovsky, and Shadish (2013).
#' \item \code{\link{effect_size_ABk}} implements the HPS estimator for the (AB)^k design,
#' as described in Hedges, Pustejovsky, and Shadish (2012).
#' }
#' 
#' The package also includes the data used in the examples from each paper, as well as a few other datasets: 
#' \itemize{
#' \item \code{\link{Lambert}}
#' \item \code{\link{Anglesea}}
#' \item \code{\link{Saddler}}
#' \item \code{\link{Laski}}
#' \item \code{\link{Schutte}}
#' \item \code{\link{Thorne}}
#' }
#' 
#' @author James E. Pustejovsky <jepusto@@gmail.com>
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:\href{http://doi.org/10.1002/jrsm.1052}{10.1002/jrsm.1052}
#' 
#' Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods, 4}(4), 324-341. doi:\href{http://doi.org/10.1002/jrsm.1086}{10.1002/jrsm.1086}
#' 
#' Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). 
#' Design-comparable effect sizes in multiple baseline designs: A general modeling framework.
#' \emph{Journal of Educational and Behavioral Statistics, 39}(4), 211-227. doi:\href{http://doi.org/10.3102/1076998614547577}{10.3102/1076998614547577}
#' 
#' @name scdhlm
#' @docType package
#' @importFrom stats simulate
#' @import nlme
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
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:\href{http://doi.org/10.1002/jrsm.1052}{10.1002/jrsm.1052}
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
#' doi:\href{http://doi.org/10.1901/jaba.2008.41-107}{10.1901/jaba.2008.41-107}
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2012).
#' A standardized mean difference effect size for single case designs. 
#' \emph{Research Synthesis Methods, 3}, 224-239. doi:\href{http://doi.org/10.1002/jrsm.1052}{10.1002/jrsm.1052}
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
#' doi:\href{http://doi.org/10.1177/0022466907310371}{10.1177/0022466907310371}
#' 
#' @references Hedges, L. V., Pustejovsky, J. E., & Shadish, W. R. (2013). 
#' A standardized mean difference effect size for multiple baseline designs across individuals. 
#' \emph{Research Synthesis Methods, 4}(4), 324-341. doi:\href{http://doi.org/10.1002/jrsm.1086}{10.1002/jrsm.1086}

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
#' \emph{Research Synthesis Methods, 4}(4), 324-341. doi:\href{http://doi.org/10.1002/jrsm.1086}{10.1002/jrsm.1086}

NULL


#' Example from Pustejovsky, Hedges, & Shadish (2014)
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
#' \emph{Behavior Modification, 32}(5), 699-713. doi:\href{http://doi.org/10.1177/0145445508317133}{10.1177/0145445508317133}
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). 
#' Design-comparable effect sizes in multiple baseline designs: A general modeling framework.
#' \emph{Journal of Educational and Behavioral Statistics, 39}(4), 211-227. doi:\href{http://doi.org/10.3102/1076998614547577}{10.3102/1076998614547577}
NULL

#' Thorne (2005)
#' 
#' Data from an ABAB design conducted by Thorne and Kamps (2008). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Participant identifier.  
#'   \item \code{outcome_desc}. Outcome measure description (academic engagement or inappropriate verbalizations). 
#'   \item \code{session}. Measurement occasion.
#'   \item \code{phase_id}. Factor descibing the phase of the study design for each case.
#'   \item \code{phase_indicator}. Indicator variable equal to 1 during intervention phases.
#'   \item \code{outcome}. Outcome scores
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 776 rows and 6 variables
#' @name Thorne
#' @source Thorne, S., & Kamps, D. (2008). The effects of a group contingency intervention on academic engagement and problem 
#' behavior of at-risk students. \emph{Behavior Analysis in Practice, 1}(2), 12-18.
NULL

#' MB1 simulation results
#' 
#' Simulation results for model MB1 from  Pustejovsky, Hedges, & Shadish (2014).
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB1results
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). 
#' Design-comparable effect sizes in multiple baseline designs: A general modeling framework.
#' \emph{Journal of Educational and Behavioral Statistics, 39}(4), 211-227. doi:\href{http://doi.org/10.3102/1076998614547577}{10.3102/1076998614547577}
NULL

#' MB2 simulation results
#' 
#' Simulation results for model MB2 from  Pustejovsky, Hedges, & Shadish (2014).
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB2results
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). 
#' Design-comparable effect sizes in multiple baseline designs: A general modeling framework.
#' \emph{Journal of Educational and Behavioral Statistics, 39}(4), 211-227. doi:\href{http://doi.org/10.3102/1076998614547577}{10.3102/1076998614547577}
NULL

#' MB4 simulation results
#' 
#' Simulation results for model MB4 from  Pustejovsky, Hedges, & Shadish (2014).
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB4results
#' 
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). 
#' Design-comparable effect sizes in multiple baseline designs: A general modeling framework.
#' \emph{Journal of Educational and Behavioral Statistics, 39}(4), 211-227. doi:\href{http://doi.org/10.3102/1076998614547577}{10.3102/1076998614547577}
NULL

#' MB1 simulation time
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB1time
NULL

#' MB2 simulation time
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB2time
NULL

#' MB4 simulation time
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame
#' @name MB4time
NULL