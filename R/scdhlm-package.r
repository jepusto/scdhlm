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
#' The package also includes the data used in the examples from each paper, as well as several other datasets: 
#' \itemize{
#' \item \code{\link{AlberMorgan}}
#' \item \code{\link{Anglesea}}
#' \item \code{\link{BartonArwood}}
#' \item \code{\link{Bryant2018}}
#' \item \code{\link{Carson}}
#' \item \code{\link{Lambert}}
#' \item \code{\link{Laski}}
#' \item \code{\link{Musser}}
#' \item \code{\link{Rodriguez}}
#' \item \code{\link{Ruiz}}
#' \item \code{\link{Saddler}}
#' \item \code{\link{Schutte}}
#' \item \code{\link{Thiemann2001}}
#' \item \code{\link{Thiemann2004}}
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
#' @import stats
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
#'   \item \code{case} Case identifier.  
#'   \item \code{condition} Factor indicating baseline or treatment condition
#'   \item \code{phase} Study phase (including both control and treatment condition)
#'   \item \code{session} Measurement occasion
#'   \item \code{outcome} Total seconds of eating time
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
#'   \item \code{case} Student identifier  
#'   \item \code{measure} Factor indicating the outcome measure (writing quality, T-unit length, number of constructions)
#'   \item \code{outcome} Value of outcome measure.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{treatment}. Factor indicating the treatment phase.
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
#'   \item \code{measure}. Outcome measure description (academic engagement or inappropriate verbalizations). 
#'   \item \code{session}. Measurement occasion.
#'   \item \code{phase_id}. Factor describing the phase of the study design for each case.
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

#' Carson (2008)
#' 
#' Data from a BAB design conducted by Carson, Gast, & Ayres (2008). The
#' variables are as follows: 
#' \itemize{ 
#'    \item \code{case} Participant identifier 
#'    \item \code{treatment} Factor describing the treatment condition
#'    \item \code{phase} Numeric describing the phase of the study design for each case 
#'    \item \code{outcome} Outcome scores 
#'    \item \code{time} Measurement occasion 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 47 rows and 5 variables
#' @name Carson
#' @source Carson, K. D., Gast, D. L., & Ayres, K. M. (2008). Effects of a photo
#'   activity schedule book on independent task changes by students with
#'   intellectual disabilities in community and school job sites. \emph{European
#'   Journal of Special Needs Education, 23}, 269-279. 
#'   
NULL

#' Musser (2001)
#' 
#' Data from a multiple baseline design conducted by 
#' Musser, Bray, Kehle, and Jenson (2001). The variables are as follows:
#' \itemize{
#'   \item \code{student} Participant identifier
#'   \item \code{session} Measurement occasion
#'   \item \code{outcome} Percentage of disruptive intervals
#'   \item \code{treatment} Factor indicating baseline, treatment, or follow-up phase
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 136 rows and 4 variables
#' @name Musser
#' @source Musser, E. H., Bray, M. A., Kehle, T. J., & Jenson, W. R. (2001).
#' Reducing disruptive behaviors in students with serious emotional disturbance.
#' School Psychology Review, 30(2), 294-304.
NULL

#' Barton-Arwood, Wehby, & Falk (2005)
#' 
#' Data from a multiple baseline design conducted by Barton-Arwood, Wehby, and
#' Falk (2005). The variables are as follows: \itemize{ \item \code{case}
#' Participant identifier \item \code{condition} Factor identifying
#' the phase of the design (A or B) \item \code{session} Measurement occasion 
#' \item \code{outcome} Oral reading fluency score (words per minute) }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 143 rows and 4 variables
#' @name BartonArwood
#' @source Barton-Arwood, S. M., Wehby, J. H., & Falk, K. B. (2005). Reading
#'   instruction for elementary-age students with emotional and behavioral
#'   disorders: Academic and behavioral outcomes. \emph{Exceptional Children, 72}(1),
#'   7-27. doi:10.1177/001440290507200101
#'   
NULL

#' Rodriguez & Anderson (2014)
#' 
#' Data from a multiple baseline design conducted by Rodriguez and Anderson (2014). 
#' The variables are as follows: \itemize{ 
#'   \item \code{case} Participant identifier 
#'   \item \code{condition} Factor identifying the phase of the design (A or B) 
#'   \item \code{session} Measurement occasion 
#'   \item \code{outcome} Percentage of intervals with problem behavior 
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 148 rows and 4 variables
#' @name Rodriguez
#' @source Rodriguez, B. J., & Anderson, C. M. (2014). Integrating a social
#'   behavior intervention during small group academic instruction using a total
#'   group criterion intervention. \emph{Journal of Positive Behavior Interventions,
#'   16}(4), 234-245. doi:\href{http://dx.doi.org/10.1177/1098300713492858}{10.1177/1098300713492858}
#'   
NULL

#' Romaniuk (2002)
#' 
#' Data from a treatment reversal design conducted by Romaniuk and colleagues (2002).
#' The variables are as follows: \itemize{ 
#'   \item \code{case} Participant identifier 
#'   \item \code{phase} Factor identifying the phase of the design
#'   \item \code{condition} Factor identifying the treatment condition 
#'   \item \code{session} Measurement occasion 
#'   \item \code{outcome} Problem behavior
#'   \item \code{measurement} Character string describing how problem behavior was measured
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 148 rows and 4 variables
#' @name Romaniuk
#' @source Romaniuk, C., Miltenberger, R., Conyers, C., Jenner, N., Jurgens, M.,
#'   & Ringenberg, C. (2002). The influence of activity choice on problem
#'   behaviors maintained by escape versus attention. \emph{Journal of Applied
#'   Behavior Analysis, 35}(4), 349-62. 
#'   doi:\href{http://dx.doi.org/10.1901/jaba.2002.35-349}{10.1901/jaba.2002.35-349}
#'   
#'   
NULL

#' Alber-Morgan, et al. (2007)
#' 
#' Data from a multiple baseline design conducted by Alber-Morgan, Ramp,
#' Anderson, & Martin (2007). The variables are as follows: \itemize{ \item
#' \code{case} Participant identifier \item \code{condition} Factor identifying
#' the phase of the design (baseline or treatment) \item \code{session}
#' Measurement occasion \item \code{outcome} Number of words read correctly per
#' minute }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 119 rows and 4 variables
#' @name AlberMorgan
#' @source Alber-Morgan, S. R., Ramp, E. M., Anderson, L. L., & Martin, C. M.
#'   (2007). Effects of repeated readings, error correction, and performance
#'   feedback on the fluency and comprehension of middle school students with
#'   behavior problems. Journal of Special Education, 41(1), 17-30.
#'   doi:\href{http://dx.doi.org/10.1177/00224669070410010201}{10.1177/00224669070410010201}
#'   
#'   
NULL


#' Salazar, et al. (2020)
#' 
#' Data from a multiple baseline design conducted by Salazar, Ruiz,
#' Ramírez1, &  Cardona-Betancourt (2020). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Participant identifier.
#'   \item \code{measure}. Outcome measure description (AFQ-Y, PTQ-C, or GPQ-C).
#'   \item \code{treatment} Factor indicating baseline, treatment, post, or follow-up phase.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{outcome}. Outcome scores.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 324 rows and 5 variables
#' @name Salazar
#' 
#' @source Salazar, D., Ruiz, F., Ramírez, E., & Cardona-Betancourt, V. (2020).
#'   Acceptance and Commitment Therapy Focused on Repetitive Negative Thinking for
#'   Child Depression: A Randomized Multiple-Baseline Evaluation. The Psychological
#'   Record. doi:\href{https://doi.org/10.1007/s40732-019-00362-5}{10.1007/s40732-019-00362-5}
#'
NULL

#' Bryant et al. (2018)
#'
#' Data from a multiple baseline across clusters design conducted by
#' Bryant et al. (2018). The variables are as follows:
#' \itemize{
#'   \item \code{Study_ID}. Study identifier.
#'   \item \code{school}. School identifier.
#'   \item \code{case}. Student identifier.
#'   \item \code{treatment}. Indicator for treatment phase.
#'   \item \code{session}. Measurement occasion.
#'   \item \code{session_trt}. Measurement occasion times treatment phase.
#'   \item \code{outcome}. Texas Early Mathematics Inventory (TEMI-Aim Check) scores.
#'   \item \code{session_c}. Measurement occasion centered at the follow-up time.
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 536 rows and 8 variables
#' @name Bryant2018
#' @source Bryant, D. R., Bryant, B. R., Sorelle-Miner, D. A., Falcomata, T. S.
#' & Nozari, M. (2018).
#' Tier 3 intensified intervention for second grade students with severe
#' mathematics difficulties. \emph{Archives of Psychology, 2}(11), 1-24.
#' doi:\href{https://doi.org/10.31296/aop.v2i11.86}{10.31296/aop.v2i11.86}
#'
NULL

#' Thiemann & Goldstein (2001)
#'
#' Data from a multiple baseline across behaviors design conducted by
#' Thiemann & Goldstein (2001). The variables are as follows:
#' \itemize{
#'   \item \code{Study_ID}. Study identifier.
#'   \item \code{case}. Student identifier.
#'   \item \code{series}. Series identifier.
#'   \item \code{outcome}. Frequency of coded social communication skills, as measured by
#'   a direct observation coding system with 15-second intervals recoding for the occurrence
#'   of any of the four social measures: contingent responses, securing attention, initiating
#'   comments, and initiating requests.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{treatment}. Indicator for treatment phase.
#'   \item \code{trt_time}. Measurement occasion times treatment phase.
#'   \item \code{time_c}. Measurement occasion centered at the follow-up time.
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 221 rows and 8 variables
#' @name Thiemann2001
#' @source Thiemann, K.S., & Goldstein, H. (2001).
#' Social stories, written text cues, and video feedback: effects on social communication
#' of children with Autism. \emph{Journal of Applied Behavior Analysis, 34}(4), 425-446.
#' doi:\href{http://doi.org/10.1901/jaba.2001.34-425}{10.1901/jaba.2001.34-425}
#'
NULL

#' Ruiz, et al. (2020)
#' 
#' Data from a multiple baseline design conducted by  Ruiz,
#' Luciano, Florez, Suarez-Falcon, &  Cardona-Betancourt (2020). The variables are as follows:
#' \itemize{
#'   \item \code{case}. Participant identifier.
#'   \item \code{measure}. Outcome measure description (AAQ-II, ANXIETY, CFQ, DASS-TOTAL, DEPRESSION, PSWQ, PTQ, STRESS, VQ-OBSTRUCTION, or VQ-PROGRESS).
#'   \item \code{treatment} Factor indicating baseline, treatment, post, or follow-up phase.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{outcome}. Outcome scores.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 840 rows and 5 variables
#' @name Ruiz
#' 
#' @source Ruiz, F., Luciano, C., Florez, C., Suarez-Falcon, J., & Cardona-Betancourt, V. (2020).
#'   A Multiple-Baseline Evaluation of Acceptance and Commitment Therapy Focused on Repetitive Negative 
#'   Thinking for Comorbid Generalized Anxiety Disorder and Depression. Frontiers in Psychology, 11. 
#'   doi: \href{https://doi.org/10.3389/fpsyg.2020.00356}{10.3389/fpsyg.2020.00356}
#'
#'
NULL


#' Thiemann & Goldstein (2004)
#'
#' Data from a multiple baseline across behaviors design conducted by
#' Thiemann & Goldstein (2004). The variables are as follows:
#' \itemize{
#'   \item \code{Study_ID}. Study identifier.
#'   \item \code{case}. Student identifier.
#'   \item \code{series}. Series identifier.
#'   \item \code{outcome}. Frequency of unprompted targeted social communication skills,
#'   as measured by a direct observation, paper and pencil coding system during the 10-minute
#'   social activity for each behavior for all sessions.
#'   \item \code{time}. Measurement occasion.
#'   \item \code{treatment}. Indicator for treatment phase.
#'   \item \code{trt_time}. Measurement occasion times treatment phase.
#'   \item \code{time_c}. Measurement occasion centered at the follow-up time.
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 408 rows and 8 variables
#' @name Thiemann2004
#' @source Thiemann, K.S., & Goldstein, H. (2004).
#' Effects of peer training and written text cueing on social communication of
#' school-age children with pervasive developmental disorder. \emph{Journal of Speech Language 
#' and Hearing Research, 47}(1), 126-144. doi:\href{https://doi.org/10.1044/1092-4388(2004/012)}{10.1044/1092-4388(2004/012)}
#'
NULL


#--------------------------
# Simulation results
#--------------------------



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