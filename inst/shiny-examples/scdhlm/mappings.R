#------------------------------------------------
# Information about pre-loaded examples
#------------------------------------------------

exampleChoices <- c("Alber-Morgan (multiple baseline across participants)" = "AlberMorgan",
                    "Anglesea (ABAB design)" = "Anglesea", 
                    "Barton-Arwood (multiple baseline across participants)" = "BartonArwood",
                    "Bryant-et-al. (clustered multiple baseline across participants)" = "Bryant2018",
                    "Case-Harris-Graham (multiple probe across participants)" = "CaseHarrisGraham",
                    "Datchuk (multiple probe across participants)" = "Datchuk",
                    "Delemere-Dounavi (multiple baseline across participants)" = "DelemereDounavi",
                    "Gunning-Espie (multiple baseline across participants)" = "GunningEspie",
                    "Lambert (ABAB design)" = "Lambert",
                    "Laski (multiple baseline across participants)" = "Laski",
                    "Peltier (multiple probe across participants)" = "Peltier",
                    "Rodgers (multiple baseline across participants)" = "Rodgers",
                    "Rodriguez (multiple baseline across participants)" = "Rodriguez",
                    "Saddler (multiple probe across participants)" = "Saddler",
                    "Schutte (multiple baseline across participants)" = "Schutte",
                    "Thiemann-Goldstein (replicated multiple baseline across behaviors)" = "Thiemann2001",
                    "Thorne (ABAB design)" = "Thorne")

exampleMapping <- list(
  AlberMorgan = list(design = "MBP",
                     vars = c("case","session","condition","outcome"),
                     phases = c("baseline","treatment")),
  Anglesea = list(design = "TR",
                  vars = c("case","session","condition","outcome"),
                  phases = c("baseline","treatment")),
  BartonArwood = list(design = "MBP",
                      vars = c("case","session","condition","outcome"),
                      phases = c("A","B")),
  Bryant2018 = list(design = "CMB",
                    vars = c("Study_ID", "school", "group", "case", "treatment", "session", "session_trt", "outcome", "session_c"),
                    phases = c("baseline", "treatment")),
  CaseHarrisGraham = list(design = "MBP",
                          vars = c("case","session","condition","outcome"),
                          phases = c("baseline","treatment")),
  Datchuk = list(design = "MBP",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  DelemereDounavi = list(design = "MBP",
                         vars = c("case","session","condition","outcome"),
                         phases = c("baseline","treatment")),
  GunningEspie = list(design = "MBP",
                      vars = c("case","session","condition","outcome"),
                      phases = c("baseline","treatment", "follow-up")),
  Lambert = list(design = "TR",
                 vars = c("case","time","treatment","outcome"),
                 phases = c("SSR","RC"),
                 filters = "measure",
                 filter_measure = c("academic response", "disruptive behavior")),
  Laski = list(design = "MBP",
               vars = c("case","time","treatment","outcome"),
               phases = c(0,1)),
  Rodgers = list(design = "MBP",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  Peltier = list(design = "MBP",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  Rodriguez = list(design = "MBP",
                   vars = c("case","session","condition","outcome"),
                   phases = c("A","B")),
  Saddler = list(design = "MBP",
                 vars = c("case","time","treatment","outcome"),
                 phases = c(0,1),
                 filters = "measure",
                 filter_measure = c("writing quality", "T-unit length", "number of constructions")),
  Schutte = list(design = "MBP",
                 vars = c("case","week","treatment","fatigue"),
                 phases = c("baseline","treatment")),
  Thiemann2001 = list(design = "RMBB",
                      vars = c("Study_ID","case", "series", "outcome", "time", "treatment", "trt_time", "time_c"),
                      phases = c("baseline", "treatment")),
  Thorne = list(design = "TR",
                vars = c("case","session","condition","outcome"),
                phases = c("A","B"),
                filters = "measure",
                filter_measure = c("Academic Engagement","Inappropriate Verbalizations"))
)


#------------------------------------------------
# Model term labels
#------------------------------------------------

labs_MBP <- list(
  fixed =  "Include fixed effect &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;",
  random = "Include case-level random effect"
)

labs_RMBB <- list(
  fixed         = "Include fixed effect &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;",
  random_series = "Include series-level random effect",
  random_case   = "Include case-level random effect &nbsp; &nbsp; &nbsp; &nbsp;"
)

labs_CMB <- list(
  fixed          = "Include fixed effect &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;",
  random_case    = "Include case-level random effect &nbsp; &nbsp; &nbsp; &nbsp;",
  random_cluster = "Include cluster-level random effect"
)

handle_white_space <- function(x) span(HTML(x), style="white-space: break-spaces;")

labs_MBP <- lapply(labs_MBP, handle_white_space)
labs_RMBB <- lapply(labs_RMBB, handle_white_space)
labs_CMB <- lapply(labs_CMB, handle_white_space)

#------------------------------------------------
# Polynomial degree names
#------------------------------------------------

degree_names_baseline <- c("level", "linear trend", "quadratic trend", "cubic trend","
                           quartic trend", "quintic trend", "sextic trend")
degree_names_treatment <- c("change in level", "change in linear trend", "change in quadratic trend", 
                            "change in cubic trend", "change in quartic trend",
                            "change in quintic trend", "change in sextic trend")

time_trends_baseline <- 0:6
time_trends_treatment <- 0:6
names(time_trends_baseline) <- degree_names_baseline
names(time_trends_treatment) <- degree_names_treatment

#------------------------------------------------
# Design names
#------------------------------------------------

design_names <- c("Treatment Reversal" = "TR", 
                  "Multiple Baseline/Multiple Probe across participants" = "MBP",
                  "Replicated multiple baseline across behaviors" = "RMBB",
                  "Clustered multiple baseline across participants" = "CMB")

#------------------------------------------------
# Estimation names
#------------------------------------------------
# estimation_names <- c("Moment estimation" = "HPS", 
#                       "Restricted Maximum Likelihood" = "RML",
#                       "Bayesian estimation (Markov Chain Monte Carlo)" = "Bayes")

install_rstan <- requireNamespace("rstan", quietly = TRUE) && requireNamespace("StanHeaders", quietly = TRUE)
if (install_rstan && packageVersion("rstan") >= "2.26.22" &&
  packageVersion("StanHeaders") >= "2.26.27") {
  estimation_names <- c("Moment estimation" = "HPS",
                        "Restricted Maximum Likelihood" = "RML",
                        "Bayesian estimation (Markov Chain Monte Carlo)" = "Bayes")
  library(brms)
  
} else {
  estimation_names <- c("Moment estimation" = "HPS",
                        "Restricted Maximum Likelihood" = "RML")
}

