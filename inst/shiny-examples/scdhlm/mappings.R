#------------------------------------------------
# Information about pre-loaded examples
#------------------------------------------------

exampleChoices <- c("Alber-Morgan (multiple baseline design)" = "AlberMorgan",
                    "Anglesea (ABAB design)" = "Anglesea", 
                    "Barton-Arwood (multiple baseline design)" = "BartonArwood",
                    "Case-Harris-Graham (multiple baseline design)" = "CaseHarrisGraham",
                    "Datchuk (multiple baseline design)" = "Datchuk",
                    "Delemere-Dounavi (multiple baseline design)" = "DelemereDounavi",
                    "Gunning-Espie (multiple baseline design)" = "GunningEspie",
                    "Lambert (ABAB design)" = "Lambert",
                    "Laski (multiple baseline design)" = "Laski",
                    "Peltier (multiple baseline design)" = "Peltier",
                    "Rodgers (multiple baseline design)" = "Rodgers",
                    "Rodriguez (multiple baseline design)" = "Rodriguez",
                    "Saddler (multiple probe design)" = "Saddler",
                    "Schutte (multiple baseline design)" = "Schutte",
                    "Thorne (ABAB design)" = "Thorne")

exampleMapping <- list(
  AlberMorgan = list(design = "MB",
                      vars = c("case","session","condition","outcome"),
                      phases = c("baseline","treatment")),
  Anglesea = list(design = "TR",
                  vars = c("case","session","condition","outcome"),
                  phases = c("baseline","treatment")),
  BartonArwood = list(design = "MB",
                      vars = c("case","session","condition","outcome"),
                      phases = c("A","B")),
  CaseHarrisGraham = list(design = "MB",
              vars = c("case","session","condition","outcome"),
              phases = c("baseline","treatment")),
  Datchuk = list(design = "MB",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  DelemereDounavi = list(design = "MB",
                         vars = c("case","session","condition","outcome"),
                         phases = c("baseline","treatment")),
  GunningEspie = list(design = "MB",
                      vars = c("case","session","condition","outcome"),
                      phases = c("baseline","treatment", "follow-up")),
  Lambert = list(design = "TR",
                 vars = c("case","time","treatment","outcome"),
                 phases = c("SSR","RC")),
  Laski = list(design = "MB",
               vars = c("case","time","treatment","outcome"),
               phases = c(0,1)),
  Rodgers = list(design = "MB",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  Peltier = list(design = "MB",
                 vars = c("case","session","condition","outcome"),
                 phases = c("baseline","treatment")),
  Rodriguez = list(design = "MB",
                   vars = c("case","session","condition","outcome"),
                   phases = c("A","B")),
  Saddler = list(design = "MB",
                 vars = c("case","time","treatment","outcome"),
                 phases = c(0,1),
                 filters = "measure",
                 filter_measure = c("writing quality", "T-unit length", "number of constructions")),
  Schutte = list(design = "MB",
                 vars = c("case","week","treatment","fatigue"),
                 phases = c("baseline","treatment")),
  Thorne = list(design = "TR",
                vars = c("case","session","condition","outcome"),
                phases = c("A","B"),
                filters = "measure",
                filter_measure = c("Academic Engagement","Inappropriate Verbalizations"))
)


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

design_names <- c("Treatment Reversal" = "TR", "Multiple Baseline/Multiple Probe" = "MB")

#------------------------------------------------
# Estimation names
#------------------------------------------------

estimation_names <- c("Moment estimation" = "HPS", "Restricted Maximum Likelihood" = "RML")
