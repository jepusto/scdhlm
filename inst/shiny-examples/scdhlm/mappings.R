#------------------------------------------------
# Information about pre-loaded examples
#------------------------------------------------

exampleChoices <- c("Anglesea (ABAB design)" = "Anglesea", 
                    "Barton-Arwood (multiple baseline design)" = "BartonArwood",
                    "Lambert (ABAB design)" = "Lambert",
                    "Laski (multiple baseline design)" = "Laski",
                    "Rodriguez (multiple baseline design)" = "Rodriguez",
                    "Saddler (multiple probe design)" = "Saddler",
                    "Schutte (multiple baseline design)" = "Schutte",
                    "Thorne (ABAB design)" = "Thorne")

exampleMapping <- list(
  Anglesea = list(design = "TR",
                  vars = c("case","session","condition","outcome"),
                  phases = c("baseline","treatment")),
  BartonArwood = list(design = "MB",
                      vars = c("case","session","condition","outcome"),
                      phases = c("A","B")),
  Lambert = list(design = "TR",
                 vars = c("case","time","treatment","outcome"),
                 phases = c("SSR","RC")),
  Laski = list(design = "MB",
               vars = c("case","time","treatment","outcome"),
               phases = c(0,1)),
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
                phase = c(0,1),
                filters = "measure",
                filter_measure = c("Academic Engagement","Inappropriate Verbalizations"))
)


#------------------------------------------------
# Polynomial degree names
#------------------------------------------------

degree_names <- c("level","linear","quadratic","cubic","quartic","quintic","sextic")

time_trends <- 0:6
names(time_trends) <- degree_names

#------------------------------------------------
# Design names
#------------------------------------------------

design_names <- c("Treatment Reversal" = "TR", "Multiple Baseline/Multiple Probe" = "MB")

#------------------------------------------------
# Estimation names
#------------------------------------------------

estimation_names <- c("Moment estimation" = "HPS", "Restricted Maximum Likelihood" = "RML")
