#------------------------------------------------
# Information about pre-loaded examples
#------------------------------------------------

exampleMapping <- list(
  Anglesea = list(design = "TR",
                  vars = c("case","session","condition","outcome"),
                  phases = c("baseline","treatment")),
  Lambert = list(design = "TR",
                 vars = c("case","time","treatment","outcome"),
                 phases = c("SSR","RC")),
  Laski = list(design = "MB",
               vars = c("case","time","treatment","outcome"),
               phases = c(0,1)),
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

#------------------------------------------------
# Design names
#------------------------------------------------

design_names <- c("Treatment Reversal" = "TR", "Multiple Baseline/Multiple Probe" = "MB")

#------------------------------------------------
# Estimation names
#------------------------------------------------

estimation_names <- c("Moment estimation" = "HPS", "Restricted Maximum Likelihood" = "RML")
