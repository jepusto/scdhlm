#------------------------------------------------
# Information about pre-loaded examples
#------------------------------------------------

exampleMapping <- list(
  Anglesea = list(design = "TR",
                  vars = c("case","session","treatment","outcome"),
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
                 filter_measure = 1:3),
  Schutte = list(design = "MB",
                 vars = c("case","week","treatment","fatigue"),
                 phases = c("baseline","treatment"))
)

#------------------------------------------------
# Polynomial degree names
#------------------------------------------------


degree_names <- c("level","linear","quadratic","cubic","quartic","quintic","sextic")
