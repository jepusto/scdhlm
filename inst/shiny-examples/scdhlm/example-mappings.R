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
                 phases = c(0,1)),
  Schutte = list(design = "MB",
                 vars = c("case","week","treatment","fatigue"),
                 phases = c("baseline","treatment"))
)
