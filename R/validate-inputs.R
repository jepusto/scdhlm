validate_design_inputs <- function(
    design, 
    case, phase, session, outcome, cluster, series,
    validate_outcome = TRUE
) {
  
  if (missing(design)) stop("Please specify a study design of 'MB' (multiple baseline), 'TR' (treatment reversal), 'RMBB' (replicated multiple baseline across behaviors), or 'CMB' (clustered multiple baseline).")
  
  design <- match.arg(design, choices = c("MBP","TR","RMBB","CMB"))
  
  if (missing(case)) stop("Please specify a case variable.")
  if (missing(phase)) stop("Please specify a phase variable.")
  if (missing(session)) stop("Please specify a session variable.")
  if (validate_outcome) {
    if (missing(outcome)) stop("Please specify an outcome variable.")  
  }
  if (design == "CMB") {
    if (missing(cluster)) stop("Please specify a cluster variable.")
  } 
  if (design == "RMBB") {
    if (missing(series)) stop("Please specify a series variable.")
  }
  
  return(design)
  
}