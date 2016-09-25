
#' @title A shiny interface for the scdhlm package
#'   
#' @description An interactive shiny interface for estimating design-comparable
#' standardized mean difference effect sizes from single-case designs. Estimation methods
#' for multiple baseline and treatment reversal designs are available. 
#' 
#' @export
#' 

shine_scd <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The scdhlm app requires the shiny package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The scdhlm app requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("markdown", quietly = TRUE)) {
    stop("The scdhlm app requires the markdown package. Please install it.", call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples", "scdhlm", package = "scdhlm")
  if (appDir == "") {
    stop("Could not find the application directory. Try re-installing the scdhlm package.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}