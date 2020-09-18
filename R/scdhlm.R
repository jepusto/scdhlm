
#' @title A shiny interface for the scdhlm package
#'   
#' @description An interactive shiny interface for estimating design-comparable
#' standardized mean difference effect sizes from single-case designs. Estimation methods
#' for multiple baseline and treatment reversal designs are available. 
#' 
#' @param dataset If specified, the app will open with the data loaded. Default is NULL.
#' 
#' @export
#' 

shine_scd <- function(dataset = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The scdhlm app requires the shiny package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The scdhlm app requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("markdown", quietly = TRUE)) {
    stop("The scdhlm app requires the markdown package. Please install it.", call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples/scdhlm", "app.R", package = "scdhlm")
  if (appDir == "") {
    stop("Could not find the application directory. Try re-installing the scdhlm package.", call. = FALSE)
  }
  
  dataset_ext <- if (!is.null(dataset)) {
      if (grepl(".xlsx", dataset)) {
        "xlsx"
      } else if (grepl(".csv", dataset)) {
        "csv" 
      } else if (grepl(".txt", dataset)) {
        "txt"
      } else {
        "other"
      } 
    } else {
      "none"
    }
  
  ui <- server <- NULL
  source(appDir, local = TRUE, chdir = TRUE)
  server_env <- environment(server)
  server_env$dataset <- dataset
  server_env$dataset_ext <- dataset_ext
  ui_env <- environment(ui)
  ui_env$dataset <- dataset
  ui_env$dataset_ext <- dataset_ext
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, display.mode = "normal", launch.browser = TRUE)
}
