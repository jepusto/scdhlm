
#' @title A shiny interface for the scdhlm package
#'
#' @description An interactive shiny interface for estimating design-comparable
#'   standardized mean difference effect sizes from single-case designs.
#'   Estimation methods for multiple baseline and treatment reversal designs are
#'   available.
#'
#' @param dataset Optionally, a data.frame or path to a file from which to read
#'   data. If specified, the app will open with the data loaded. Default is
#'   NULL. If \code{dataset} is a data.frame, then it will be passed directly. If
#'   a file path with a \code{.xlsx} extension is specified, it will be read using \code{read_excel}.
#'   If a file path with a \code{.csv} extension is specified, it will be read using \code{read.csv}.
#'   If a file path with a different extension is specified, it will be read using \code{read.table}.
#' @param ... Further arguments passed to \code{read_excel}, \code{read.csv}, or \code{read.table}.
#'
#' @examples 
#' \dontrun{
#' shine_scd()
#' data(Laski)
#' shine_scd(dataset = Laski)
#' shine_scd(dataset = "SCD_data.xlsx", sheet = "Laski")
#' shine_scd(dataset = "Laski.csv") 
#' }
#' 
#' @export
#' 
#' @importFrom readxl read_excel
#' 

shine_scd <- function(dataset = NULL, ...) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The scdhlm app requires the shiny package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The scdhlm app requires the ggplot2 package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("markdown", quietly = TRUE)) {
    stop("The scdhlm app requires the markdown package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("The scdhlm app requires the glue package. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The scdhlm app requires the readxl package. Please install it.", call. = FALSE)
  }
  
  uiDir <- system.file("shiny-examples/scdhlm", "ui.R", package = "scdhlm")
  serveDir <- system.file("shiny-examples/scdhlm", "server.R", package = "scdhlm")
  if (uiDir == "" | serveDir == "") {
    stop("Could not find the application directory. Try re-installing the scdhlm package.", call. = FALSE)
  }
  
  ui <- server <- NULL
  source(uiDir, local = TRUE, chdir = TRUE)
  source(serveDir, local = TRUE, chdir = TRUE)
  
  if (!is.null(dataset)) {
    if (!inherits(dataset, "data.frame")) {
      if (grepl(".xlsx$", dataset)) {
        dataset <- as.data.frame(readxl::read_xlsx(dataset, ...))
      } else if (grepl(".csv$", dataset)) {
        dataset <- utils::read.csv(dataset, ...)
      } else {
        dataset <- utils::read.table(dataset, ...)
      }
    }
    
    server_env <- environment(server)
    ui_env <- environment(ui)
    server_env$dataset <- dataset
    ui_env$dataset <- dataset
    
  } 
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, display.mode = "normal", launch.browser = TRUE)
}
