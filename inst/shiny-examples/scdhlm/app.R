library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)

exampleMapping <- list(
  Anglesea = list(vars = c("case","session","treatment","outcome"),
                  phases = ),
  Lambert = list(vars = c("case","time","treatment","outcome"),
                 phases = ),
  Laski = list(vars = c("case","time","treatment","outcome"),
               phases = ),
  Saddler = list(vars = c("case","time","treatment","outcome"),
                 phases = ),
  Schutte = list(vars = c("case","week","treatment","fatigue"),
                 phases = )
)

ui <- navbarPage(title = "scdhlm",
  tabPanel("Effect size estimation",
    tabsetPanel(type = "tabs", 
      tabPanel("Load",
        br(),
        fluidRow(
          column(6,
            radioButtons('dat_type', 'What data do you want to use?', 
                         c("Use an example" = "example", "Upload data from a file" = "dat")),
            conditionalPanel(
              condition = "input.dat_type == 'example'",
              selectInput("example", label = "Choose an example", 
                          choices = c("Anglesea (ABAB design)" = "Anglesea", 
                                      "Lambert (ABAB design)" = "Lambert",
                                      "Laski (multiple baseline design)" = "Laski",
                                      "Saddler (multiple baseline design)" = "Saddler",
                                      "Schutte (multiple baseline design)" = "Schutte"))
            ),
            conditionalPanel(
              condition = "input.dat_type == 'dat'",
              fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
              checkboxInput('header','File contain a header.', TRUE),
              radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' '), ','),
              radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"), '')
            )
          ),
          column(6,
            conditionalPanel(
              condition = "output.fileUploaded & input.dat_type == 'dat'",
              strong("Please select the variable containing each type of information."),
              column(12, br()),
              uiOutput("variableMapping"),
              uiOutput("phaseMapping")
            )
          )
        )
      ),
      tabPanel("Inspect", 
        tableOutput("datTable")
      ), 
      tabPanel("Model", strong("Build a model.")), 
      tabPanel("Effect size", strong("Calculate an effect size."))
    )
  ),
  tabPanel("Help",
    navlistPanel(widths = c(3,9),
      tabPanel("Overview", includeMarkdown("markdown/Overview.md"))
    )
  ),
  tabPanel("About",
           navlistPanel(widths = c(3,9),
                        tabPanel("scdhlm", includeMarkdown("markdown/scdhlm.md")),
                        tabPanel("Accessing scdhlm", includeMarkdown("markdown/Accessing_scdhlm.md")),
                        tabPanel("References", includeMarkdown("markdown/references.md"))
           )
  )
)

server <- function(input, output) {
  
  datFile <- reactive({
    inFile <- input$dat
    if (is.null(inFile)) {
      return(NULL)
    } else {
      return(read.table(inFile$datapath, header=input$header, 
                        sep=input$sep, quote=input$quote, fill=TRUE))
    }
  })
  
  
  output$fileUploaded <- reactive({
    return(!is.null(datFile()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$variableMapping <- renderUI({
    var_names <- names(datFile())
    list(
      selectInput("caseID", label = "Case identifier", choices = var_names, selected = NULL),
      selectInput("session", label = "Session number", choices = var_names, selected = NULL),
      selectInput("outcome", label = "Outcome", choices = var_names, selected = NULL),
      selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = NULL)
    )
  })
  
  output$phaseMapping <- renderUI({
    phases <- levels(as.factor(datFile()[,input$phaseID]))
    list(
      selectInput("baseline", label = "Baseline level", choices = phases, selected = NULL),
      selectInput("treatment", label = "Treatment level", choices = phases, selected = NULL)
    )
  })
  
  datClean <- reactive({
    if (input$dat_type == "example") {
      data(list = input$example)
      dat <- get(input$example)
      dat <- dat[,exampleMapping[[input$example]]$vars]
    } else {
      caseID <- as.factor(datFile()[,input$caseID])
      session <- as.numeric(datFile()[,input$session])
      phaseID <- as.factor(datFile()[,input$phaseID])
      outcome <- as.numeric(datFile()[,input$outcome])
      dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
    }
    return(dat)
  })
  
  output$datTable <- renderTable(datClean())
  
}

shinyApp(ui = ui, server = server)
