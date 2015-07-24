library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)

ui <- navbarPage(title = "scdhlm",
  tabPanel("Effect size estimation",
    tabsetPanel(type = "tabs", 
      tabPanel("Load",
        br(),
        fluidRow(
          column(6,
            radioButtons('dat_type', 'What data do you want to use?', c("Use an example" = "example", "Upload your own data" = "dat")),
            conditionalPanel(
              condition = "input.dat_type == 'example'",
              selectInput("example", label = "Choose an example", choices = c("Anglesea", "Lambert","Laski","Saddler","Schutte"))
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
              uiOutput("variableMapping")
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
  
  dat <- reactive({
    if (input$dat_type == "example") {
      data(list = input$example)
      dat <- get(input$example)
    } else {
      inFile <- input$dat
      dat <- if (is.null(inFile)) NULL else read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, fill=TRUE)
    }
    return(dat)
  })
  
  output$datTable <- renderTable(dat())
  
  output$fileUploaded <- reactive({
    return(!is.null(dat()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$variableMapping <- renderUI({
    var_names <- names(dat())
    list(
    selectInput("caseID", label = "Case identifier", choices = var_names, selected = NULL),
    selectInput("session", label = "Session number", choices = var_names, selected = NULL),
    selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = NULL),
    selectInput("outcome", label = "Outcome", choices = var_names, selected = NULL)
    )
  })
  
}

shinyApp(ui = ui, server = server)
