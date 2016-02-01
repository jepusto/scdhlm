library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)

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


#------------------------------------------------
# User interface
#------------------------------------------------

ui <- navbarPage(title = "scdhlm",
  tabPanel("Effect size estimation",
    tabsetPanel(type = "tabs",
                
      #--------------------
      # Load the data
      #--------------------
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
      
      #--------------------
      # Show loaded data
      #--------------------
      tabPanel("Inspect", 
        tableOutput("datTable")
      ), 
      
      #--------------------
      # Model building
      #--------------------
      tabPanel("Model", 
        br(),
        fluidRow(
          column(6,
            uiOutput("studyDesign")),
          column(6,
            selectInput("method", label = "Estimation method",
                        choices = c("Moment estimation" = "HPS", "Restricted Maximum Likelihood" = "RML"), 
                        selected = "RML")
            )
        ),
        conditionalPanel(condition = "input.method == 'RML'",
          hr(),
          uiOutput("modelDegree"),
          uiOutput("modelSpec")
        ),
        hr(),
        strong("Build a model.")), 
      
      #------------------------------
      # Effect size estimation
      #------------------------------
      tabPanel("Effect size", strong("Calculate an effect size."))
    )
  ),
  
  #--------------------
  # Help pages
  #--------------------
  tabPanel("Help",
    navlistPanel(widths = c(3,9),
      tabPanel("Overview", includeMarkdown("markdown/Overview.md"))
    )
  ),
  
  #--------------------
  # About pages
  #--------------------
  tabPanel("About",
           navlistPanel(widths = c(3,9),
                        tabPanel("scdhlm", includeMarkdown("markdown/scdhlm.md")),
                        tabPanel("Accessing scdhlm", includeMarkdown("markdown/Accessing_scdhlm.md")),
                        tabPanel("References", includeMarkdown("markdown/references.md"))
           )
  )
)

#------------------------------------------------
# Server
#------------------------------------------------

server <- function(input, output) {
  
  # Read in data
  
  datFile <- reactive({
    inFile <- input$dat
    if (is.null(inFile)) {
      return(NULL)
    } else {
      return(read.table(inFile$datapath, header=input$header, 
                        sep=input$sep, quote=input$quote, fill=TRUE))
    }
  })
  
  
  # Check that file is uploaded
  
  output$fileUploaded <- reactive({
    return(!is.null(datFile()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  # Variable mapping interface
  
  output$variableMapping <- renderUI({
    var_names <- names(datFile())
    list(
      selectInput("caseID", label = "Case identifier", choices = var_names, selected = NULL),
      selectInput("session", label = "Session number", choices = var_names, selected = NULL),
      selectInput("outcome", label = "Outcome", choices = var_names, selected = NULL),
      selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = NULL)
    )
  })
  
  # Treatment label mapping interface
  
  output$phaseMapping <- renderUI({
    phases <- levels(as.factor(datFile()[,input$phaseID]))
    list(
      selectInput("baseline", label = "Baseline level", choices = phases, selected = NULL),
      selectInput("treatment", label = "Treatment level", choices = phases, selected = NULL)
    )
  })
  
  # Study design with defaults 
  
  output$studyDesign <- renderUI({
    design <- if (input$dat_type == "example") exampleMapping[[input$example]]$design else "MB"
    selectInput("design", label = "Study design",
                choices = c("Treatment reversal" = "TR", "Multiple baseline" = "MB"),
                selected = design)
  })
  
  # Clean the data
  
  datClean <- reactive({
    if (input$dat_type == "example") {
      data(list = input$example)
      dat <- get(input$example)
      dat <- dat[,exampleMapping[[input$example]]$vars]
      names(dat) <- c("case","session","phase","outcome")
    } else {
      caseID <- as.factor(datFile()[,input$caseID])
      session <- as.numeric(datFile()[,input$session])
      phaseID <- as.factor(datFile()[,input$phaseID])
      outcome <- as.numeric(datFile()[,input$outcome])
      dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
    }
    return(dat)
  })

  # Model degree
  
  output$modelDegree <- renderUI({
    max_degree <- ifelse(input$design == "MB", 6, 0)
    fluidRow(
      column(6,
        numericInput("degree_base", label = "Baseline time trend degree", min = 0, max = max_degree, step = 1, value = 0)
      ),
      column(6,
        numericInput("degree_trt", label = "Treatment time trend degree", min = 0, max = max_degree, step = 1, value = 0)
      )
    )
  })
  
  # Model specification
  
  output$modelSpec <- renderUI({
    degree_names <- c("level","linear","quadratic","cubic","quartic","quintic","sextic")
    deg_base <- if (is.null(input$degree_base)) 0 else input$degree_base
    degree_base_list <- 0:deg_base
    names(degree_base_list) <- degree_names[1:(deg_base + 1)]
    deg_trt <- if (is.null(input$degree_trt)) 0 else input$degree_trt
    degree_trt_list <- 0:deg_trt
    names(degree_trt_list) <- degree_names[1:(deg_trt + 1)]
    fluidRow(
      column(6,
        column(6,
              checkboxGroupInput("FE_base", "Include fixed effect", degree_base_list, selected = degree_base_list)
        ),
        column(6,
              checkboxGroupInput("RE_base", "Include random effect", degree_base_list, selected = 0)
        )
      ),
      column(6,
        column(6,
              checkboxGroupInput("FE_base", "Include fixed effect", degree_trt_list, selected = degree_trt_list)
        ),
        column(6,
              checkboxGroupInput("RE_base", "Include random effect", degree_trt_list, selected = NULL)
        )
      )
    )
  })
  
  output$datTable <- renderTable(datClean())
  
}

shinyApp(ui = ui, server = server)
