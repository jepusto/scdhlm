library(shiny)
source("mappings.R")
library(readxl)

numericInput_inline <- function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL) {
  
  labelTag <- tags$label(label, `for` = inputId)
      
  inputTag <- tags$input(id = inputId, type = "number", class = "form-control", 
                         value = format(value, scientific = FALSE, digits = 15))
  
  if (!is.na(min)) inputTag$attribs$min = min
  if (!is.na(max)) inputTag$attribs$max = max
  if (!is.na(step)) inputTag$attribs$step = step
  
  if (!is.null(width)) {
    labelTag$attribs$width = width[1]
    inputTag$attribs$display = "inline-block"
    inputTag$attribs$width = width[2]
  } 
  
  div(class = "form-group shiny-input-container", labelTag, inputTag)
  
}

shinyUI(fluidPage(
   tags$head(tags$style(HTML("div#inline label { width: 50%; } div#inline input { display: inline-block; width: 25%;}"))),
   titlePanel("Between-case standardized mean difference estimator"),
   tabsetPanel(type = "tabs",
        
        #--------------------
        # Load the data
        #--------------------
        tabPanel("scdhlm",
                 navlistPanel(widths = c(3,9),
                              tabPanel("About", includeMarkdown("markdown/scdhlm.md")),
                              tabPanel("Accessing scdhlm", includeMarkdown("markdown/Accessing_scdhlm.md")),
                              tabPanel("References", includeMarkdown("markdown/references.md")),
                              tabPanel("Example data", includeMarkdown("markdown/example-data.md"))
                 )
        ),
        tabPanel("Load",
           br(),
           fluidRow(
             column(4,
                radioButtons('dat_type', 'What data do you want to use?', 
                             c("Use an example" = "example", "Upload data from a .csv or .txt file" = "dat", "Upload data from a .xlsx file" = "xlsx")),
                conditionalPanel(
                  condition = "input.dat_type == 'example'",
                  selectInput("example", label = "Choose an example", 
                              choices = exampleChoices)
                ),
                conditionalPanel(
                  condition = "input.dat_type == 'dat'",
                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
                  checkboxInput('header', 'File has a header?', TRUE),
                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' ')),
                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"))
                ),
                conditionalPanel(
                  condition = "input.dat_type == 'xlsx'",
                  fileInput('xlsx', 'Upload a .xlsx file', accept = c('.xlsx')),
                  checkboxInput('col_names', 'File has a header?', TRUE),
                  numericInput('sheet','What is the sheet number?', value = 1, min = 1),
                  selectInput("inSelect", "inSelect", "")
                )
             ),
             column(8,
                tableOutput("contents"),
                conditionalPanel(
                  condition = "output.fileUploaded & input.dat_type == 'dat' || output.fileUploaded & input.dat_type == 'dat2'",
                  selectInput("design", label = "1. Please specify the study design.",
                              choices = design_names),
                  strong("2. Please select the variable containing each type of information."),
                  column(12, br()),
                  uiOutput("variableMapping"),
                  strong("3. Please specify the baseline and treatment levels."),
                  column(12, br()),
                  uiOutput("phaseMapping")
                ),
                uiOutput("filterMapping")
             )
           ),
           fluidRow(br(),br(),br())
        ),
        
        #--------------------
        # Show loaded data
        #--------------------
        tabPanel("Inspect", 
          tabsetPanel(type = "tabs",
            tabPanel("Graph",
              column(12, br()),
              column(12,
                plotOutput("raw_plot", height = "auto")
              )
            ),
            tabPanel("Data",
              column(12, br()),
              tableOutput("datTable")
            )
          )
        ), 
        
        #--------------------
        # Model building
        #--------------------
        tabPanel("Model", 
           br(),
           fluidRow(
             column(6,
                selectInput("method", label = "Estimation method",
                            choices = estimation_names, 
                            selected = "RML")
             ),
             column(6,
                uiOutput("model_centering")
             )
           ),
           conditionalPanel(condition = "input.method == 'HPS'",
              plotOutput("HPS_plot", height = "auto")   
           ),
           
           conditionalPanel(condition = "input.method == 'RML'",
              fluidRow(
                column(6,
                   wellPanel(
                     h4("Baseline phase"),
                     uiOutput("modelDegree_baseline"),
                     uiOutput("modelSpec_baseline")
                   )
                ),
                column(6,
                   wellPanel(
                     h4("Treatment phase"),
                     uiOutput("modelDegree_treatment"),
                     uiOutput("modelSpec_treatment")
                   )
                )
              ),
              fluidRow(
                column(12, 
                htmlOutput("model_spec")
                )
              ),
              tabsetPanel(type = "tabs",
                tabPanel("Graph",
                  column(12, br(),
                    plotOutput("RML_plot", height = "auto")
                  )
                ),
                tabPanel("Model estimates",
                  column(12, br()),
                  verbatimTextOutput("model_fit")
                )
              )
           )
        ), 
        
        #------------------------------
        # Effect size estimation
        #------------------------------
        
        tabPanel("Effect size", 
          br(),
          uiOutput("ES_timing"),
          h4("Effect size estimates and auxilliary information"),
          div(id = "inline", 
              numericInput("coverage","CI coverage level (%)", value = 95, min = 0, max = 100, step = 0.5)
          ),
          tableOutput("effect_size_report"),
          downloadButton('download_ES', 'Download')
        )
   )
))

