library(shiny)

shinyUI(navbarPage(title = "scdhlm",
   tabPanel("Effect size estimation",
      tabsetPanel(type = "tabs",
        
        #--------------------
        # Load the data
        #--------------------
        tabPanel("Load",
           br(),
           fluidRow(
             column(4,
                radioButtons('dat_type', 'What data do you want to use?', 
                             c("Use an example" = "example", "Upload data from a file" = "dat")),
                conditionalPanel(
                  condition = "input.dat_type == 'example'",
                  selectInput("example", label = "Choose an example", 
                              choices = c("Anglesea (ABAB design)" = "Anglesea", 
                                          "Lambert (ABAB design)" = "Lambert",
                                          "Laski (multiple baseline design)" = "Laski",
                                          "Saddler (multiple baseline design)" = "Saddler",
                                          "Schutte (multiple baseline design)" = "Schutte",
                                          "Thorne (ABAB design)" = "Thorne"))
                ),
                conditionalPanel(
                  condition = "input.dat_type == 'dat'",
                  fileInput('dat', 'Upload a .csv or .txt file', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
                  checkboxInput('header','File contain a header.', TRUE),
                  radioButtons('sep', 'Data seperator', c(Commas=',', Semicolons=';', Tabs='\t', Spaces=' '), ','),
                  radioButtons('quote', 'Include quotes?', c('No'='', 'Double Quotes'='"', 'Single Quotes'="'"), '')
                )
             ),
             column(8,
                conditionalPanel(
                  condition = "output.fileUploaded & input.dat_type == 'dat'",
                  selectInput("design", label = "1. Please specify the study design.",
                              choices = c("Treatment reversal" = "TR", "Multiple baseline" = "MB")),
                  strong("2. Please select the variable containing each type of information."),
                  column(12, br()),
                  uiOutput("variableMapping"),
                  strong("3. Please specify the baseline and treatment levels."),
                  column(12, br()),
                  uiOutput("phaseMapping")
                ),
                uiOutput("filterMapping")
             )
           )
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
                            choices = c("Moment estimation" = "HPS", "Restricted Maximum Likelihood" = "RML"), 
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
                     strong("Baseline phase"),
                     uiOutput("modelDegree_baseline"),
                     uiOutput("modelSpec_baseline")
                   )
                ),
                column(6,
                   wellPanel(
                     strong("Treatment phase"),
                     uiOutput("modelDegree_treatment"),
                     uiOutput("modelSpec_treatment")
                   )
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
          uiOutput("ES_timing")
        )
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
))

