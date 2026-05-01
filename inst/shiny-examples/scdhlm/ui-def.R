library(shiny)
library(rclipboard)

source("mappings.R", local = TRUE)

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

ui <- 
  shinyUI(fluidPage(
   tags$head(tags$style(HTML("div#inline label { width: 50%; } div#inline input { display: inline-block; width: 25%;}"))),
   titlePanel("Between-case standardized mean difference estimator"),
   tabsetPanel(id = "scdhlm_calculator", type = "tabs",
        
        #--------------------
        # Load the data
        #--------------------
        tabPanel("scdhlm",
                 navlistPanel(widths = c(3,9),
                              tabPanel("About", includeMarkdown("markdown/scdhlm.md")),
                              tabPanel("Accessing scdhlm", includeMarkdown("markdown/Accessing_scdhlm.md")),
                              tabPanel("Demo videos", includeMarkdown("markdown/demos.md")),
                              tabPanel("References", includeMarkdown("markdown/references.md")),
                              tabPanel("Example data", includeMarkdown("markdown/example-data.md"))
                 )
        ),
        tabPanel("Load",
           br(),
             fluidRow(
               column(4,
                      radioButtons('dat_type', 
                                   'What data do you want to use?',
                                   c("Use an example" = "example",
                                     "Upload data from a .csv or .txt file" = "dat",
                                     "Upload data from a .xlsx file" = "xlsx")),
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
                        selectInput("inSelect", "Select a sheet", "")
                      ),
                    ),
               column(8,
                      tableOutput("contents"),
                      conditionalPanel(
                        condition = "output.fileUploaded & input.dat_type == 'dat' || output.fileUploaded & input.dat_type == 'xlsx' || input.dat_type == 'loaded'",
                        selectInput("design", label = "1. Please specify the study design.",
                                    choices = design_names),
                        strong("2. Please select the variable containing each type of information."),
                        column(12, br()),
                        uiOutput("variableMapping"),
                        uiOutput("sessionIssues1"),
                        uiOutput("sessionIssues2"),
                        uiOutput("outcomeMapping"),
                        uiOutput("outcomeIssue"),
                        strong("3. Please specify the baseline and treatment levels."),
                        column(12, br()),
                        uiOutput("phaseMapping"), 
                        strong("4. Please select the variables you wish to filter (optional)."),
                        column(12, br()),
                        uiOutput("filtervarMapping")
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
           uiOutput("estMethod"), 
           conditionalPanel(condition = "input.method == 'HPS'",
              plotOutput("HPS_plot", height = "auto")   
           ),
           
           conditionalPanel(condition = "input.method == 'RML' || input.method == 'Bayes'",
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
              fluidRow(
                column(12,
                  textOutput("ES_timing_message"),
                  br()
                )
              ),
              fluidRow(
                column(12, 
                       wellPanel(
                         h4("Session-level error structure assumptions"),
                         fluidRow(
                           column(6,
                                  selectInput("corStruct",
                                               label = "Correlation structure of session-level errors",
                                               choices = c("Auto-regressive (AR1)" = "AR1", "Moving average (MA1)" = "MA1", "Independent" = "IID"),
                                               selected = "AR1")
                           ),
                           column(6,
                                  selectInput("varStruct",
                                               label = "Variance of session-level errors",
                                               choices = c("Constant variance" = "hom",
                                                           "Variance differs by phase" = "het"),
                                               selected = "hom")
                           )                           
                         )
                       ))
              ),
              fluidRow(
                column(12,
                         h4("Run the analysis"),
                         fluidRow(
                           column(6,
                                  wellPanel(
                                    actionButton("runModel", "RUN", class = "btn-primary"),
                                    helpText(
                                      p("Notes:",
                                        tags$ul(
                                          tags$li(
                                            "Remember to click the RUN button every time you change the model specifications or the center point."
                                          ),
                                          tags$li(
                                            "It might take a while for the Bayesian models to run."
                                          )
                                        )
                                      )
                                    )
                                  )
                           ),
                           column(6,
                                  conditionalPanel(condition = "input.method == 'Bayes'",
                                                   wellPanel(
                                                     checkboxInput("bshow_advOpts", "Show advanced options for Bayesian estimation", value = FALSE),
                                                     h5("Advanced options for Bayesian estimation"),
                                                     helpText(
                                                       "If not set, these advanced options have defaults."
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.bshow_advOpts",
                                                       helpText(
                                                         p("For these following advanced options, details can be found on the `calc_BCSMD()` help page.")
                                                       ),
                                                       fluidRow(
                                                         column(6,
                                                                numericInput("badvOpts_seed", "Seed:", value = NA, step = 1L),
                                                                numericInput("badvOpts_cores", "Cores:",
                                                                             value = 1L, step = 1L, min = 1L),
                                                                numericInput("badvOpts_chains", "Chains (MCMC chains):",
                                                                             value = 4L, step = 1L, min = 1L)),
                                                         column(6,
                                                                numericInput("badvOpts_iter", "Total iterations per chain:",
                                                                             value = 2000L, step = 1L, min = 1L),
                                                                numericInput("badvOpts_warmup", "Warmup iterations per chain:",
                                                                             value = 1000L, step = 1L, min = 0L),
                                                                numericInput("badvOpts_thin", "Thinning rate:",
                                                                             value = 10L, step = 1L, min = 1L))
                                                       )
                                                     )
                                                   ))
                           )
                       ))
              )
           ),
           
           conditionalPanel(condition = "input.method == 'RML'",
                            tabsetPanel(type = "tabs",
                              tabPanel("Graph",
                                column(12, br(),
                                  plotOutput("RML_plot", height = "auto")
                                )
                              ),
                              tabPanel("Model estimates",
                                       column(12, br()),
                                       conditionalPanel(condition = "input.degree_base != 0",
                                                        uiOutput("model_centering")),
                                       fluidRow(
                                         column(12, h4("Model fit"), tableOutput("model_sample_size")),
                                         column(12, tableOutput("model_fit_fixed")),
                                         column(12, tableOutput("model_fit_random")),
                                         column(12, tableOutput("model_fit_corr")),
                                         column(12, tableOutput("model_fit_var")),
                                         column(12, tableOutput("model_info")),
                                         column(12, h4("Convergence"), tableOutput("model_fit_convg"))
                                       )
                              )
                            )
                            
           ),
           
           conditionalPanel(condition = "input.method == 'Bayes'",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Model estimates",
                                                 column(12, br()),
                                                 conditionalPanel(condition = "input.degree_base != 0",
                                                                  uiOutput("model_centering2")),
                                                 fluidRow(
                                                   column(12, h4("Model fit"), tableOutput("Bayes_sample_size")),
                                                   column(12, tableOutput("Bayes_fit_fixed")),
                                                   column(12, tableOutput("Bayes_fit_random")),
                                                   column(12, tableOutput("Bayes_fit_corr")),
                                                   column(12, tableOutput("Bayes_fit_var")),
                                                   column(12, tableOutput("Bayes_info"))
                                                 )
                                        ),
                                        tabPanel("Bayesian plots",
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Density plot",
                                                             column(12, br(),
                                                                    plotOutput("Bayes_dens", height = "auto"))
                                                    ),
                                                    tabPanel("Auto-correlation plot",
                                                             column(12, br(),
                                                                    plotOutput("Bayes_ar", height = "auto"))
                                                    ),
                                                    tabPanel("Trace plot",
                                                             column(12, br(),
                                                                    plotOutput("Bayes_trace", height = "auto"))
                                                    ),
                                                    tabPanel("Rhat",
                                                             column(12, br(),
                                                                    plotOutput("Bayes_rhat", height = "auto"))
                                                    ),
                                                    tabPanel("Overlaid density plot",
                                                             column(12, br(),
                                                                    plotOutput("Bayes_overlaid", height = "auto"))
                                                    ),
                                          ))
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
        ),
        
        
        tabPanel("Syntax for R",
                 rclipboardSetup(),
                 uiOutput("clip"),
                 verbatimTextOutput("syntax")
        )
   )
))

