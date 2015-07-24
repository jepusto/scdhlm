library(shiny)
library(markdown)
library(ARPobservation)
library(dplyr)
library(ggplot2)
source("effect_sizes.R")
source("ARPsimulator.R")

ui <- navbarPage(title = "Alternating Renewal Process Simulator",
  tabPanel("Simulator",
  
  fluidRow(

    # Behavioral parameters
    column(4, h3("Behavioral parameters"),
           selectInput("behavior", label = "Behavior class", 
                       choices = c("Event behavior", "State behavior")),
           conditionalPanel(
             condition = "input.behavior=='Event behavior'",
             numericInput("freq", label = "Frequency (per min)", value = 1, min = 0, step = 0.1),
             numericInput("dispersion", label = "Variability", value = 1, min = 0.05, step = 0.05),
             numericInput("freq_change", label = "Percentage change in frequency", value = 0, min = -100, step = 10)
           ),
           conditionalPanel(
             condition = "input.behavior=='State behavior'",
             numericInput("duration", label = "Event duration (seconds)", value = 30, min = 0, step = 1),
             numericInput("interim_time", label = "Interim time (seconds)", value = 60, min = 0, step = 1),
             numericInput("duration_change", label = "Percentage change in event duration", value = 0, min = -100, step = 10),
             numericInput("interim_change", label = "Percentage change in Interim time", value = 0, min = -100, step = 10)
           ),
           sliderInput("immediacy", label = "Immediacy of change (%)", 
                       min = 0, max = 100, value = 100, step = 5)
    ),

    # Study design
    column(4, h3("Study design"),
           selectInput("design", label = "Study design", choices = c("Treatment Reversal","Multiple Baseline")),
           htmlOutput("cases_UI"),
           conditionalPanel(
             condition = "input.design=='Treatment Reversal'",
             numericInput("phase_pairs", label = "Number of (AB) phases", value = 2, min = 1),
             numericInput("sessions_TR", label = "Sessions per phase", value = 5, min = 1)
           ),
           conditionalPanel(
             condition = "input.design=='Multiple Baseline'",
             numericInput("sessions_MB", label = "Total number of sessions", value = 20, min = 1),
             htmlOutput("MB_phase_change_UI")
           )
    ),
    
    # Measurement procedures
    column(4, h3("Measurement procedures"),
           numericInput("session_length", label = "Session length (min)", value = 10, min = 1),
           htmlOutput("systemUI"),
           conditionalPanel(
             condition = "input.system=='Momentary time sampling'||input.system=='Partial interval recording'||input.system=='Whole interval recording'",
             numericInput("interval_length", label = "Interval length (seconds)", value = 15, min = 1)
           )
    )
  ),

  fluidRow(column(12, 
                  hr(),
                  h3("Results"))),
  
  tabsetPanel(id = "outputPanel", type = "tabs",
              tabPanel("SCD Graph",  
                column(12, br()),
                sidebarLayout(
                  sidebarPanel(width = 3,
                    numericInput("samplesGraph", label = "Samples per case", value = 1, min = 1, max = 100),
                    checkboxInput("showtruth", label = "Show true trend lines", value = FALSE),
                    column(12, align = "center", actionButton("simulateGraph", label = "Simulate!")),
                    br()
                  ),
                  mainPanel(width = 9,
                    plotOutput('SCDplot', height = "auto")
                    )
                )
              ),
              tabPanel("Effect sizes",
                column(12, br()),
                sidebarLayout(
                  sidebarPanel(width = 3,
                    selectInput("effect_size", label = "Effect size measure", choices = ES_choices),
                    conditionalPanel(
                      condition = "input.effect_size != 'Within-case SMD'",
                      radioButtons("improvement", label = "Direction of improvement", choices = list("increase" = 1, "decrease" = 2), selected = 1)
                      ),
                    numericInput("samplesES", label = "Samples per case", value = 100, min = 1, max = 1000),
                    checkboxInput("showAvgES", label = "Show average", value = FALSE),
                    column(12, align = "center", actionButton("simulateES", label = "Simulate!")),
                    br()
                    ),
                  mainPanel(width = 9,
                    plotOutput('ESplot', height = "auto")
                    )
                  )
                )
              )
  ),
  tabPanel("Help",
    navlistPanel(widths = c(3,9),
      tabPanel("Overview", includeMarkdown("markdown/Overview.md")),
      tabPanel("Behavioral parameters", includeMarkdown("markdown/Behavioral_parameters.md")),
      tabPanel("Event behaviors", includeMarkdown("markdown/Event_behaviors.md")),
      tabPanel("State behaviors", includeMarkdown("markdown/State_behaviors.md")),
      tabPanel("Study design features", includeMarkdown("markdown/Study_design.md")),
      tabPanel("Measurement procedures", includeMarkdown("markdown/Measurement_procedures.md")),
      tabPanel("Single-case graph", includeMarkdown("markdown/SCD_graph.md")),
      tabPanel("Effect size graph", includeMarkdown("markdown/ES_graph.md"))
    )
  ),
  tabPanel("About",
           navlistPanel(widths = c(3,9),
                        tabPanel("ARPsimulator", includeMarkdown("markdown/ARPsimulator.md")),
                        tabPanel("Accessing the simulator", includeMarkdown("markdown/Accessing_ARPsimulator.md"))
           )
  )
)

server <- function(input, output) {

  choices <- c("Frequency counting","Continuous recording", 
               "Momentary time sampling","Partial interval recording","Whole interval recording")
  
  output$systemUI <- renderUI( {
    choices_available <- switch(input$behavior,
                                "Event behavior" = choices[c(1,4)],
                                "State behavior" = choices[-1])
    selectInput("system", label = "Measurement system", choices = choices_available)
  })
  
  output$cases_UI <- renderUI({
    cases <- 1L + 2 * (input$design == "Multiple Baseline")
    numericInput("cases", label = "Number of cases", value = cases, min = 1)
  })
  
  output$MB_phase_change_UI <- renderUI({
      cases <- if (is.null(input$cases)) 1L else input$cases
      phase_changes <- trunc(input$sessions_MB * (1:cases) / (cases + 1))
      phase_change_list <- paste(phase_changes, collapse = ", ")
      textInput("phase_change_list", label = "Phase change times", value = phase_change_list)  
  })
  
  sim_dat <- eventReactive(c(input$outputPanel, input$simulateGraph, input$simulateES), {
    cases <- if (is.null(input$cases)) 1L else input$cases
    system <- if (is.null(input$system)) {
      if (input$behavior=="Event behavior") choices[1] else choices[2]
    } else {
      input$system
    }
    phase_changes <- get_phase_changes(input$design, input$sessions_TR, input$phase_pairs, 
                                       input$phase_change_list, cases)
    samples <- ifelse(input$outputPanel == "SCD Graph", input$samplesGraph, input$samplesES)
    dat <- phase_design(input$design, cases, input$phase_pairs, input$sessions_TR, 
                           input$sessions_MB, phase_changes, samples)
    dat <- simulate_measurements(dat, input$behavior, 
                                   input$freq, input$dispersion, input$freq_change, 
                                   input$duration, input$interim_time, input$duration_change,
                                   input$interim_change, input$immediacy, 
                                   system, input$interval_length, input$session_length)
    height <- max(300, 150 * cases)
    list(dat = dat, design = input$design, phase_changes = phase_changes, 
         system = system, height_SCD = height)
  })

  output$SCDplot <- renderPlot({
    if (input$simulateGraph > 0 | input$simulateES > 0) {
      with(sim_dat(), graph_SCD(dat, design, phase_changes, system, input$showtruth))
    }
  }, height = function() sim_dat()$height_SCD)

  output$ESplot <- renderPlot({
    if (input$simulateGraph > 0 | input$simulateES > 0) {
      graph_ES(sim_dat()$dat, input$effect_size, input$improvement, input$showAvgES)
    }
  }, height = 400)
  
}

shinyApp(ui = ui, server = server)
