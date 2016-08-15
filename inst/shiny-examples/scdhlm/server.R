library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)

source("example-mappings.R")
source("lme-fit.R")

shinyServer(function(input, output) {
  
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
      trt_phase <- levels(as.factor(dat$phase))[2]
    } else {
      caseID <- as.factor(datFile()[,input$caseID])
      session <- as.numeric(datFile()[,input$session])
      phaseID <- as.factor(datFile()[,input$phaseID])
      outcome <- as.numeric(datFile()[,input$outcome])
      dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
      trt_phase <- levels(dat$phase)[2]
    }
    dat$trt <- as.numeric(dat$phase==trt_phase)
    if (!is.null(input$design)) {
      if (input$design=="MB") dat$session_trt <- unlist(by(dat, dat$case, function(x) pmax(0, x$session - min(x$session[x$phase==trt_phase]))))
    } 
    return(dat)
  })
  
  output$datTable <- renderTable(datClean())
  
  # Model degree
  
  output$modelDegree <- renderUI({
    max_degree <- if (!is.null(input$design)) {if (input$design == "MB") 6 else 0} else 0
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
                    checkboxGroupInput("FE_trt", "Include fixed effect", degree_trt_list, selected = degree_trt_list)
             ),
             column(6,
                    checkboxGroupInput("RE_trt", "Include random effect", degree_trt_list, selected = NULL)
             )
      )
    )
  })
  
  # Model spec output
  output$model_fit <- renderPrint({
    if (!is.null(input$design)) {
      if (input$method=="RML") {
        fit_function <- list(MB = "lme_fit_MB", TR = "lme_fit_TR")[[input$design]]
        fit <- do.call(fit_function,
                       args = list(dat = datClean(), 
                                   FE_base = input$FE_base, RE_base = input$RE_base,
                                   FE_trt = input$FE_trt, RE_trt = input$RE_trt))
      } else {
        if (input$design=="MB") {
          fit <- with(datClean(), effect_size_MB(outcome = outcome, treatment = trt, id = case, time = session))
        } else {
          fit <- with(datClean(), effect_size_ABk(outcome = outcome, treatment = phase_indicator, id = case, phase = phase_pair, time = session))
        }
      }
      list(fixed = fit$fixed, random = fit$random, fit = summary(fit$fit))
    }
  })
  
})
