library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)

source("example-mappings.R")
source("graphing-functions.R")
source("helper-functions.R")
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
  
  # Study design with defaults 
  
  studyDesign <- reactive({
    if (input$dat_type == "example") {
      exampleMapping[[input$example]]$design
    } else {
      input$design
    }
  })
  
  # Variable mapping interface
  
  output$variableMapping <- renderUI({
    var_names <- names(datFile())
    list(
      selectizeInput("filters", label = "Filtering variables", choices = var_names, selected = NULL, multiple = TRUE),
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
  
  # Filtering interface

  output$filterMapping <- renderUI({
    
    if (input$dat_type == "example") {
      example_parms <- exampleMapping[[input$example]]
      filter_vars <- example_parms$filters  
      filter_vals <- lapply(filter_vars, function(x) example_parms[[paste0("filter_", x)]])
      names(filter_vals) <- filter_vars
      header <- strong("Please select values for the filtering variables.")
    } else {
      filter_vars <- input$filters  
      filter_vals <- lapply(filter_vars, function(x) levels(as.factor(datFile()[,x])))
      names(filter_vals) <- filter_vars
      header <- strong("4. Please select values for the filtering variables.")
    }
    
    filter_selects <- lapply(filter_vars, function(x) 
      selectizeInput(paste0("filter_",x), label = x, choices = filter_vals[[x]], 
                     selected = filter_vals[[x]][1], multiple = TRUE))
    
    if (length(filter_vars) > 0) {
      filter_selects <- list(header, column(12, br()), filter_selects)
    }
    
    filter_selects
  })
  
  # Clean the data

  datClean <- reactive({
    if (input$dat_type == "example") {
      data(list = input$example)
      dat <- get(input$example)
      example_parms <- exampleMapping[[input$example]]
      filter_vars <- example_parms$filters  
      if (!is.null(filter_vars)) {
        subset_vals <- sapply(filter_vars, function(x) levels(dat[[x]])[dat[[x]]] %in% input[[paste0("filter_",x)]])
        dat <- dat[apply(subset_vals, 1, all),]
      } 
      dat <- dat[,example_parms$vars]
      names(dat) <- c("case","session","phase","outcome")
      trt_phase <- levels(as.factor(dat$phase))[2]
    } else {
      caseID <- as.factor(datFile()[,input$caseID])
      session <- as.numeric(datFile()[,input$session])
      phaseID <- as.factor(datFile()[,input$phaseID])
      outcome <- as.numeric(datFile()[,input$outcome])
      dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
      if (!is.null(input$filters)) {
        subset_vals <- sapply(input$filters, function(x) datFile()[[x]] %in% input[[paste0("filter_",x)]])
        dat <- dat[apply(subset_vals, 1, all),]
      } 
      trt_phase <- input$treatment
    }
    
    dat$trt <- as.numeric(dat$phase==trt_phase)
    
    if (studyDesign() == "MB") {
      dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
    } else {
      dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
    }
    return(dat)
  })
  
  output$datTable <- renderTable(datClean())
  
  # Raw data graph
  
  output$raw_plot <- renderPlot({
    graph_SCD(dat = datClean(), design = studyDesign()) 
  }, height = function() 120 * nlevels(datClean()$case),
     width = function() 700)

  time_range <- reactive({
    default_times(datClean())
  })
  
  # Centering slider
  output$model_centering <- renderUI({
    if (studyDesign()=="MB" & input$method=="RML") {
      session_range <- time_range()$range
      sliderInput("model_center", "Center session at", 
                  min=session_range[1], max=session_range[2], 
                  value=time_range()$A, step = 1)
    }
  })
  
  # Model degree
  
  output$modelDegree_baseline <- renderUI({
    max_degree <- if (studyDesign() == "MB") 6 else 0
    numericInput("degree_base", label = "Time trend degree", min = 0, max = max_degree, step = 1, value = 0)
  })
  
  output$modelDegree_treatment <- renderUI({
    max_degree <- if (studyDesign() == "MB") 6 else 0
    numericInput("degree_trt", label = "Time trend degree", min = 0, max = max_degree, step = 1, value = 0)
  })
  
  # Model specification
  
  output$modelSpec_baseline <- renderUI({
    deg_base <- if (is.null(input$degree_base)) 0 else input$degree_base
    degree_base_list <- 0:deg_base
    names(degree_base_list) <- degree_names[1:(deg_base + 1)]
    fluidRow(
       column(6,
              checkboxGroupInput("FE_base", "Include fixed effect", degree_base_list, selected = degree_base_list)
       ),
       column(6,
              checkboxGroupInput("RE_base", "Include random effect", degree_base_list, selected = 0)
       )
    )
  })
  
  output$modelSpec_treatment <- renderUI({
    deg_trt <- if (is.null(input$degree_trt)) 0 else input$degree_trt
    degree_trt_list <- 0:deg_trt
    names(degree_trt_list) <- degree_names[1:(deg_trt + 1)]
    fluidRow(
       column(6,
              checkboxGroupInput("FE_trt", "Include fixed effect", degree_trt_list, selected = degree_trt_list)
       ),
       column(6,
              checkboxGroupInput("RE_trt", "Include random effect", degree_trt_list, selected = NULL)
       )
    )
  })
  
  # Model spec output
  output$model_fit <- renderPrint({
    if (input$method=="RML") {
      fit_function <- list(MB = "lme_fit_MB", TR = "lme_fit_TR")[[studyDesign()]]
      fit <- do.call(fit_function,
                     args = list(dat = datClean(), 
                                 FE_base = input$FE_base, RE_base = input$RE_base,
                                 FE_trt = input$FE_trt, RE_trt = input$RE_trt))
      res <- list(fixed = fit$fixed, random = fit$random, fit = summary(fit$fit))
    } else {
      if (studyDesign()=="MB") {
        res <- with(datClean(), effect_size_MB(outcome = outcome, treatment = trt, id = case, time = session))
      } else {
        res <- with(datClean(), effect_size_ABk(outcome = outcome, treatment = trt, id = case, phase = phase_pair, time = session))
      }
    }
    res
  })
})
