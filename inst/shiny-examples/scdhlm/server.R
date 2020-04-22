library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)
library(readxl)


source("mappings.R")
source("helper-functions.R")
source("lme-fit.R")

shinyServer(function(input, output, session) {
  
  # Read in data
  sheetname <- reactive({
    if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      if (is.null(inFile)) return(NULL)
      sheetnames <- excel_sheets(inFile$datapath)
      return(sheetnames)
    } else {
      return(NULL)
    }
  })
  
  observe({
    updateSelectInput(session, "inSelect", label = "Select a sheet",
                      choices = sheetname(),
                      selected = NULL)
  })
  
  datFile <- reactive({ 
    
    if (input$dat_type == "dat") {
 
      inFile <- input$dat
      
      if (is.null(inFile)) return(NULL)
  
      read.table(inFile$datapath, header=input$header, 
                 sep=input$sep, quote=input$quote,
                 stringsAsFactors = FALSE)
      
   } else if (input$dat_type == "xlsx") {
       
       inFile <- input$xlsx
       
       if (is.null(inFile)) return(NULL)
       
       as.data.frame(read_xlsx(inFile$datapath, col_names = input$col_names,
                 sheet = input$inSelect))
       
   } else {
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
    n_var <- length(var_names)
    list(
      selectizeInput("filters", label = "Filtering variables (optional)", choices = var_names, selected = NULL, multiple = TRUE),
      selectInput("caseID", label = "Case identifier", choices = var_names, selected = var_names[n_var - 3]),
      selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = var_names[n_var - 2]),
      selectInput("session", label = "Session number", choices = var_names, selected = var_names[n_var - 1]),
      selectInput("outcome", label = "Outcome", choices = var_names, selected = var_names[n_var])
    )
  })
  
  # Treatment label mapping interface
  
  output$phaseMapping <- renderUI({
    phases <- levels(as.factor(datFile()[,input$phaseID]))
    list(
      selectInput("baseline", label = "Baseline level", choices = phases, selected = phases[1]),
      selectInput("treatment", label = "Treatment level", choices = phases, selected = phases[2])
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
      case_vec <- datFile()[,input$caseID]
      caseID <- factor(case_vec, levels = unique(case_vec))
      session <- as.numeric(datFile()[,input$session])
      phase_vec <- datFile()[,input$phaseID]
      phaseID <- factor(phase_vec, levels = unique(phase_vec))
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
    
    # remove rows with missing outcome values
    dat <- dat[!is.na(dat$outcome),]
    dat <- droplevels(dat)
    return(dat)
  })
  
  output$datTable <- renderTable(datClean())

  # Centering and timing sliders for RML estimation of MBD
  
  time_range <- reactive({
    default_times(datClean())
  })
  
  output$model_centering <- renderUI({
    if (studyDesign()=="MB" & input$method=="RML") {
      session_range <- time_range()$range
      sliderInput("model_center", "Center session at", 
                  min=session_range[1], max=session_range[2], 
                  value=time_range()$A, step = 1)
    }
  })
  
  output$ES_timing <- renderUI({
    if (studyDesign()=="MB" & input$method=="RML") {
      timings <- time_range()
      wellPanel(
        fluidRow(
          column(12, 
             h4("Hypothetical experimental parameters")
          ),
          column(6, 
             sliderInput("A_time", "Initial treatment time",
               min = timings$range[1], max = timings$range[2],
               value = timings$A, step = 1)
          ),
          column(6,
             sliderInput("B_time", "Follow-up time",
                             min = timings$range[1], max = timings$range[2],
                             value = timings$B, step = 1)
          )
        )
      )  
    }
  })
  
  
  # Model degree
  
  output$modelDegree_baseline <- renderUI({
    if (studyDesign() == "MB") {
      selectInput("degree_base", label = "Type of time trend", choices = time_trends, width = "40%")  
    }
  })
  
  output$modelDegree_treatment <- renderUI({
    if (studyDesign() == "MB") {
      selectInput("degree_trt", label = "Type of time trend", choices = time_trends, width = "40%")  
    }
  })
  
  # Model specification
  
  output$modelSpec_baseline <- renderUI({
    deg_base <- if (is.null(input$degree_base)) 0 else as.numeric(input$degree_base)
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
    deg_trt <- if (is.null(input$degree_trt)) 0 else as.numeric(input$degree_trt)
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

  # Validate model specification
  
  model_validation <- reactive({
    validate_specification(input$FE_base, input$RE_base, 
                           input$FE_trt, input$RE_trt)
  })
  
  output$model_spec <- renderUI({model_validation()})
  
  # Fit model 
  
  model_fit <- reactive({
    fit_function <- list(MB = "lme_fit_MB", TR = "lme_fit_TR")[[studyDesign()]]
    do.call(fit_function,
                   args = list(dat = datClean(), 
                               FE_base = input$FE_base, RE_base = input$RE_base,
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt, 
                               center = input$model_center))
    
  })

  
  # Model spec output
  
  output$model_fit <- renderPrint({
    if (input$method=="RML") {
      list(fixed = model_fit()$fixed, 
                  random = model_fit()$random, 
                  fit = summary(model_fit()$fit),
                  converged = model_fit()$converged)
    } 
  })
  
  
  # Calculate effect sizes
  
  effect_size <- reactive({
    if ("lme" %in% class(model_fit()$fit)) {
      if (input$method=="RML") {
        A <- if (is.null(input$A_time)) 0L else input$A_time
        B <- if (is.null(input$B_time)) 1L else input$B_time
        res <- effect_size_RML(design = studyDesign(), dat = datClean(), 
                               FE_base = input$FE_base, RE_base = input$RE_base,
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt, 
                               A = A, B = B)
      } else {
        if (studyDesign()=="MB") {
          res <- with(datClean(), effect_size_MB(outcome = outcome, treatment = trt, id = case, time = session))
        } else {
          res <- with(datClean(), effect_size_ABk(outcome = outcome, treatment = trt, id = case, phase = phase_pair, time = session))
        }
      }
      filter_vars <- grep("filter_", names(input), value=TRUE)
      filter_vals <- if (length(filter_vars) > 0) lapply(filter_vars, function(x) input[[x]]) else NULL
      summarize_ES(res, 
                   filter_vars = filter_vars, filter_vals = filter_vals, 
                   design = studyDesign(), method = input$method, 
                   FE_base = input$FE_base, RE_base = input$RE_base,
                   FE_trt = input$FE_trt, RE_trt = input$RE_trt, 
                   A = input$A_time, B = input$B_time,
                   coverage = input$coverage)
      
    }
  })  
  
  # Effect size output
  
  output$effect_size_report <- renderTable({
    effect_size()
  }, digits = 4, include.rownames = FALSE)
  
  output$download_ES <- downloadHandler(
    filename = function() {
      fname <- if (input$dat_type == "example") input$example else input$dat
      paste(fname, '- effect size estimate.csv')
    },
    content = function(file) {
      dat <- effect_size()
      write.csv(dat, file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  # Graphs
  
  raw_graph <- reactive({
    graph_SCD(data = datClean(), design = studyDesign(), case=case, phase=phase, session=session, outcome=outcome, treatment_name = NULL, model_fit = NULL) 
  })
  
  output$raw_plot <- renderPlot({
    raw_graph()
  }, height = function() 120 * nlevels(datClean()$case),
  width = function() 700)
  
  output$HPS_plot <- renderPlot({
    raw_graph() + 
      geom_smooth(formula = y ~ 1, method = "lm", se = FALSE)
  }, height = function() 120 * nlevels(datClean()$case),
  width = function() 700)
  
  output$RML_plot <- renderPlot({
    if ("lme" %in% class(model_fit()$fit)) {
      dat <- datClean()
      dat$fitted <- predict(model_fit()$fit)
      raw_graph() + 
        geom_line(data = dat, aes(session, fitted), size = 0.8)  
    }
  }, height = function() 120 * nlevels(datClean()$case),
  width = function() 700)
  
  
 session$onSessionEnded(stopApp)
  
})
