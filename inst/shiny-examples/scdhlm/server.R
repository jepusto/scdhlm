library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)
library(readxl)
library(janitor)

source("mappings.R", local = TRUE)
source("helper-functions.R", local = TRUE)
source("lme-fit.R", local = TRUE)

server <- 
  shinyServer(function(input, output, session) {
    
    if (exists("dataset", where = environment(server))) if (!is.null(dataset)) {
      updateRadioButtons(
        session, 
        inputId = 'dat_type', 
        label = 'What data do you want to use?',
        choices = c("Use an example" = "example",
                    "Upload data from a .csv or .txt file" = "dat",
                    "Upload data from a .xlsx file" = "xlsx",
                    "Use the dataset specified at initialization" = "loaded"),
        selected = "loaded"
      )
    }
    
    sheetname <- reactive({
      if (input$dat_type == "xlsx") {
        inFile <- input$xlsx
        if (is.null(inFile)) return(NULL)
        sheetnames <- excel_sheets(inFile$datapath)
      } 
    })
    
    observe({
      sheets <- sheetname()
      updateSelectInput(session, "inSelect", label = "Select a sheet",
                        choices = sheets,
                        selected = sheets[1])
    })
    
    # Read in data
    
    datFile <- reactive({ 

      if (input$dat_type == "dat") {
        
        inFile <- input$dat
        
        if (is.null(inFile)) return(NULL)
        
        read.table(inFile$datapath, header=input$header, 
                   sep=input$sep, quote=input$quote,
                   stringsAsFactors = FALSE) %>% 
          clean_names(case = "parsed")
        
      } else if (input$dat_type == "xlsx") {
        
        inFile <- input$xlsx
        
        if (is.null(inFile) || is.null(input$inSelect) || nchar(input$inSelect) == 0) return(NULL)
        
        readxl::read_xlsx(inFile$datapath, col_names = input$col_names,
                          sheet = input$inSelect) %>% 
          clean_names(case = "parsed") %>%
          as.data.frame()
        
      } else if (input$dat_type == "loaded") {
        
        dataset %>% 
          clean_names(case = "parsed")
        
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
      if (studyDesign() %in% c("MBP", "TR")) {
        list(
          selectInput("caseID", label = "Case identifier", choices = var_names, selected = var_names[n_var - 3]),
          selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = var_names[n_var - 2]),
          selectInput("session", label = "Session number", choices = var_names, selected = var_names[n_var - 1])
        )
      } else if (studyDesign() == "RMBB") {
        list(
          selectInput("caseID", label = "Case identifier", choices = var_names, selected = var_names[n_var - 4]),
          selectInput("seriesID", label = "Series identifier", choices = var_names, selected = var_names[n_var - 3]),
          selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = var_names[n_var - 2]),
          selectInput("session", label = "Session number", choices = var_names, selected = var_names[n_var - 1])
        )
      } else if (studyDesign() == "CMB") {
        list(
          selectInput("clusterID", label = "Cluster identifier", choices = var_names, selected = var_names[n_var - 4]),
          selectInput("caseID", label = "Case identifier", choices = var_names, selected = var_names[n_var - 3]),
          selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = var_names[n_var - 2]),
          selectInput("session", label = "Session number", choices = var_names, selected = var_names[n_var - 1])
        )
      }
      
    })
    
    variablesLoaded <- reactive({
      if (is.null(input$caseID) | is.null(input$phaseID) | is.null(input$session) | is.null(input$outcome)) {
        FALSE
      } else {
        (nchar(input$caseID) > 0) & (nchar(input$phaseID) > 0) & (nchar(input$session) > 0) & (nchar(input$outcome) > 0)
      }
    }) 
    
    # Check Session variable
    
    output$sessionIssues1 <- renderUI({
      
      if (!variablesLoaded()) return(NULL)
      
      sessions <- datFile()[,input$session]
      
      if (!is.numeric(sessions) && !is.integer(sessions)) {
        list(
          strong(style="color:red", "Session variable contains non-numeric data."),
          br("")
        )
        
      } else if (!isTRUE(all.equal(sessions, round(sessions)))) {
        checkboxInput('round_session', 'Round Session variable to nearest integer?', TRUE)
      }
    })
    
    output$sessionIssues2 <- renderUI({
      
      if (!variablesLoaded()) return(NULL)
      
      sessions <- datFile()[,input$session]
      case_var <- datFile()[,input$caseID,drop=FALSE]
      filter_vars <- datFile()[,input$filters,drop=FALSE]
      split_vars <- cbind(case_var, filter_vars)
      if (studyDesign() == "RMBB") {
        series_var <- datFile()[, input$seriesID, drop=FALSE]
        split_vars <- cbind(case_var, series_var, filter_vars)
      } else if (studyDesign() == "CMB") {
        cluster_var <- datFile()[, input$clusterID, drop=FALSE]
        split_vars <- cbind(cluster_var, case_var, filter_vars)
      }
      
      if (length(split_vars) > 0 & is.numeric(sessions)) {
        
        if (!is.null(input$round_session)) if (input$round_session) sessions <- round(sessions)
        
        unique_sessions <- tapply(sessions, split_vars, 
                                  function(x) isTRUE(all.equal(x, unique(x))))
        
        if (!all(unique_sessions, na.rm = TRUE)) {
          list(
            strong(style="color:red", "Session variable contains repeated values. Please ensure that each data point has a unique value within each case."),
            br("") 
          )
        }
      }
      
    })
    
    output$outcomeMapping <- renderUI({
      var_names <- names(datFile())
      n_var <- length(var_names)
      selectInput("outcome", label = "Outcome variable", choices = var_names, selected = var_names[n_var])
    })
    
    # Check Outcome variable
    
    output$outcomeIssue <- renderUI({
      
      if (!variablesLoaded()) return(NULL)
      
      outcomes <- datFile()[,input$outcome]
      
      if (!is.numeric(outcomes) && !is.integer(outcomes)) {
        list(
          strong(style="color:red", "Outcome variable contains non-numeric data. Please use a numeric outcome variable."),
          br("")
        )
      } 
      
    })
    
    # Treatment label mapping interface
    
    output$phaseMapping <- renderUI({
      
      if (!variablesLoaded()) return(NULL)
      
      phases <- levels(as.factor(datFile()[,input$phaseID]))
      list(
        selectInput("baseline", label = "Baseline level", choices = phases, selected = phases[1]),
        selectInput("treatment", label = "Treatment level", choices = phases, selected = phases[2])
      )
      
    })
    
    # Filtering interface
    
    output$filtervarMapping <- renderUI({
      
      if (input$dat_type == "example") {
      } else {
        var_names <- names(datFile())
        n_var <- length(var_names)
        list(
          selectizeInput("filters", label = "Filtering variables", choices = var_names, selected = NULL, multiple = TRUE)
        )
      }
      
    })
    
    output$filterMapping <- renderUI({
      
      if (input$dat_type == "example") {
        if (is.null(input$example)) return(NULL)
        example_parms <- exampleMapping[[input$example]]
        filter_vars <- example_parms$filters  
        filter_vals <- lapply(filter_vars, function(x) example_parms[[paste0("filter_", x)]])
        names(filter_vals) <- filter_vars
        header <- strong("Please select the variables you wish to filter.")
      } else {
        filter_vars <- input$filters  
        filter_vals <- lapply(filter_vars, function(x) levels(as.factor(datFile()[,x])))
        names(filter_vals) <- filter_vars
        header <- strong("Values for the filtering variables.")
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
        if (studyDesign() %in% c("MBP", "TR")) {
          names(dat) <- c("case","session","phase","outcome")
          cluster <- series <- NULL
        } else if (studyDesign() == "RMBB"){
          names(dat) <- c("studyID","case", "series", "outcome", "session", "phase", "trt_time", "time_c")
          cluster <- NULL
        } else if (studyDesign() == "CMB") {
          names(dat) <- c("studyID","cluster", "case", "phase", "session", "session_trt", "outcome", "session_c")
          series <- NULL
        }
        
        dat <- preprocess_SCD(design = studyDesign(), 
                              cluster = cluster, 
                              case = case, series = series, 
                              phase = phase, 
                              session = session, outcome = outcome, 
                              data = dat)
        
      } else {
        
        case <- datFile()[,input$caseID]
        phase <- datFile()[,input$phaseID]
        session <- as.numeric(datFile()[,input$session])
        outcome <- as.numeric(datFile()[,input$outcome])
        cluster <- series <- NULL
        dat <- data.frame(case = case, phase = phase, session = session, outcome = outcome)
        
        if (studyDesign() == "RMBB") {
          dat$series <- datFile()[,input$seriesID]
          dat <- dat[order(dat$case, dat$series, dat$session),]
        } else if (studyDesign() == "CMB") {
          dat$cluster <- datFile()[,input$clusterID]
          dat <- dat[order(dat$cluster, dat$case, dat$session),]
        } else {
          dat <- dat[order(dat$case, dat$session),]
        }
        
        if (!is.null(input$filters)) {
          subset_vals <- sapply(input$filters, function(x) datFile()[[x]] %in% input[[paste0("filter_",x)]])
          dat <- dat[apply(subset_vals, 1, all),]
        } 
        
        design <- studyDesign()
        round_session <- if (!is.null(input$round_session)) TRUE else FALSE
        treatment_name <- input$treatment
        
        dat <- preprocess_SCD(design = design, 
                              cluster = cluster, case = case, series = series,
                              phase = phase, session = session, outcome = outcome, 
                              round_session = round_session, treatment_name = treatment_name,
                              data = dat)

      }
      
      # if (studyDesign() == "MBP") {
      #   names(dat)[6] <- "session_trt"
      # } else if (studyDesign() == "TR") {
      #   names(dat)[6] <- "phase_pair"
      # } else if (studyDesign() %in% c("RMBB", "CMB")) {
      #   names(dat)[7] <- "session_trt"
      # } 
      
      return(dat)
      
    })

    output$datTable <- renderTable(datClean())
    
    # Estimation method
    
    output$estMethod <- renderUI({
      estimation_choices <- if (studyDesign() %in% c("MBP", "TR")) {
        estimation_names 
      } else {
        c("Restricted Maximum Likelihood" = "RML")
      }
      
      selectInput("method", label = "Estimation method",
                  choices = estimation_choices, 
                  selected = "RML")
    })
    
    # Centering and timing sliders for RML estimation of MBD
    
    time_range <- reactive({
      default_times(datClean())
    })
    
    output$model_centering <- renderUI({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method=="RML") {
        session_range <- time_range()$range
        sliderInput("model_center", "Center session at", 
                    min=session_range[1], max=session_range[2], 
                    value=time_range()$A, step = 1)
      }
    })
    
    output$ES_timing_message <- renderText({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method=="RML" &
          any(input$degree_trt != 0, (input$degree_base != 0 & input$RE_base > 0))) {
        note_txt <- "Note: Options for selecting an initial treatment time and follow-up time can be modified on the next page." 
        HTML(note_txt)
      }
    })
    
    output$ES_timing <- renderUI({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method=="RML" & 
          any(input$degree_trt != 0, (input$degree_base != 0 & input$RE_base > 0))) {
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
      if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
        selectInput("degree_base", label = "Type of time trend", choices = time_trends_baseline, width = "40%")  
      }
    })
    
    output$modelDegree_treatment <- renderUI({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
        selectInput("degree_trt", label = "Type of time trend", choices = time_trends_treatment, width = "40%")  
      }
    })
    
    # Model specification
    
    output$modelSpec_baseline <- renderUI({
      deg_base <- if (is.null(input$degree_base)) 0 else as.numeric(input$degree_base)
      degree_base_list <- 0:deg_base
      names(degree_base_list) <- degree_names_baseline[1:(deg_base + 1)]
      if (studyDesign() %in% c("MBP", "TR")) {
        fluidRow(
          column(6,
                 checkboxGroupInput("FE_base", labs_MBP$fixed, degree_base_list, selected = degree_base_list)
          ),
          column(6,
                 checkboxGroupInput("RE_base", labs_MBP$random, degree_base_list, selected = 0)
          )
        )
      } else if (studyDesign() == "RMBB") {
        fluidRow(
          column(4,
                 checkboxGroupInput("FE_base", labs_RMBB$fixed, degree_base_list, selected = degree_base_list)
          ),
          column(4,
                 checkboxGroupInput("RE_base", labs_RMBB$random_series, degree_base_list, selected = 0)
          ),
          column(4,
                 checkboxGroupInput("RE_base2", labs_RMBB$random_case, degree_base_list, selected = 0)
          )
        )
      } else if (studyDesign() == "CMB") {
        fluidRow(
          column(4,
                 checkboxGroupInput("FE_base", labs_CMB$fixed, degree_base_list, selected = degree_base_list)
          ),
          column(4,
                 checkboxGroupInput("RE_base", labs_CMB$random_case, degree_base_list, selected = 0)
          ),
          column(4,
                 checkboxGroupInput("RE_base2", labs_CMB$random_cluster, degree_base_list, selected = 0)
          )
        )
      }
      
    })
    
    output$modelSpec_treatment <- renderUI({
      deg_trt <- if (is.null(input$degree_trt)) 0 else as.numeric(input$degree_trt)
      degree_trt_list <- 0:deg_trt
      names(degree_trt_list) <- degree_names_treatment[1:(deg_trt + 1)]
      if (studyDesign() %in% c("MBP", "TR")) {
        fluidRow(
          column(6,
                 checkboxGroupInput("FE_trt", labs_MBP$fixed, degree_trt_list, selected = degree_trt_list)
          ),
          column(6,
                 checkboxGroupInput("RE_trt", labs_MBP$random, degree_trt_list, selected = NULL)
          )
        )
      } else if (studyDesign() == "RMBB") {
        fluidRow(
          column(4,
                 checkboxGroupInput("FE_trt", labs_RMBB$fixed, degree_trt_list, selected = degree_trt_list)
          ),
          column(4,
                 checkboxGroupInput("RE_trt", labs_RMBB$random_series, degree_trt_list, selected = NULL)
          ),
          column(4,
                 checkboxGroupInput("RE_trt2", labs_RMBB$random_case, degree_trt_list, selected = NULL)
          )
        )
      } else if (studyDesign() == "CMB") {
        fluidRow(
          column(4,
                 checkboxGroupInput("FE_trt", labs_CMB$fixed, degree_trt_list, selected = degree_trt_list)
          ),
          column(4,
                 checkboxGroupInput("RE_trt", labs_CMB$random_case, degree_trt_list, selected = NULL)
          ),
          column(4,
                 checkboxGroupInput("RE_trt2", labs_CMB$random_cluster, degree_trt_list, selected = NULL)
          )
        )
      }

    })
    
    # Validate model specification
    
    model_validation <- reactive({
      if (studyDesign() %in% c("MBP", "TR")) {
        validate_specification(design = studyDesign(), n_outer_levels = nlevels(datClean()$case),
                               FE_base = input$FE_base, RE_base = input$RE_base, 
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt)
      } else if (studyDesign() == "RMBB") {
        validate_specification(design = studyDesign(), n_outer_levels = nlevels(datClean()$case),
                               FE_base = input$FE_base, RE_base = input$RE_base,  
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt,
                               RE_base2 = input$RE_base2, RE_trt2 = input$RE_trt2)
      } else {
        validate_specification(design = studyDesign(), n_outer_levels = nlevels(datClean()$cluster),
                               FE_base = input$FE_base, RE_base = input$RE_base,  
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt,
                               RE_base2 = input$RE_base2, RE_trt2 = input$RE_trt2)
      }
      
    })
    
    output$model_spec <- renderUI({model_validation()})
    
    # Fit model 
    
    model_fit <- reactive({
      
      center <- if (input$degree_base == 0 || is.null(input$model_center)) 0L else input$model_center
      
      fit_function <- switch(studyDesign(), 
                             MBP = "lme_fit_MB", 
                             RMBB = "lme_fit_MB", 
                             CMB = "lme_fit_MB", 
                             TR = "lme_fit_TR")
      
      if (studyDesign() %in% c("MBP", "TR")) {
        RE_base2 <- RE_trt2 <- NULL
      } else {
        RE_base2 <- input$RE_base2
        RE_trt2 <- input$RE_trt2
      }
      do.call(fit_function,
              args = list(design = studyDesign(), dat = datClean(), 
                          FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2,
                          FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
                          varStruct = input$varStruct,
                          corStruct = input$corStruct,
                          center = center))
      
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
        
        if (studyDesign() %in% c("MBP", "TR")) {
          RE_base2 <- RE_trt2 <- NULL
        } else {
          RE_base2 <- input$RE_base2
          RE_trt2 <- input$RE_trt2
        }
        
        if (input$method=="RML") {
          center <- if (input$degree_base == 0 || is.null(input$model_center)) 0L else input$model_center
          A <- if (is.null(input$A_time)) 0L else input$A_time
          B <- if (is.null(input$B_time)) 1L else input$B_time
          res <- effect_size_RML(design = studyDesign(), dat = datClean(), 
                                 FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2,
                                 FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
                                 varStruct = input$varStruct,
                                 corStruct = input$corStruct,
                                 A = A, B = B, center = center)
        } else {
          if (studyDesign()=="MBP") {
            res <- with(datClean(), effect_size_MB(outcome = outcome, treatment = trt, id = case, time = session))
          } else if (studyDesign()=="TR") {
            res <- with(datClean(), effect_size_ABk(outcome = outcome, treatment = trt, id = case, phase = phase_pair, time = session))
          } 
        } 
        
        example_parms <- exampleMapping[[input$example]]
        if (input$dat_type == "example" & !is.null(example_parms$filters)) {
          filter_vars <- example_parms$filters
          filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                            function(x) paste0(input[[x]], collapse = ",")) else NULL
          names(filter_vals) <- filter_vars
          filter_vals <- as.data.frame(filter_vals)
        } else if (input$dat_type %in% c("dat", "xlsx") & !is.null(input$filters)) {
          filter_vars <- input$filters
          filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                            function(x) paste0(input[[x]], collapse = ",")) else NULL
          names(filter_vals) <- filter_vars
          filter_vals <- as.data.frame(filter_vals)
        } else {
          filter_vars <- NULL
          filter_vals <- NULL
        } 
        
        summarize_ES(res, 
                     filter_vals = filter_vals, 
                     design = studyDesign(), method = input$method, 
                     FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2, 
                     FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
                     corStruct = input$corStruct,
                     varStruct = input$varStruct,
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
    cluster <- if (studyDesign() == "CMB") substitute(cluster) else NULL
    series <- if (studyDesign() == "RMBB") substitute(series) else NULL
    graph_SCD(design = studyDesign(), 
              cluster = cluster, case = case, series = series, 
              phase = phase, session = session, outcome = outcome, 
              treatment_name = NULL, model_fit = NULL, data = datClean())

  })
  
  output$raw_plot <- renderPlot({
    raw_graph()
  }, height = function() 120 * nlevels(datClean()[[1]]),
  width = function() 700)
  
  output$HPS_plot <- renderPlot({
    raw_graph() + 
      geom_smooth(formula = y ~ 1, method = "lm", se = FALSE)
  }, height = function() 120 * nlevels(datClean()[[1]]),
  width = function() 700)
  
  output$RML_plot <- renderPlot({
    
    if ("lme" %in% class(model_fit()$fit)) {
      cluster <- if (studyDesign() == "CMB") substitute(cluster) else NULL
      series <- if (studyDesign() == "RMBB") substitute(series) else NULL
      graph_SCD(design = studyDesign(), 
                cluster = cluster, case = case, series = series, 
                phase = phase, session = session, outcome = outcome, 
                model_fit = model_fit()$fit)
    }
  }, height = function() 120 * nlevels(datClean()[[1]]),
  width = function() 700)
  
  
  #------------------------------
  # Syntax for replication in R
  #------------------------------

  ES_syntax <- reactive({
    
    header_res <- c(
      '# Load packages',
      'library(nlme)',
      'library(scdhlm)',
      ''
    )
    
    # read in file code
    
    if(input$dat_type == "example") {
      read_res <- c(
        parse_code_chunk("load-example", args = list(example_name = input$example)),
        ''
      )
    } else if(input$dat_type == "dat"){
      inFile <- input$dat
      read_res <- c(
        parse_code_chunk("load-dat", 
                         args = list(user_path = inFile$name, user_header = input$header, 
                                     user_sep = input$sep, user_quote = input$quote)),
        ''
      )
    } else if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      read_res <- c(
        parse_code_chunk("load-excel", args = list(user_path = inFile$name, user_sheet = input$inSelect)),
        ''
      )
    } else {
      read_res <- c(
        parse_code_chunk("load-from-function", args = NULL),
        ''
      )
    }

    # Clean the data
  
    if (input$degree_base == 0 || is.null(input$model_center)) {
      model_center <- 0
    } else {
      model_center <- input$model_center
    }
    
    if (input$dat_type == "example") {
      
      example_parms <- exampleMapping[[input$example]]
      filter_vars <- example_parms$filters
      filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                        function(x) paste0('"', input[[x]], '"', collapse = ",")) else NULL
      filter_vals <- paste0("%in% c(", filter_vals, ")")
      filter_string <- paste(example_parms$filters, filter_vals, collapse = " & ")
      
      if (!is.null(example_parms$filters)) {
        clean_dat_A <- c(
          parse_code_chunk("clean-example-filter", 
                           args = list(user_filterString = filter_string))
        )
      } else {
        clean_dat_A <- c()
      }
      
      case <- "case"
      session <- "session"
      phase <- "phase"
      outcome <- "outcome"
      series <- "series"
      cluster <- "cluster"
      
      if (studyDesign() == "TR") {
        clean_dat <- c(clean_dat_A,
                       '',
                       parse_code_chunk("clean-example-nofilter-TR",
                                        args = list(user_parms = paste(example_parms$vars, collapse='", "'),
                                                    user_design = studyDesign()))
        )
      } else if (studyDesign() == "MBP") {
        clean_dat <- c(clean_dat_A,
                       '',
                       parse_code_chunk("clean-example-nofilter",
                                        args = list(user_parms = paste(example_parms$vars, collapse='", "'),
                                                    user_design = studyDesign(),
                                                    user_model_center = model_center))
        )
      } else if (studyDesign() == "RMBB") {
        clean_dat <- c(clean_dat_A,
                       '',
                       parse_code_chunk("clean-example-nofilter-RMBB",
                                        args = list(user_parms = paste(example_parms$vars[2:6], collapse='", "'),
                                                    user_design = studyDesign(),
                                                    user_model_center = model_center))
        )
      } else if (studyDesign() == "CMB") {
        clean_dat <- c(clean_dat_A,
                       '',
                       parse_code_chunk("clean-example-nofilter-CMB",
                                        args = list(user_parms = paste(example_parms$vars[-c(1,6,8)], collapse='", "'),
                                                    user_design = studyDesign(),
                                                    user_model_center = model_center))
        )
      }
      
    } else {
      
      case <- input$caseID
      session <- input$session
      phase <- input$phaseID
      outcome <- input$outcome
      cluster <- if (studyDesign() == "CMB") input$clusterID else "NULL"
      series <- if (studyDesign() == "RMBB") input$seriesID else "NULL"
      round_session <- if (!is.null(input$round_session)) TRUE else FALSE
      
      filter_vars <- input$filters
      filter_vals <- if(length(filter_vars) > 0) lapply(paste0("filter_", filter_vars), 
                                                        function(x) paste0('"', input[[x]], '"', collapse = ",")) else NULL
      filter_vals <- paste0("%in% c(", filter_vals, ")")
      filter_string <- paste(input$filters, filter_vals, collapse = " & ")
      
      if (!is.null(input$filters)) {
        clean_dat_B <- c(
          '',
          parse_code_chunk("clean-inputdata-filter", args = list(user_filterString = filter_string))
        )
      } else {
        clean_dat_B <- c()
      }

      if (studyDesign() == "TR") {
          clean_dat <- c(clean_dat_B,
                         '',
                         parse_code_chunk("clean-inputdata-nofilter-TR", 
                                          args = list(user_caseID = case, 
                                                      user_session = session,
                                                      user_phaseID = phase,
                                                      user_outcome = outcome,
                                                      user_design = studyDesign(),
                                                      user_treatment = input$treatment,
                                                      user_round = round_session))
          )
        } else if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
          clean_dat <- c(clean_dat_B,
                         '',
                         parse_code_chunk("clean-inputdata-nofilter", 
                                          args = list(user_caseID = case,
                                                      user_session = session,
                                                      user_phaseID = phase,
                                                      user_outcome = outcome,
                                                      user_design = studyDesign(),
                                                      user_model_center = model_center,
                                                      user_treatment = input$treatment,
                                                      user_round = round_session,
                                                      user_clusterID = cluster,
                                                      user_seriesID = series))
          )
        }
      }
    
    # Fit the model
    
    if (input$method=="RML") {
      
      if (studyDesign() == "TR") {
        session_FE <- if (is.null(input$FE_base) | !(0 %in% input$FE_base)) "0" else "1"
        trt_FE <- if (is.null(input$FE_trt) | !(0 %in% input$FE_trt)) NULL else "trt"
        session_RE <- if (is.null(input$RE_base) | !(0 %in% input$RE_base)) "0" else "1"
        trt_RE <- if (is.null(input$RE_trt) | !(0 %in% input$RE_trt)) NULL else "trt"
      } else {
        session_FE <- write_formula(input$FE_base, c("0","1", session))
        trt_FE <- write_formula(input$FE_trt, c("NULL", "trt", paste0(session, "_trt")))
        session_RE <- write_formula(input$RE_base, c("0","1", session))
        trt_RE <- write_formula(input$RE_trt, c("NULL","trt", paste0(session, "_trt")))
        if (studyDesign() %in% c("RMBB", "CMB")) {
          session_RE_2 <- write_formula(input$RE_base2, c("0","1", session))
          trt_RE_2 <- write_formula(input$RE_trt2, c("NULL","trt", paste0(session, "_trt")))
        }
      }
      
      fixed <- paste(outcome, "~", paste(c(session_FE, trt_FE), collapse = " + "))
      if (studyDesign() == "RMBB") {
        random <- paste0("list(", case, " = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                         ", ", series, " = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")
      } else if (studyDesign() == "CMB") {
        random <- paste0("list(",cluster, " = ~ ", paste(c(session_RE_2, trt_RE_2), collapse = " + "),
                         ", ", case, " = ~ ", paste(c(session_RE, trt_RE), collapse = " + "),")")
      } else {
        random <- paste("~", paste(c(session_RE, trt_RE), collapse = " + "), "|", case)
      }
      
      corr_struct <- switch(input$corStruct,
                            "AR(1)" = 
                              if (studyDesign() == "RMBB") {
                                paste0("\n               correlation = corAR1(0.01, ~ ", session, " | ", case," / ", series, "),")
                              } else if (studyDesign() == "CMB") {
                                paste0("\n               correlation = corAR1(0.01, ~ ", session, " | ", cluster, " / ", case, "),")
                              } else {
                                paste0("\n               correlation = corAR1(0.01, ~ ", session, " | ", case, "),")
                              },
                            "MA(1)" = 
                              if (studyDesign() == "RMBB") {
                                paste0("\n               correlation = corARMA(0, ~ ", session, " | ", case," / ", series, ", p = 0, q = 1),")
                              } else if (studyDesign() == "CMB") {
                                paste0("\n               correlation = corARMA(0, ~ ", session, " | ", cluster, " / ", case, ", p = 0, q = 1),")
                              } else {
                                paste0("\n               correlation = corARMA(0, ~ ", session, " | ", case, ", p = 0, q = 1),")
                              },
                            "IID" = "",
                            c())
      
      var_struct <- switch(input$varStruct,
                           "hom" = "",
                           "het" = paste0("\n               weights = varIdent(form = ~ 1 | ", phase, "),"),
                           c())
      
      fit_mod <- parse_code_chunk("fit-RML", args = list(user_fixed = fixed, 
                                                         user_random = random,
                                                         corr_struct = corr_struct,
                                                         var_struct = var_struct))
      
    } else {
      fit_mod <- c()
    }
    
    # Calculate effect size
    
    if (input$method == "RML") {
      center <- if (input$degree_base == 0 || is.null(input$model_center)) 0L else input$model_center
      A <- if (is.null(input$A_time)) 0L else input$A_time
      B <- if (is.null(input$B_time)) 1L else input$B_time
      p_const <- c(rep(0L, length(input$FE_base)), (B - A)^as.integer(input$FE_trt))
      
      # get r_const when centering at an arbitrary time instead of B
      r_dim <- length(input$RE_base) + length(input$RE_trt)
      r_const_dim <- r_dim * (r_dim + 1) / 2
      bc_vec <- (B - center)^as.integer(input$RE_base)
      bc_mat <- 2 * tcrossprod(bc_vec) - diag(bc_vec^2)
      r_const_base <- bc_mat[upper.tri(bc_mat, diag = TRUE)]
      r_const_trt <- rep(0L, (r_const_dim - length(r_const_base)))
      r_const_cor <- rep(0L, length(model_fit()$fit$modelStruct$corStruct))
      r_const_var <- rep(0L, length(model_fit()$fit$modelStruct$varStruct))
      if (studyDesign() %in% c("MBP", "TR")) {
        r_const <- c(r_const_base, r_const_trt, r_const_cor, r_const_var, 1L)
      } else {
        r_dim2 <- length(input$RE_base2) + length(input$RE_trt2)
        r_const_dim2 <- r_dim2 * (r_dim2 + 1) / 2
        bc_vec2 <- (B - center)^as.integer(input$RE_base2)
        bc_mat2 <- 2 * tcrossprod(bc_vec2) - diag(bc_vec2^2)
        r_const_base2 <- bc_mat2[upper.tri(bc_mat2, diag = TRUE)]
        r_const_trt2 <- rep(0L, r_const_dim2 - length(r_const_base2))
        r_const <- c(r_const_base, r_const_trt, r_const_base2, r_const_trt2, r_const_cor, r_const_var, 1L)
      }
      
      if (studyDesign() == "MBP") {
        calc_ES <- parse_code_chunk("es-RML-MB", args = list(user_A = A,
                                                          user_B = B,
                                                          user_FE_base = paste_object(input$FE_base), 
                                                          user_FE_trt = paste_object(input$FE_trt),
                                                          user_rconst_base = paste_object(r_const_base),
                                                          user_rconst_trt = paste_object(r_const_trt),
                                                          user_rconst_cor = paste_object(r_const_cor),
                                                          user_rconst_var = paste_object(r_const_var),
                                                          user_pconstant = paste_object(p_const),
                                                          user_rconstant= paste_object(r_const)))
      } else if (studyDesign() %in% c("RMBB", "CMB")) {
        calc_ES <- parse_code_chunk("es-RML-3level", args = list(user_A = A,
                                                             user_B = B,
                                                             user_FE_base = paste_object(input$FE_base), 
                                                             user_FE_trt = paste_object(input$FE_trt),
                                                             user_rconst_base = paste_object(r_const_base),
                                                             user_rconst_trt = paste_object(r_const_trt),
                                                             user_rconst_base2 = paste_object(r_const_base2),
                                                             user_rconst_trt2 = paste_object(r_const_trt2),
                                                             user_rconst_cor = paste_object(r_const_cor),
                                                             user_rconst_var = paste_object(r_const_var),
                                                             user_pconstant = paste_object(p_const),
                                                             user_rconstant= paste_object(r_const)))
      } else{
        calc_ES <- parse_code_chunk("es-RML-TR", args = list(user_rconst_base = paste_object(r_const_base),
                                                          user_rconst_trt = paste_object(r_const_trt),
                                                          user_rconst_cor = paste_object(r_const_cor),
                                                          user_rconst_var = paste_object(r_const_var),
                                                          user_pconstant = paste_object(p_const),
                                                          user_rconstant= paste_object(r_const)))
      }
      
    } else {
      if (studyDesign() == "MBP") {
        calc_ES <- parse_code_chunk("es-MB", args = list(user_case = case,
                                                         user_session = session,
                                                         user_outcome = outcome))
      } else {
        phase_pair <- paste0(phase, "_pair")
        calc_ES <- parse_code_chunk("es-ABK", args = list(user_case = case,
                                                          user_session = session,
                                                          user_outcome = outcome,
                                                          user_phase_pair = phase_pair))
      }
    }
    
    furtherArg_design <- switch(studyDesign(),
                                "RMBB" = paste0("series = ", series, ","),
                                "CMB" = paste0("cluster = ", cluster, ","),
                                "MBP" = "",
                                "TR" = "",
                                c())
    
    furtherArg_method <- switch(input$method,
                                "RML" = paste0("model_fit = fit_RML,"),
                                "HPS" = "",
                                c())
    
    SCD_graph <- parse_code_chunk("graph-scd", args = list(user_case = case,
                                                           user_phase = phase,
                                                           user_session = session,
                                                           user_outcome = outcome,
                                                           user_design = studyDesign(),
                                                           furtherArg_method = furtherArg_method,
                                                           furtherArg_design = furtherArg_design))

    res <- c(header_res, read_res, clean_dat, '', fit_mod, '', calc_ES, '', SCD_graph, '')
    paste(res, collapse = "\n")
  })
  
  
  output$syntax <- renderPrint({
    cat(ES_syntax(), sep = "\n")
  })
  
  output$clip <- renderUI({
    rclipboard::rclipButton("clipbtn", "Copy", ES_syntax(), modal = FALSE, icon("clipboard"))
  })
  
  session$onSessionEnded(stopApp)
    
})
