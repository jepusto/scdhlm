library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)
library(readxl)

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
                   stringsAsFactors = FALSE)
        
      } else if (input$dat_type == "xlsx") {
        
        inFile <- input$xlsx
        
        if (is.null(inFile) || is.null(input$inSelect) || nchar(input$inSelect) == 0) return(NULL)
        
        as.data.frame(readxl::read_xlsx(inFile$datapath, col_names = input$col_names,
                                sheet = input$inSelect))
        
      } else if (input$dat_type == "loaded") {
        dataset
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
        selectInput("caseID", label = "Case identifier", choices = var_names, selected = var_names[n_var - 3]),
        selectInput("phaseID", label = "Phase identifier", choices = var_names, selected = var_names[n_var - 2]),
        selectInput("session", label = "Session number", choices = var_names, selected = var_names[n_var - 1])
      )
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
      
      if (!is.numeric(sessions)) {
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
        names(dat) <- c("case","session","phase","outcome")
        trt_phase <- levels(as.factor(dat$phase))[2]
      } else {
        
        case_vec <- datFile()[,input$caseID]
        caseID <- factor(case_vec, levels = unique(case_vec))
        
        session <- as.numeric(datFile()[,input$session])
        if (is.null(input$round_session)) {
          session <- as.integer(session) 
        } else if (input$round_session) {
          session <- as.integer(round(session))
        }
        
        phase_vec <- datFile()[,input$phaseID]
        phaseID <- factor(phase_vec, levels = unique(phase_vec))
        
        outcome <- as.numeric(datFile()[,input$outcome])
        
        dat <- data.frame(case = caseID, session = session, phase = phaseID, outcome = outcome)
        
        if (!is.null(input$filters)) {
          subset_vals <- sapply(input$filters, function(x) datFile()[[x]] %in% input[[paste0("filter_",x)]])
          dat <- dat[apply(subset_vals, 1, all),]
        } 
        
        trt_phase <- input$treatment
        
        # remove rows with missing outcome values
        dat <- dat[!is.na(dat$outcome),]
      }
      
      dat$trt <- as.numeric(dat$phase==trt_phase)
      
      if (studyDesign() == "MB") {
        dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
      } else {
        dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
      }
      
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
                             input$FE_trt, input$RE_trt, datClean()$case)
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
  
  
  #------------------------------
  # Syntax for replication in R
  #------------------------------
  
  parse_code_chunk <- function(chunk, args) {
    raw_code <- readLines(paste0("inst/shiny-examples/scdhlm/code-chunks/", chunk, ".R"))
    code_chunk <- paste(raw_code, collapse = "\n")
    glue::glue_data(.x = args, code_chunk)
  }

  output$syntax <- renderPrint({
    cat('setwd("") # paste path to folder containing data between quotes. Use forward slash: /', sep = "\n")
    cat("\n")
    cat('# Load packages', sep = "\n")
    cat('library(nlme)', sep = "\n")
    cat('library(scdhlm)', sep = "\n")
    cat("\n")
    
    # read in file code
    
    if(input$dat_type == "example") {
      cat(parse_code_chunk("load-example", args = list(example_name = input$example)))
      cat("\n")

    } else if(input$dat_type == "dat"){
      inFile <- input$dat
      cat(parse_code_chunk("load-dat", args = list(user_path = inFile$name, user_header = input$header, 
                                                   user_sep = input$sep, user_quote = input$quote)))
      cat("\n")

    } else if (input$dat_type == "xlsx") {
      inFile <- input$xlsx
      cat(parse_code_chunk("load-excel", args = list(user_path = inFile$name, user_sheet = input$inSelect)))
      cat("\n")
      
    } else {
      cat(parse_code_chunk("load-from-function", args = NULL))
      cat("\n")
    }

    # Clean the data
    
    if (input$dat_type == "example") {

      example_parms <- exampleMapping[[input$example]]
      filter_vars <- example_parms$filters

      if (!is.null(filter_vars)) {
        cat("\n")
        cat(parse_code_chunk("clean-example-filter", 
                             args = list(user_filtervars = paste(filter_vars, collapse='", "'), 
                                         user_parms = paste(example_parms$vars, collapse='", "'))))
        cat("\n")
      }
      cat("\n")
      cat(parse_code_chunk("clean-example-nofilter", 
                           args = list(user_parms = paste(example_parms$vars, collapse='", "'))))
      cat("\n")

      case <- "case"
      session <- "session"
      phase <- "phase"
      outcome <- "outcome"
      cat("\n")

    } else {

      if (!is.null(input$filters)) {
        cat("\n")
        cat(parse_code_chunk("clean-inputdata-filter", args = list(user_caseID = input$caseID, 
                                                                   user_session = input$session, 
                                                                   user_phaseID = input$phaseID , 
                                                                   user_outcome = input$outcome, 
                                                                   user_treatment = input$treatment, 
                                                                   user_filtervars = paste(input$filters, collapse='", "'))))
        cat("\n")
      } else {
        cat("\n")
        cat(parse_code_chunk("clean-inputdata-nofilter", args = list(user_caseID = input$caseID, 
                                                                     user_session = input$session, 
                                                                     user_phaseID = input$phaseID , 
                                                                     user_outcome = input$outcome, 
                                                                     user_treatment = input$treatment)))
        cat("\n")
      }
      
      case <- input$caseID
      session <- input$session
      phase <- input$phaseID
      outcome <- input$outcome
      cat("\n")

      if (is.null(input$round_session)) {
        cat(parse_code_chunk("session_noround", args = list(user_session = input$session)))
        cat("\n")
      } else if (input$round_session) {
        cat(parse_code_chunk("session_round", args = list(user_session = input$session)))
        cat("\n")
      }

      cat("\n")
    }

    if (studyDesign() == "MB") {
      cat(parse_code_chunk("clean-MB", args = list(user_case = case, 
                                                   user_session = session, 
                                                   user_phase = phase, 
                                                   user_model_center = input$model_center)))
      cat("\n")
    } else {
      cat(parse_code_chunk("clean-TR", args = list(user_case = case)))
      cat("\n")
    }

    # Fit the model

    if (input$method=="RML") {

      if (studyDesign() == "MB") {
        session_FE <- write_formula(input$FE_base, c("0","1","session"))
        trt_FE <- write_formula(input$FE_trt, c("NULL", "trt", "session_trt"))
        session_RE <- write_formula(input$RE_base, c("0","1","session"))
        trt_RE <- write_formula(input$RE_trt, c("NULL","trt","session_trt"))

      } else {
        session_FE <- if (is.null(input$FE_base) | !(0 %in% input$FE_base)) "0" else "1"
        trt_FE <- if (is.null(input$FE_trt) | !(0 %in% input$FE_trt)) NULL else "trt"
        session_RE <- if (is.null(input$RE_base) | !(0 %in% input$RE_base)) "0" else "1"
        trt_RE <- if (is.null(input$RE_trt) | !(0 %in% input$RE_trt)) NULL else "trt"
      }

      fixed <- paste(outcome, "~",paste(c(session_FE, trt_FE), collapse = " + "))
      random <- paste("~ ", paste(c(session_RE, trt_RE), collapse = " + "), "| ", case)
      
      cat("\n")
      cat(parse_code_chunk("fit-RML", args = list(user_fixed = fixed, user_random = random, 
                                                  user_case = case, user_session = session)))
      cat("\n")
    
    }
    
    # Calculate effect size
    
    if (input$method == "RML") {
      A <- if (is.null(input$A_time)) 0L else input$A_time
      B <- if (is.null(input$B_time)) 1L else input$B_time
      p_const <- c(rep(0L, length(input$FE_base)), (B - A - 1)^as.integer(input$FE_trt))
      r_dim <- length(input$RE_base) + length(input$RE_trt)
      r_const <- c(as.integer(0 %in% input$RE_base),
                   rep(0, r_dim * (r_dim + 1) / 2 - 1),
                   rep(0, length(model_fit()$fit$modelStruct$corStruct)),
                   rep(0, length(model_fit()$fit$modelStruct$varStruct)),
                   1L)
      
      cat("\n")
      cat(parse_code_chunk("es-RML", args = list(user_pconstant = paste("c(", paste(p_const, collapse = ","), ")", sep = ""),
                                                 user_rconstant= paste("c(", paste(r_const, collapse = ","), ")", sep = ""))))
      cat("\n")
      
    } else {
      if (studyDesign() == "MB") {
        cat("\n")
        cat(parse_code_chunk("es-MB", args = list(user_outcome = outcome, user_case = case, user_session = session)))
        cat("\n")
      } else {
        cat("\n")
        cat(parse_code_chunk("es-ABK", args = list(user_outcome = outcome, user_case = case, user_session = session)))
        cat("\n")
      }
    }
    
    cat("\n")
    cat(parse_code_chunk("graph", args = list(user_design = studyDesign(), user_case = case, 
                                              user_phase = phase, user_session = session,  
                                              user_outcome = outcome)))
    cat("\n")

  })
    session$onSessionEnded(stopApp)
    
})
