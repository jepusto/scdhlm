library(shiny)
library(markdown)
library(ggplot2)
library(scdhlm)
library(readxl)
library(janitor)

install_rstan <- requireNamespace("rstan", quietly = TRUE) && requireNamespace("StanHeaders", quietly = TRUE)

if (install_rstan && 
    packageVersion("rstan") >= "2.26.22" &&
    packageVersion("StanHeaders") >= "2.26.27") {

  estimation_names <- c("Moment estimation" = "HPS",
                        "Restricted Maximum Likelihood" = "RML",
                        "Bayesian estimation (Markov Chain Monte Carlo)" = "Bayes")
  library(brms)
  
} else {
  estimation_names <- c("Moment estimation" = "HPS",
                        "Restricted Maximum Likelihood" = "RML")
}

source("mappings.R", local = TRUE)
source("helper-functions.R", local = TRUE)

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
          names(dat) <- c("studyID","school", "cluster", "case", "phase", "session", "session_trt", "outcome", "session_c")
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
        estimation_names[estimation_names != "HPS"]
      }
      
      selectInput("method", label = "Estimation method",
                  choices = estimation_choices, 
                  selected = "RML",
                  width = '400px')
    })
    
    # Centering and timing sliders for RML estimation of MBD
    
    time_range <- reactive({
      
      cluster <- if (studyDesign() == "CMB") substitute(cluster) else NULL
      series <- if (studyDesign() == "RMBB") substitute(series) else NULL
      default_times(
        design = studyDesign(),
        case = case, phase = phase, session = session,
        cluster = cluster, series = series,
        treatment_name = NULL, data = datClean()
      )
      
    })
    
    output$model_centering2 <- output$model_centering <- renderUI({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method %in% c("RML", "Bayes")) {
        session_range <- time_range()$range
        sliderInput("model_center", "Center session at", 
                    min=session_range[1], max=session_range[2], 
                    value=time_range()$B, step = 1)
      }
    })
    
    output$ES_timing_message <- renderText({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method %in% c("RML", "Bayes") &
          any(input$degree_trt != 0, (input$degree_base != 0 & input$RE_base > 0))) {
        note_txt <- "Note: Options for selecting an initial treatment time and follow-up time can be modified on the next page." 
        HTML(note_txt)
      }
    })
    
    output$ES_timing <- renderUI({
      if (studyDesign() %in% c("MBP", "RMBB", "CMB") & input$method %in% c("RML", "Bayes") & 
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
    
    # Fit RML or Bayesian models
    
    model_fit <- eventReactive(input$runModel, {
      
      # fit the model
      if (studyDesign() %in% c("MBP", "TR")) {
        RE_base2 <- RE_trt2 <- NULL
        dat <- datClean()[order(datClean()$case, datClean()$session),]
        cluster <- series <- NULL
      } else {
        RE_base2 <- input$RE_base2
        RE_trt2 <- input$RE_trt2
        if (studyDesign() == "RMBB") {
          dat <- datClean()[order(datClean()$case, datClean()$series, datClean()$session),]
          cluster <- NULL
          series <- dat$series
        } else {
          dat <- datClean()[order(datClean()$cluster, datClean()$case, datClean()$session),]
          cluster <- dat$cluster
          series <- NULL
        }
      }
      
      center <- if (input$degree_base == 0 || is.null(input$model_center)) 0L else input$model_center
      
      if (input$method == "RML") {
        res <- 
        calc_BCSMD(
          design = studyDesign(),
          case = dat$case, phase = dat$phase, session = dat$session, outcome = dat$outcome,
          cluster = cluster, series = series,
          center = center,
          FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2,
          FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
          corStruct = input$corStruct, varStruct = input$varStruct,
          summary = FALSE
        )
        
      } else if (input$method == "Bayes") {
        
        if (input$bshow_advOpts == TRUE) {
          seed <- input$badvOpts_seed
          cores <- input$badvOpts_cores
          chains <- input$badvOpts_chains
          iter <- input$badvOpts_iter
          warmup <- input$badvOpts_warmup
          thin <- input$badvOpts_thin
        } else {
          seed <- NA
          cores <- 1L
          chains <- 4L
          iter <- 2000L
          warmup <- 1000L
          thin <- 10L
        }
        
        run_mssg <- paste(
          "Stan will now compile the C++ code for your model (which may take a while) and will then start sampling."
        )
        
        showNotification(run_mssg, duration = 60, type = "message")
        
        res <- 
        calc_BCSMD(
          design = studyDesign(),
          case = dat$case, phase = dat$phase, session = dat$session, outcome = dat$outcome,
          cluster = cluster, series = series,
          center = center,
          FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2,
          FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
          corStruct = input$corStruct, varStruct = input$varStruct,
          Bayesian = TRUE, seed = seed, cores = cores, 
          chains = chains, iter = iter, warmup = warmup, thin = thin,
          summary = FALSE
        )
        
        if (isTRUE(res$converged)) {
          showNotification(
            paste("Stan results obtained."),
            duration = NA,
            type = "message"
          )
        } else {
          showNotification(
            paste("Warning: Stan results obtained, but at least one MCMC diagnostic is worrying. In general,",
                  "this indicates that the Stan results should not be used."),
            duration = NA,
            type = "warning"
          )
        }
        
      }
      
      return(res)
      
    })
    
    
    # Model spec output for RML
    
    output$model_sample_size <- renderTable({
      if (input$method == "RML") {
        mf_model <- model_fit()$model
        if (studyDesign() %in% c("MBP", "TR")) {
          data.frame("Total number of observations" = nobs(mf_model), 
                     "Total number of groups" = nlevels(summary(mf_model)$groups$case),
                     check.names = FALSE) 
        } else if (studyDesign() == "CMB") {
          n_groups <- summary(mf_model)$groups
          data.frame("Total number of observations" = nobs(mf_model), 
                     "Total number of cases" = nlevels(n_groups$case),
                     "Total number of clusters" = nlevels(n_groups$cluster),
                     check.names = FALSE)
        } else if (studyDesign() == "RMBB") {
          n_groups <- summary(mf_model)$groups
          data.frame("Total number of observations" = nobs(mf_model),
                     "Total number of series" = nlevels(n_groups$series),
                     "Total number of cases" = nlevels(n_groups$case),
                     check.names = FALSE)
        }
      }
    }, 
    caption = "Sample sizes", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$model_fit_fixed <- renderTable({
      if (input$method == "RML") {
        summary(model_fit()$model)$tTable
      }
    }, 
    caption = "Fixed effects", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = TRUE)
    
    output$model_fit_random <- renderTable({
      if (input$method == "RML") {
        random_table <- data.frame(VarCorr(model_fit()$model)[])
        rownames <- rownames(VarCorr(model_fit()$model)[])
        
        # set the rownames
        if (studyDesign() %in% c("CMB", "RMBB")) {
          row_index_lvl3 <- length(input$RE_base2)+length(input$RE_trt2)+1
          rownames_lvl3 <- c("", rownames[2:row_index_lvl3])
          rownames_lvl2 <- c("", rownames[(row_index_lvl3 + 2):(length(rownames))])
          name_lvl3 <- if (studyDesign() == "CMB") "<strong>Cluster-level</strong>" else "<strong>Case-level</strong>"
          name_lvl2 <- if (studyDesign() == "CMB") "<strong>Case-level</strong>" else "<strong>Series-level</strong>"
          random_levels <- c(name_lvl3, rep("", row_index_lvl3-1), name_lvl2, rep("", (length(rownames)-row_index_lvl3-1)))
          random_table[1, 1] <- NA # get rid of pdLogChol
          random_table[(row_index_lvl3 + 1), 1] <- NA
          random_table <- cbind("Level" = random_levels, "Rowname" = c(rownames_lvl3, rownames_lvl2), random_table)
          random_table[, 3:4] <- lapply(random_table[, 3:4], function(x) as.numeric(x))
          names(random_table)[2] <- ""
        } else if (studyDesign() %in% c("MBP", "TR")) {
          random_table <- cbind("Rowname" = rownames, random_table)
          random_table[, 2:3] <- lapply(random_table[, 2:3], function(x) as.numeric(x))
          names(random_table)[1] <- ""
        }
        
        # set the column names
        columnnames <- names(random_table)
        names(random_table) <- gsub("V[0-9]|Var.[0-9]", "", columnnames)
        random_table
        
      }
    },
    caption = "Random effects",
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, na = "", include.rownames = FALSE, sanitize.text.function = function(x){x})
    
    output$model_fit_corr <- renderTable({
      if (input$method=="RML" & input$corStruct != "IID") {
        data.frame("Correlation parameter" = model_fit()$phi, check.names = FALSE)
      }
    }, 
    caption = "Correlation structure", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$model_fit_var <- renderTable({
      if (input$method == "RML" & input$varStruct == "het") {
        data.frame("Baseline" = 1, "Treatment" = model_fit()$var_param)
      }
    }, 
    caption = "Variance structure", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$model_info <- renderTable({
      if (input$method == "RML") {
        mf_model <- model_fit()$model
        data.frame("AIC" = AIC(mf_model), 
                   "BIC" = BIC(mf_model), 
                   "Log likelihood" = as.numeric(mf_model$logLik),
                   check.names = FALSE) 
      }
    }, 
    caption = "Information criteria", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$model_fit_convg <- renderTable({
      if (input$method=="RML") {
        if (isTRUE(model_fit()$converged)) {
          "The model converged."
        } else {
          "The model did not converge."
        }
      }
    }, colnames = FALSE)
    
    
    # Model spec output for Bayes
    
    output$Bayes_sample_size <- renderTable({
      if (input$method == "Bayes") {
        mf_model <- summary(model_fit()$model)
        if (studyDesign() %in% c("MBP", "TR")) {
          data.frame("Total number of observations" = mf_model$nobs, 
                     "Total number of groups" = mf_model$ngrps$case,
                     check.names = FALSE) 
        } else if (studyDesign() == "CMB") {
          data.frame("Total number of observations" = mf_model$nobs, 
                     "Total number of cases" = mf_model$ngrps$`cluster:case`,
                     "Total number of clusters" = mf_model$ngrps$cluster,
                     check.names = FALSE)
        } else if (studyDesign() == "RMBB") {
          data.frame("Total number of observations" = mf_model$nobs,
                     "Total number of series" = mf_model$ngrps$`case:series`,
                     "Total number of cases" = mf_model$ngrps$ngrps$case,
                     check.names = FALSE)
        }
      }
    }, 
    caption = "Sample sizes", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$Bayes_fit_fixed <- renderTable({
      if (input$method == "Bayes") {
        fixed_table <- summary(model_fit()$model)$fixed
        if (input$varStruct == "hom") {
          return(fixed_table)
        } else {
          return(fixed_table[2:(nrow(fixed_table) - 1), ])
        }
      }
    }, 
    caption = "Fixed effects", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = TRUE)
    
    output$Bayes_fit_random <- renderTable({
      if (input$method == "Bayes") {
        random_list <- summary(model_fit()$model)$random
        random_table <- do.call(rbind, random_list)
        if (input$varStruct == "hom") {
          sigma <- summary(model_fit()$model)$spec_pars
          return(rbind(random_table, sigma))
        } else {
          return(random_table)
        }
      }
    },
    caption = "Random effects",
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, na = "", include.rownames = TRUE)
    
    output$Bayes_fit_corr <- renderTable({
      if (input$method=="Bayes" & input$corStruct != "IID") {
        data.frame("Correlation parameter" = model_fit()$phi, check.names = FALSE)
      }
    }, 
    caption = "Correlation structure", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$Bayes_fit_var <- renderTable({
      if (input$method == "Bayes" & input$varStruct == "het") {
        data.frame("Baseline" = 1, "Treatment" = model_fit()$var_param)
      }
    }, 
    caption = "Variance structure", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)
    
    output$Bayes_info <- renderTable({
      if (input$method == "Bayes") {
        data.frame("LOOIC" = loo(model_fit()$model)$looic, 
                   "WAIC" = waic(model_fit()$model)$waic,
                   check.names = FALSE) 
      }
    }, 
    caption = "Information criteria", 
    caption.placement = getOption("xtable.caption.placement", "top"),
    digits = 4, include.rownames = FALSE)

    
    # Calculate effect sizes
    
    effect_size <- reactive({
      
      if (studyDesign() %in% c("MBP", "TR")) {
        RE_base2 <- RE_trt2 <- NULL
      } else {
        RE_base2 <- input$RE_base2
        RE_trt2 <- input$RE_trt2
      }
      
      if (input$method %in% c("RML", "Bayes")) {
        center <- if (input$degree_base == 0 || is.null(input$model_center)) 0L else input$model_center
        A <- if (is.null(input$A_time)) 0L else input$A_time
        B <- if (is.null(input$B_time)) 1L else input$B_time
        res <- calc_effect_size(model = model_fit(), design = studyDesign(), method = input$method, 
                               FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2,
                               FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
                               varStruct = input$varStruct, corStruct = input$corStruct,
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
      
      summarize_ES(res = res, 
                   filter_vals = filter_vals, 
                   design = studyDesign(), method = input$method, 
                   FE_base = input$FE_base, RE_base = input$RE_base, RE_base_2 = RE_base2, 
                   FE_trt = input$FE_trt, RE_trt = input$RE_trt, RE_trt_2 = RE_trt2,
                   corStruct = input$corStruct, varStruct = input$varStruct,
                   A = input$A_time, B = input$B_time,
                   coverage = input$coverage)
      
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
    if (inherits(model_fit()$model, "lme")) {
      cluster <- if (studyDesign() == "CMB") substitute(cluster) else NULL
      series <- if (studyDesign() == "RMBB") substitute(series) else NULL
      graph_SCD(design = studyDesign(), 
                cluster = cluster, case = case, series = series, 
                phase = phase, session = session, outcome = outcome, 
                model_fit = model_fit()$model)
    }
  }, height = function() 120 * nlevels(datClean()[[1]]),
  width = function() 700)
  
  
  # Bayes plots
  
  output$Bayes_dens <- renderPlot({
    if (inherits(model_fit()$model, "brmsfit")) {
      brms::mcmc_plot(model_fit()$model,type = "dens")
    }
  }, height = 700, width = 700)
  
  output$Bayes_ar <- renderPlot({
    if (inherits(model_fit()$model, "brmsfit")) {
      brms::mcmc_plot(model_fit()$model,type = "acf")
    }
  }, height = 700, width = 700)
  
  output$Bayes_trace <- renderPlot({
    if (inherits(model_fit()$model, "brmsfit")) {
      brms::mcmc_plot(model_fit()$model,type = "trace")
    }
  }, height = 700, width = 700)
  
  output$Bayes_rhat <- renderPlot({
    if (inherits(model_fit()$model, "brmsfit")) {
      brms::mcmc_plot(model_fit()$model,type = "rhat")
    }
  }, height = 700, width = 700)
  
  output$Bayes_overlaid <- renderPlot({
    if (inherits(model_fit()$model, "brmsfit")) {
      brms::pp_check(model_fit()$model, ndraws = 100)
    }
  }, height = 700, width = 700)
  
  
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

    # get values of several arguments
  
    if (input$degree_base == 0 || is.null(input$model_center)) {
      model_center <- 0
    } else {
      model_center <- input$model_center
    }
    
    FE_base <- paste0('c(', paste(input$FE_base, collapse=','), ')')
    
    RE_base <- paste0('c(', paste(input$RE_base, collapse=','), ')')
    
    RE_base2 <- if (is.null(input$RE_base2)) "NULL" else 
      paste0('c(', paste(input$RE_base2, collapse=','), ')')
    
    FE_trt <- paste0('c(', paste(input$FE_trt, collapse=','), ')')
    
    RE_trt <- if (is.null(input$RE_trt)) "NULL" else 
      paste0('c(', paste(input$RE_trt, collapse=','), ')')
    
    RE_trt2 <- if (is.null(input$RE_trt2)) "NULL" else 
      paste0('c(', paste(input$RE_trt2, collapse=','), ')')
    
    A <- if (is.null(input$A_time)) 0L else input$A_time
    B <- if (is.null(input$B_time)) 1L else input$B_time
    
    if (input$bshow_advOpts == TRUE) {
      seed <- input$badvOpts_seed
      cores <- input$badvOpts_cores
      chains <- input$badvOpts_chains
      iter <- input$badvOpts_iter
      warmup <- input$badvOpts_warmup
      thin <- input$badvOpts_thin
    } else {
      seed <- NA
      cores <- 1L
      chains <- 4L
      iter <- 2000L
      warmup <- 1000L
      thin <- 10L
    }
    
    # calculate effect size using calc_BCSMD
    
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
      cluster <- if (studyDesign() == "CMB") "cluster" else "NULL"
      series <- if (studyDesign() == "RMBB") "series" else "NULL"
      param_args <- switch(studyDesign(),
                                "RMBB" = paste(example_parms$vars[2:6], collapse='", "'),
                                "CMB" = paste(example_parms$vars[-c(1,2,7,9)], collapse='", "'),
                                "MBP" = paste(example_parms$vars, collapse='", "'),
                                "TR" = paste(example_parms$vars, collapse='", "'),
                                c())
      var_names <- switch(studyDesign(),
                         "RMBB" = paste(c("case","series","outcome","session","phase"), collapse='", "'),
                         "CMB" = paste(c("cluster","case","phase","session","outcome"), collapse='", "'),
                         "MBP" = paste(c("case","session","phase","outcome"), collapse='", "'),
                         c())
      
      if (input$method == "RML") {
        
        if (studyDesign() == "TR") {
          es_res <- c(clean_dat_A,
                         '',
                         parse_code_chunk("es-RML-example-TR",
                                          args = list(user_parms = param_args,
                                                      user_design = studyDesign(),
                                                      user_corStruct = input$corStruct,
                                                      user_varStruct = input$varStruct))
          )
        } else if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
          es_res <- c(clean_dat_A,
                         '',
                         parse_code_chunk("es-RML-example-MBs",
                                          args = list(user_parms = param_args,
                                                      user_varnames = var_names,
                                                      user_design = studyDesign(),
                                                      user_cluster = cluster,
                                                      user_series = series,
                                                      user_model_center = model_center,
                                                      user_FE_base = FE_base,
                                                      user_RE_base = RE_base,
                                                      user_RE_base2 = RE_base2,
                                                      user_FE_trt = FE_trt,
                                                      user_RE_trt = RE_trt,
                                                      user_RE_trt2 = RE_trt2,
                                                      user_corStruct = input$corStruct,
                                                      user_varStruct = input$varStruct,
                                                      user_A = A,
                                                      user_B = B))
          )
        }
        
      } else if (input$method == "Bayes") {
        
        if (studyDesign() == "TR") {
          es_res <- c(clean_dat_A,
                         '',
                         parse_code_chunk("es-Bayes-example-TR",
                                          args = list(user_parms = paste(example_parms$vars, collapse='", "'),
                                                      user_design = studyDesign(),
                                                      user_corStruct = input$corStruct,
                                                      user_varStruct = input$varStruct,
                                                      user_chains = chains,
                                                      user_iter = iter,
                                                      user_warmup = warmup,
                                                      user_thin = thin,
                                                      user_cores = cores,
                                                      user_seed = seed))
          )
        } else if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
          es_res <- c(clean_dat_A,
                         '',
                         parse_code_chunk("es-Bayes-example-MBs",
                                          args = list(user_parms = param_args,
                                                      user_varnames = var_names,
                                                      user_design = studyDesign(),
                                                      user_cluster = cluster,
                                                      user_series = series,
                                                      user_model_center = model_center,
                                                      user_FE_base = FE_base,
                                                      user_RE_base = RE_base,
                                                      user_RE_base2 = RE_base2,
                                                      user_FE_trt = FE_trt,
                                                      user_RE_trt = RE_trt,
                                                      user_RE_trt2 = RE_trt2,
                                                      user_corStruct = input$corStruct,
                                                      user_varStruct = input$varStruct,
                                                      user_chains = chains,
                                                      user_iter = iter,
                                                      user_warmup = warmup,
                                                      user_thin = thin,
                                                      user_cores = cores,
                                                      user_seed = seed,
                                                      user_A = A,
                                                      user_B = B))
          )
        }
        
      } else {
        
        if (studyDesign() == "MBP") {
          es_res <- parse_code_chunk("es-MB", args = list(user_case = case,
                                                          user_session = session,
                                                          user_outcome = outcome))
        } else {
          phase_pair <- paste0(phase, "_pair")
          es_res <- parse_code_chunk("es-ABK", args = list(user_case = case,
                                                           user_session = session,
                                                           user_outcome = outcome,
                                                           user_phase_pair = phase_pair))
        } 
        
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
        
        if (input$method == "RML") {
          es_res <- c(clean_dat_B,
                      '',
                      parse_code_chunk("es-RML-inputdata-TR",
                                       args = list(user_caseID = case,
                                                   user_session = session,
                                                   user_phaseID = phase,
                                                   user_outcome = outcome,
                                                   user_design = studyDesign(),
                                                   user_treatment = input$treatment,
                                                   user_round = round_session,
                                                   user_corStruct = input$corStruct,
                                                   user_varStruct = input$varStruct))
          )
        } else if (input$method == "Bayes") {
          es_res <- c(clean_dat_B,
                       '',
                       parse_code_chunk("es-Bayes-inputdata-TR", 
                                        args = list(user_caseID = case, 
                                                    user_session = session,
                                                    user_phaseID = phase,
                                                    user_outcome = outcome,
                                                    user_design = studyDesign(),
                                                    user_treatment = input$treatment,
                                                    user_round = round_session,
                                                    user_corStruct = input$corStruct,
                                                    user_varStruct = input$varStruct,
                                                    user_chains = chains,
                                                    user_iter = iter,
                                                    user_warmup = warmup,
                                                    user_thin = thin,
                                                    user_cores = cores,
                                                    user_seed = seed))
          )
        }
          
      } else if (studyDesign() %in% c("MBP", "RMBB", "CMB")) {
        
        if (input$method == "RML") {
          
          es_res <- c(clean_dat_B,
                       '',
                       parse_code_chunk("es-RML-inputdata-MBs", 
                                        args = list(user_design = studyDesign(),
                                                    user_caseID = case,
                                                    user_phaseID = phase,
                                                    user_session = session,
                                                    user_outcome = outcome,
                                                    user_clusterID = cluster,
                                                    user_seriesID = series,
                                                    user_model_center = model_center,
                                                    user_round = round_session,
                                                    user_treatment = input$treatment,
                                                    user_FE_base = FE_base,
                                                    user_RE_base = RE_base,
                                                    user_RE_base2 = RE_base2,
                                                    user_FE_trt = FE_trt,
                                                    user_RE_trt = RE_trt,
                                                    user_RE_trt2 = RE_trt2,
                                                    user_corStruct = input$corStruct,
                                                    user_varStruct = input$varStruct,
                                                    user_A = A,
                                                    user_B = B))
          )
          
        } else if (input$method == "Bayes") {
          
          es_res <- c(clean_dat_B,
                       '',
                       parse_code_chunk("es-Bayes-inputdata-MBs", 
                                        args = list(user_design = studyDesign(),
                                                    user_caseID = case,
                                                    user_phaseID = phase,
                                                    user_session = session,
                                                    user_outcome = outcome,
                                                    user_clusterID = cluster,
                                                    user_seriesID = series,
                                                    user_model_center = model_center,
                                                    user_round = round_session,
                                                    user_treatment = input$treatment,
                                                    user_FE_base = FE_base,
                                                    user_RE_base = RE_base,
                                                    user_RE_base2 = RE_base2,
                                                    user_FE_trt = FE_trt,
                                                    user_RE_trt = RE_trt,
                                                    user_RE_trt2 = RE_trt2,
                                                    user_corStruct = input$corStruct,
                                                    user_varStruct = input$varStruct,
                                                    user_chains = chains,
                                                    user_iter = iter,
                                                    user_warmup = warmup,
                                                    user_thin = thin,
                                                    user_cores = cores,
                                                    user_seed = seed,
                                                    user_A = A,
                                                    user_B = B))
          )
        }
      }
    }
    
    # Graphing: NEED TO BE UPDATED
    
    furtherArg_design <- switch(studyDesign(),
                                "RMBB" = paste0("series = ", series, ","),
                                "CMB" = paste0("cluster = ", cluster, ","),
                                "MBP" = "",
                                "TR" = "",
                                c())
    
    furtherArg_method <- switch(input$method,
                                "RML" = paste0("model_fit = res$model"),
                                "Bayes" = paste0("model_fit = res$model"),
                                "HPS" = "",
                                c())
    
    SCD_graph <- parse_code_chunk("graph-scd", args = list(user_case = case,
                                                           user_phase = phase,
                                                           user_session = session,
                                                           user_outcome = outcome,
                                                           user_design = studyDesign(),
                                                           furtherArg_method = furtherArg_method,
                                                           furtherArg_design = furtherArg_design))

    res <- c(header_res, read_res, es_res, '', SCD_graph, '')
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
