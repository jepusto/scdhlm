
output$RCT_plot <- renderPlot({
  if (studyDesign()=="MB" & input$method=="RML" & "lme" %in% class(model_fit()$fit)) {
    range <- time_range()$range
    A <- input$A_time
    B <- input$B_time
    cases <- levels(datClean()$case)
    sessions <- seq(from = range[1], to = range[2])
    dat_RCT <- data.frame(case = rep(cases, each = length(sessions)),
                          session = sessions,
                          trt = as.integer(sessions > A),
                          session_trt = ifelse(sessions > A, sessions - A - 1, 0))
    dat_RCT$phase <- levels(datClean()$phase)[dat_RCT$trt + 1]
    graph_SCD(dat_RCT, design = studyDesign())
  }
}, height = function() 120 * nlevels(datClean()$case),
width = function() 700)
