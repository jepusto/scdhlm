
graph_SCD <- function(dat, design) {
  phase_line_dat <- phase_lines_by_case(dat)
  
  if (design=="MB") {
    p <- ggplot(dat, aes(session, outcome, color = as.factor(phase), shape = as.factor(phase)))  
  } else {
    p <- ggplot(dat, 
           aes(session, outcome, 
               color = phase, shape = phase,
               group = interaction(phase, phase_pair)))
  }
  
  p + 
    geom_point() + 
    geom_line() + 
    facet_grid(case ~ .) + 
    theme_bw() + 
    labs(color = "", shape = "") + 
    geom_vline(data = phase_line_dat, aes(xintercept = phase_time), linetype = "dashed")
}

# input <- list(example = "Saddler")
# data(list = input$example)
# dat <- get(input$example)
# example_parms <- exampleMapping[[input$example]]
# filter_vars <- example_parms$filters
# input$filter_measure = 1
# if (!is.null(filter_vars)) {
#   subset_vals <- sapply(filter_vars, function(x) dat[[x]] %in% input[[paste0("filter_",x)]])
#   dat <- dat[apply(subset_vals, 1, all),]
# }
# dat <- dat[,example_parms$vars]
# names(dat) <- c("case","session","phase","outcome")
# design <- example_parms$design
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# if (design == "MB") {
#   dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
# } else {
#   dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
# }
# 
# default_times(dat)
# 
# graph_SCD(dat, design)
