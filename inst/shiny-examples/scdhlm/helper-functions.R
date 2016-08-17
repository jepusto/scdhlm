
#---------------------------------------------------------------
# calculate session-by-treatment interaction
#

session_by_treatment <- function(x, trt_phase) {
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}


# input <- list(example = "Schutte")
# data(list = input$example)
# dat <- get(input$example)
# example_parms <- exampleMapping[[input$example]]
# filter_vars <- example_parms$filters
# if (!is.null(filter_vars)) {
#   subset_vals <- sapply(filter_vars, function(x) dat[[x]] %in% input[[paste0("filter_",x)]])
#   dat <- dat[apply(subset_vals, 1, all),]
# }
# dat <- dat[,example_parms$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))

#---------------------------------------------------------------
# calculate phase-pairs based on phases and session numbering
#

phase_pairs <- function(x) {
  conditions <- levels(as.factor(x$phase))
  n <- length(x$phase)
  phase <- x$phase[order(x$session)]
  y <- rep(1,n)
  for (i in 2:n) {
    (i <- i + 1)
    (which_lev <- match(phase[i-1], conditions))
    (which_conditions <- conditions[c(which_lev, which_lev + 1)])
    !(phase[i] %in% which_conditions)
    (y[i] <- y[i - 1] + !(phase[i] %in% which_conditions))
  }
  y[order(order(x$session))]
}



# input <- list(example = "Anglesea")
# data(list = input$example)
# dat <- get(input$example)
# example_parms <- exampleMapping[[input$example]]
# design <- example_parms$design
# filter_vars <- example_parms$filters
# if (!is.null(filter_vars)) {
#   subset_vals <- sapply(filter_vars, function(x) dat[[x]] %in% input[[paste0("filter_",x)]])
#   dat <- dat[apply(subset_vals, 1, all),]
# }
# dat <- dat[,example_parms$vars]
# names(dat) <- c("case","session","phase","outcome")
# trt_phase <- levels(as.factor(dat$phase))[2]
# dat$trt <- as.numeric(dat$phase==trt_phase)
# dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
# dat


#---------------------------------------------------------------
# calculate phase border times
#

phase_lines <- function(x) {
  phase <- x$phase[order(x$session)]
  n <- length(phase)
  switches <- which(phase[2:n] != phase[1:(n-1)])
  (x$session[switches] + x$session[switches + 1]) / 2
}

phase_lines_by_case <- function(x) {
  phase_line <- by(x, x$case, phase_lines)
  data.frame(case = rep(names(phase_line), lengths(phase_line)), phase_time = as.vector(unlist(phase_line)))
}