
#---------------------------------------------------------------
# calculate session-by-treatment interaction
#---------------------------------------------------------------

session_by_treatment <- function(x, trt_phase) {
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}


#---------------------------------------------------------------
# calculate phase-pairs based on phases and session numbering
#---------------------------------------------------------------

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


#---------------------------------------------------------------
# calculate phase border times
#---------------------------------------------------------------

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


#---------------------------------------------------------------
# calculate timing defaults
#---------------------------------------------------------------

default_times <- function(x) {
  range <- range(x$session)
  case_base_last <- with(x, tapply(session[trt==0], case[trt==0], max))
  case_trt_range <- with(x, tapply(session[trt==1], case[trt==1], function(x) diff(range(x))))
  A <- min(case_base_last)
  B <- A + min(case_trt_range[which(case_base_last == min(case_base_last))])
  list(range = range, A = A, B = B)
}
