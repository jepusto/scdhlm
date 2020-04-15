library(testthat)

data("Anglesea")
data("AlberMorgan")



session_by_treatment <- function(x, trt_phase) {
  pmax(0, x$session - min(x$session[x$phase==trt_phase]))
}

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

#assume it is secondond level of  treatment facrot. 

graph_SCD <- function(case, phase, session, outcome, design, treatment_name = NULL,  data=NULL) {
  
  if (is.null(treatment_name)) { 
    treatment_name <-  levels(as.factor(dat$phase))[2]
  }
  
  if (!is.null(data)) {
    outcome_call <- substitute(outcome)
    phase_call <- substitute(phase)
    case_call <- substitute(case)
    session_call <- substitute(session)
    treatment_name_call <- substitute(treatment_name)
    
    env <- list2env(data, parent = parent.frame())
    
    outcome <- eval(outcome_call, env)
    phase <- eval(phase_call, env)
    case <- eval(case_call, env)
    session <- eval(session_call, env)
    treatment_name <- eval(treatment_name_call, env)
  }
  
  
  
  dat <- data.frame(case = factor(case),
                    phase = factor(phase),
                    session_fac = factor(session),
                    outcome, session)
  
  
  
  
  trt_phase <- treatment_name
  dat$trt <- as.numeric(dat$phase==trt_phase)
  phase_line_dat <- phase_lines_by_case(dat)
  
  if (design == "MB") {
    dat$session_trt <- unlist(by(dat, dat$case, session_by_treatment, trt_phase = trt_phase))
  } else {
    dat$phase_pair <- unlist(by(dat, dat$case, phase_pairs))
  }
  
  
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

test_that("graph is a ggplot2 graph", {
  
  my_graph <- graph_SCD(case=case, phase=condition, session=session, outcome=outcome, design="TR", treatment_name = NULL,  data=Anglesea)
  expect_s3_class(my_graph, "ggplot")
  
})

