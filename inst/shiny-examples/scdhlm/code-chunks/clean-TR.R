#function for phase pairs
cat(phase_pairs <- function(x) {
    conditions <- levels(as.factor(x${user_phase}))
    n <- length(x${user_phase})
    phase <- x${user_phase}[order(x${user_session})]
    y <- rep(1,n)
    for (i in 2:n) {
        (i <- i + 1)
        (which_lev <- match(phase[i-1], conditions))
        (which_conditions <- conditions[c(which_lev, which_lev + 1)])
        !(phase[i] %in% which_conditions)
        (y[i] <- y[i - 1] + !(phase[i] %in% which_conditions))
        }
      y[order(order(x${user_session}))]
} # add function to package. 


dat$phase_pair <- unlist(by(dat, dat${user_case}, phase_pairs))

dat <- droplevels(dat)
