phase_pairs <- function(x) {
  condition <- factor(x, levels = unique(x), labels = LETTERS[1:length(unique(x))])
  phases <- levels(condition)
  n <- length(x)
  y <- rep(1,n)
  for (i in 2:n) {
    which_lev <- match(condition[i-1], phases)
    which_conditions <- phases[c(which_lev, which_lev + 1)]
    y[i] <- y[i - 1] + !(condition[i] %in% which_conditions)
  }
  paste0(levels(condition)[condition], y)
}

x1 <- c("Baseline", "Baseline", "Baseline", "Baseline", "VSM", "VSM", "VSM", "Baseline", "Baseline")

phase_pairs(x1)

x2 <- c("base","base","trt","trt","trt","base","base","trt","trt","base")

phase_pairs(x2)