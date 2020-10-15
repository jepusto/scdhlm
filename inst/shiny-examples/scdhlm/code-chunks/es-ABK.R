
# Calculate effect size
res <- with(dat, effect_size_ABk(outcome = outcome, treatment = trt, id = case, phase = phase_pair, time = session))
res