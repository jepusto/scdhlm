
# Calculate effect size
res <- with(dat, effect_size_MB(outcome = outcome, treatment = trt, id = case, time = session)) 
res