
# Calculate effect size
res <- with(dat, effect_size_ABk(outcome = {user_outcome}, treatment = trt, id = {user_case}, phase = {user_phase_pair}, time = {user_session}))
res