# Calculate effect size
res <- effect_size_ABk(outcome = {user_outcome}, 
                       treatment = trt, 
                       id = {user_case}, 
                       phase = {user_phase_pair}, 
                       time = {user_session},
                       data = dat)

res
