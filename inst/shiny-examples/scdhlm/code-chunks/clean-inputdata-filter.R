# Clean data
dat <- preprocess_SCD(data = dat, case = {user_caseID}, phase = {user_phaseID}, 
                      session = {user_session}, outcome = {user_outcome}, design = "{user_design}", 
                      center = {user_model_center}, treatment_name = "{user_treatment}", round_session = {user_round})

# filter data
filter_vars <- {user_filtervars}
filter_vals <- {user_filtervals}
dat <- subset(dat, filter_vars == filter_vals)

