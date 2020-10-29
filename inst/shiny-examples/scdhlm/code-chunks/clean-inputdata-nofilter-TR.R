# Clean data
dat <- preprocess_SCD(data = dat, case = {user_caseID}, phase = {user_phaseID}, 
                      session = {user_session}, outcome = {user_outcome}, design = "{user_design}", 
                      treatment_name = "{user_treatment}", round_session = {user_round})
