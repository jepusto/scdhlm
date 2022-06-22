# Clean data
dat <- preprocess_SCD(design = "{user_design}", 
                      cluster = {user_clusterID},
                      case = {user_caseID}, 
                      series = {user_seriesID},
                      phase = {user_phaseID}, 
                      session = {user_session}, 
                      outcome = {user_outcome}, 
                      center = {user_model_center}, 
                      round_session = {user_round}, 
                      treatment_name = "{user_treatment}", 
                      data = dat)
