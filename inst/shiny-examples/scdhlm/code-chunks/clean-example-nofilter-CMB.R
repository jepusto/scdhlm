# Clean data

dat <- dat[,c("{user_parms}")]
names(dat) <- c("cluster","case","phase","session","outcome")

dat <- preprocess_SCD(case = case, 
                      phase = phase, 
                      session = session, 
                      outcome = outcome, 
                      design = "{user_design}", 
                      center = {user_model_center},
                      cluster = cluster,
                      data = dat)

