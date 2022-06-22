# Clean data

dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","series","outcome","session","phase")

dat <- preprocess_SCD(case = case, 
                      phase = phase, 
                      session = session, 
                      outcome = outcome, 
                      design = "{user_design}", 
                      center = {user_model_center},
                      series = series,
                      data = dat)

