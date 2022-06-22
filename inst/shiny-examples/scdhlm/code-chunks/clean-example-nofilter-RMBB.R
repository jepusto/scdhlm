# Clean data

dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","series","outcome","session","phase")

dat <- preprocess_SCD(design = "{user_design}", 
                      case = case, 
                      series = series,
                      phase = phase, 
                      session = session, 
                      outcome = outcome, 
                      center = {user_model_center},
                      data = dat)

