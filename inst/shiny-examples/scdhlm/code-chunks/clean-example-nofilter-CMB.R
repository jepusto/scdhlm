# Clean data

dat <- dat[,c("{user_parms}")]
names(dat) <- c("cluster","case","phase","session","outcome")

dat <- preprocess_SCD(design = "{user_design}", 
                      case = case, 
                      phase = phase, 
                      session = session, 
                      outcome = outcome, 
                      cluster = cluster,
                      center = {user_model_center},
                      data = dat)

