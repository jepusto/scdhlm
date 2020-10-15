# Clean data
dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","session","phase","outcome")
dat <- preprocess_SCD(data = dat, case = case, phase = phase, 
                      session = session, outcome = outcome, design = "{user_design}")

