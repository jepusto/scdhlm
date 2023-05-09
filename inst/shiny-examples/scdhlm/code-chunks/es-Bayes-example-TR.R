# Clean data
dat <- dat[,c("{user_parms}")]
names(dat) <- c("case","session","phase","outcome")

# Calculate effect size
res <- calc_BCSMD(design = "{user_design}", 
                  case = case, 
                  phase = phase, 
                  session = session, 
                  outcome = outcome, 
                  FE_base = 0, 
                  RE_base = 0, 
                  FE_trt = 0,
                  corStruct = "{user_corStruct}",
                  varStruct = "{user_varStruct}",
                  Bayesian = TRUE,
                  chains = user_chains,
                  iter = user_iter,
                  warmup = user_warmup,
                  thin = user_thin,
                  cores = user_cores,
                  seed = user_seed,
                  summary = FALSE,
                  data = dat)

summary(res)


