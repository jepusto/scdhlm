# Calculate effect size
res <- calc_BCSMD(design = "{user_design}", 
                  case = {user_caseID}, 
                  phase = {user_phaseID}, 
                  session = {user_session}, 
                  outcome = {user_outcome}, 
                  round_session = {user_round}, 
                  treatment_name = "{user_treatment}",
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
