# Clean data
dat <- dat[,c("{user_parms}")]
names(dat) <- c("{user_varnames}")

# Calculate effect size
res <- calc_BCSMD(design = "{user_design}", 
                  cluster = {user_cluster},
                  case = case, 
                  series = {user_series},
                  phase = phase, 
                  session = session, 
                  outcome = outcome, 
                  center = {user_model_center},
                  FE_base = {user_FE_base},
                  RE_base = {user_RE_base},
                  RE_base_2 = {user_RE_base2},
                  FE_trt = {user_FE_trt},
                  RE_trt = {user_RE_trt},
                  RE_trt_2 = {user_RE_trt2},
                  corStruct = "{user_corStruct}",
                  varStruct = "{user_varStruct}",
                  A = {user_A},
                  B = {user_B},
                  summary = FALSE,
                  data = dat)

summary(res)


