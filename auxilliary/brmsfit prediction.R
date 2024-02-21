# Explore different ways to predict brmsfit

data(Laski)
Laski_mod <- calc_BCSMD(design = "MBP",
           case = case, phase = treatment,
           session = time, outcome = outcome,
           FE_base = 0, RE_base = 0, FE_trt = 0,
           Bayesian = TRUE,
           data = Laski)

data(Anglesea)
Ang_mod <- calc_BCSMD(design = "TR",
           case = case, phase = condition,
           session = session, outcome = outcome,
           treatment_name = "treatment",
           FE_base = 0, RE_base = 0, FE_trt = 0,
           Bayesian = TRUE,
           data = Anglesea)

data(Thiemann2001)
Thi_mod <- calc_BCSMD(design = "RMBB",
           case = case, series = series, phase = treatment,
           session = time, outcome = outcome,
           FE_base = 0, RE_base = 0, RE_base_2 = 0, FE_trt = 0,
           Bayesian = TRUE,
           data = Thiemann2001)

data(Bryant2018)
Bry_mod <- calc_BCSMD(design = "CMB",
           cluster = group, case = case, phase = treatment,
           session = session, outcome = outcome, center = 49,
           treatment_name = "treatment",
           FE_base = c(0,1), RE_base = c(0,1), RE_base_2 = 0,
           FE_trt = c(0,1), RE_trt = 1, RE_trt_2 = NULL,
           Bayesian = TRUE,
           data = Bryant2018)


mod <- Laski_mod
mod <- Ang_mod
mod <- Thi_mod
mod <- Bry_mod

# fixed 
beta <- brms::fixef(mod$model)[,1]
X_matrix <- brms::make_standata(mod$model$formula, mod$model$data)[["X"]]
fixed_pred <- X_matrix %*% beta

# random
dat <- mod$model$data
random_est <- lapply(brms::ranef(mod$model), function(x) x[,,1][,1])
random_names <- names(random_est)
random_pred <- mapply(function(x,y) rep(x, table(dat[[y]])), random_est, random_names)
random_pred_sum <- rowSums(random_pred)

dat["prediction"] <- fixed_pred + random_pred_sum

# use posterior_predict
pp <- brms::posterior_predict(Laski_mod$model) # higher variance than ppe 

# use posterior_epred to compute posterior draws of the expected 
# value of the posterior predictive distribution
# ignore the residual error
ppe <- brms::posterior_epred(Laski_mod$model) # rows: iterations cols:observations


# fixed effects
# fixed_est <- brms::fixef(mod$model)[,1]
# fixed_names <- names(fixed_est)
# fixed_slope_names <- setdiff(fixed_names, "Intercept")
# fixed_pred_slope_list <- sapply(fixed_slope_names, function(x) fixed_est[x] * dat[x], simplify = FALSE)
# fixed_pred_slope_sum <- Reduce("+", fixed_pred_slope_list)
# fixxxx <- fixed_est["Intercept"] + fixed_pred_slope_sum

## random effects
# random_est <- brms::ranef(mod$model)$case[,,1][,1]
# rand <- rep(random_est, table(dat$case))




