
# Fit the model
phi_init <- .01 # specify the value of lag 1 autocorrelation
fit_RML <- lme(fixed = {user_fixed}, random = {user_random}, correlation = corAR1(phi_init, ~ {user_session} | {user_case}), data = dat,
           control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
summary(fit_RML)
