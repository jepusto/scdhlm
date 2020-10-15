
# Fit the model
phi_init <- .01 # specify an initial value of lag 1 auto-correlation
fit_RML <- lme(fixed = {user_fixed}, 
               random = {user_random}, 
               correlation = corAR1(phi_init, ~ session | case), 
               data = dat,
               control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
summary(fit_RML)
