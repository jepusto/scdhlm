
# Fit the model
fit_RML <- lme(fixed = {user_fixed}, 
               random = {user_random}, {corr_struct} {var_struct}
               data = dat,
               control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
summary(fit_RML)
