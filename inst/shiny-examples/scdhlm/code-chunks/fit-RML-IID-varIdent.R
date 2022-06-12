
# Fit the model
fit_RML <- lme(fixed = {user_fixed}, 
               random = {user_random},
               data = dat,
               weights = varIdent(form = ~ 1 | {user_phase}),
               control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
summary(fit_RML)
