
# Fit the model
fit_RML <- lme(fixed = {user_fixed}, 
               random = {user_random}, 
               correlation = corARMA(0, ~ {user_session} | {user_case}, p = 0, q = 1), 
               data = dat,
               control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE))
summary(fit_RML)
