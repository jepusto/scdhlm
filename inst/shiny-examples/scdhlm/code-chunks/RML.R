#RML
dat <-dat[order(dat${user_case}, dat${user_session}),] 

#model fit
modelfit <-lme(fixed = {user_fixed}, random = {user_random}, 
    correlation = corAR1(0.01, ~ {user_session} | {user_case}), 
    data = dat, control = lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE))


summary(modelfit)

#Effect Size

g_mlm(modelfit, p_const = c("{user_pconstant}"), r_const = c(" {user_rconstant}" ), infotype = "expected", returnModel = TRUE)
