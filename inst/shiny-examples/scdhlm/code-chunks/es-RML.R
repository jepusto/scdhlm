
# Calculate effect size with g_mlm()
ES_RML <- g_mlm(fit_RML, p_const = {user_pconstant}, r_const = {user_rconstant}, infotype = "expected", returnModel = TRUE)
summary(ES_RML)
