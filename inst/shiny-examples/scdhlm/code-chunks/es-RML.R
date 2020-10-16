# Calculate effect size with g_mlm()
A <- {user_A}
B <- {user_B}
p_const <- c(rep(0L, length({user_FE_base})), (B - A - 1)^as.integer({user_FE_trt})) 
p_const <- {user_pconstant}
r_const <- c({user_rconst_base}, {user_rconst_trt}, {user_rconst_cor}, {user_rconst_var}, 1L) # specify whether using random effects, cor struct, var struct, and level-1 errors
r_const <- {user_rconstant}
ES_RML <- g_mlm(fit_RML, p_const = p_const, r_const = r_const, infotype = "expected", returnModel = TRUE)
summary(ES_RML)
