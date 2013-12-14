library(nlme)
library(scdhlm)
data(Schutte)
Schutte$week <- Schutte$week - 9
Schutte$trt.week <- with(Schutte, unlist(tapply((treatment=="treatment") * week, 
                                                list(factor(treatment),case), 
                                                function(x) x - min(x))) + (treatment=="treatment"))
Schutte$id <- ordered(Schutte$case, levels=rev(with(Schutte, unique(case)[order(tapply(treatment=="baseline", case, sum))])))

p_const <- c(0,0,1,7)
r_const <- c(1,0,1)

m1 <- lme(fixed = fatigue ~ week + treatment + trt.week, 
             random = ~ 1 | case, 
             correlation = corAR1(0, ~ week | case),
             data = Schutte,
             method = "REML")
summary(m1)

m2 <- lme(fixed = fatigue ~ week + treatment + trt.week, 
    random = ~ 1 | id, 
    correlation = corAR1(0, ~ week | id),
    data = Schutte,
    method = "REML")
g_REML(m1, p_const, r_const)
g_REML(m2, p_const, r_const)
Info_Expected_lmeAR1(m1)
Info_Expected_lmeAR1(m2)

data(Saddler)
outcome <- subset(Saddler, measure==1)$outcome
treatment <- subset(Saddler, measure==1)$treatment
id <- subset(Saddler, measure==1)$case
time <- subset(Saddler, measure==1)$time
phi <- NULL
rho <- NULL

