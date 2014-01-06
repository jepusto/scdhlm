
library(scdhlm)
library(plyr)
library(reshape)
library(ggplot2)

##-----------------------------------------------------------------
## Read in data
##-----------------------------------------------------------------

data(Schutte)

all_weeks <- with(Schutte, expand.grid(week=unique(week),case=unique(case)))
Schutte <- merge(Schutte, all_weeks, all=TRUE)
Schutte$treatment[is.na(Schutte$treatment)] <- "treatment"

# create time-by-trt interaction
Schutte$trt.week <- with(Schutte, unlist(tapply((treatment=="treatment") * week, 
                                                list(treatment,case), 
                                                function(x) x - min(x))) + (treatment=="treatment"))

# center at week 9
Center <- 9
Schutte$week <- Schutte$week - Center

# exclude case 4
Schutte_all <- subset(Schutte, case != 4)
Schutte <- subset(Schutte, !is.na(fatigue) & case != 4)


##-----------------------------------------------------------------
## Model 3: random intercepts, fixed trends
##-----------------------------------------------------------------

hlm1 <- lme(fixed = fatigue ~ week + treatment + trt.week, 
            random = ~ 1 | case, 
            correlation = corAR1(0, ~ week | case),
            data = Schutte,
            method = "REML")
summary(hlm1)
Schutte_all$hlm1 <- predict(hlm1, newdata = Schutte_all)

Schutte_g1 <- g_REML(m_fit = hlm1, p_const = c(0,0,1,7), r_const = c(1,0,1))
summary(Schutte_g1)


##-----------------------------------------------------------------
## Model 4: random intercepts, random baseline trends ##
##-----------------------------------------------------------------

hlm2 <- update(hlm1, random = ~ week | case, 
               control=lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE))
summary(hlm2)
Schutte_all$hlm2 <- predict(hlm2, newdata = Schutte_all)

Schutte_g2 <- g_REML(m_fit = hlm2, p_const = c(0,0,1,7), r_const = c(1,0,1,0,0))
summary(Schutte_g2)

anova(hlm1, hlm2)
mean(pchisq(2 * (hlm2$logLik - hlm1$logLik), 1:2, lower.tail=FALSE))

g2_boots <- simulate_g_REML(Schutte_g2, nsim = 2000)
library(boot)
boot.ci(boot.out = list(R=2000, call = "", sim = "parametric"), 
        type = "perc", t0 = Schutte_g2$delta_AB, t = g2_boots$delta_AB)
boot.ci(boot.out = list(R=2000, call = "", sim = "parametric"), 
        type = "perc", t0 = Schutte_g2$g_AB, t = g2_boots$g_AB)
mean(g2_boots$delta_AB < Schutte_g2$delta_AB)
mean(g2_boots$g_AB < as.double(Schutte_g2$g_AB))
CI_g(Schutte_g2)

##--------------------------------------------------------------------------------
## Model 5: random intercepts, random baseline trends, random treatment trends
##--------------------------------------------------------------------------------


hlm3 <- update(hlm2, random = ~ week + trt.week | case, 
               control=lmeControl(msMaxIter = 50, apVar=FALSE, returnObject=TRUE))
summary(hlm3)
Schutte_all$hlm3 <- predict(hlm3, newdata=Schutte_all)

Schutte_g3 <- g_REML(m_fit = hlm3, p_const = c(0,0,1,7), r_const = c(1,0,1,0,0,0,0,0))
summary(Schutte_g3)

anova(hlm1, hlm2, hlm3)
Dev <- 2 * (hlm3$logLik - hlm2$logLik)
mean(pchisq(Dev, 2:3, lower.tail=FALSE))

## look at individual effects at week 9
individual.effects <- fixed.effects(hlm3)["treatmenttreatment"] + 7 * fixed.effects(hlm3)["trt.week"] + 7 * random.effects(hlm3)["trt.week"]
summary(individual.effects)
individual.effects

fixed.effects(hlm3)["trt.week"] + random.effects(hlm3)["trt.week"]

##--------------------------------------------------------------------------------
## plot data with BLUP estimated trends
##--------------------------------------------------------------------------------

Schutte_all$id <- factor(Schutte_all$case, labels = paste("Case",LETTERS[1:12]))
levels(Schutte_all$treatment) <- c("Baseline","Treatment")

change <- data.frame(id=levels(Schutte_all$id),
                     phase.change = with(subset(Schutte_all, treatment== "Treatment"), 
                                         tapply(week, id, min)) - 0.5)


Schutte.pred <- melt.data.frame(
  subset(Schutte_all, trt.week <=7, select=c("id","week","treatment","hlm1","hlm2","hlm3")),
  id.vars=c("id","week","treatment"))
Schutte.pred$variable <- factor(Schutte.pred$variable, labels=paste0("MB",3:5))
Schutte_all$variable <- "MB3"

ggplot(Schutte.pred, aes(week, value, linetype=variable)) + 
  geom_line() + 
  facet_wrap(~ id, ncol = 3) + 
  labs(color="Phase",linetype="Model", shape="Phase", y ="Fatigue", x="Week") + 
  geom_point(data = subset(Schutte_all, !is.na(fatigue)),
             aes(week, fatigue, shape=treatment, color=treatment)) +
  geom_vline(data = change, aes(xintercept=phase.change)) +
  theme_bw() 
