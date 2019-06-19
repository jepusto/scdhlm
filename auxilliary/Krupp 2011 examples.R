library(readr)

Krupp_reg <- read_csv("auxilliary/Krupp (2011) Regular Education Setting wo Maintenance.csv")
Krupp_SH <- read_csv("auxilliary/Krupp (2011) Study Hall Setting wo Maintenance.csv")

Krupp_reg_fit <- 
  lme(Outcome ~ Condition,
      random = ~ 1 | Case,
      correlation = corAR1(0, ~ Session | Case),
      data = Krupp_reg)

summary(Krupp_reg_fit)

reg_ES <- 
  g_REML(Krupp_reg_fit, 
         p_const = c(0,1), 
         r_const = c(1,0,1), 
         returnModel = FALSE)
reg_ES$g_AB


Krupp_SH_fit <- 
  lme(Outcome ~ Condition,
      random = ~ Condition | Case,
      correlation = corAR1(0, ~ Session | Case),
      data = Krupp_SH,
      control = lmeControl())

summary(Krupp_SH_fit)

SH_ES <- 
  g_REML(Krupp_SH_fit, 
         p_const = c(0,1), 
         r_const = c(1,0,1), 
         returnModel = FALSE)
SH_ES$g_AB
