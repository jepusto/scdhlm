# load effect size function
source("Multiple baseline effect size function.R")

#######################################
## Example 1: Saddler, et al. (2008) ##
#######################################

# read in data
Saddler <- read.csv("Saddler.csv")

# calculate effect size, using outcome measure 1
ES.Saddler <- with(subset(Saddler, measure==1), 
                   effect_size_MB(outcome, treatment, case, time))
print(ES.Saddler)

# auxilliary calculations
J(ES.Saddler$nu)  # J(nu)
ES.Saddler$theta^2 * length(unique(Saddler$case))^2 / (1 - ES.Saddler$rho_hat)  # Constant A

# calculate effect sizes for other outcome measures
with(subset(Saddler, measure==2), effect_size_MB(outcome, treatment, case, time))
with(subset(Saddler, measure==3), effect_size_MB(outcome, treatment, case, time))



####################################
## Example 2:Laski, et al. (1988) ##
#####################################

# read in data
Laski <- read.csv("Laski.csv")

# calculate effect size
ES.Laski <- with(Laski, effect_size_MB(outcome, treatment, case, time))
print(ES.Laski)

# auxilliary calculations
J(ES.Laski$nu)
ES.Laski$theta^2 * length(unique(Laski$case))^2 / (1 - ES.Laski$rho_hat)  # Constant A