library(nlme)

# Narozanic and Blair_datasets_long_academic
Naro <- read.csv("auxilliary/Narozanic and Blair_datasets_long_academic.csv", stringsAsFactors = FALSE)
Naro$Session_int <- round(Naro$Session)

Naro_fit <- lme(Outcome ~ Phase,
                random = ~ 1 | Participant, 
                correlation = corAR1(0.1, ~ Session | Participant),
                data = Naro)

g_mlm(Naro_fit, p_const = c(0,1), r_const = c(1,0,1))


Naro_fit <- lme(Outcome ~ Phase,
                random = ~ 1 | Participant, 
                correlation = corAR1(0.1, ~ Session_int | Participant),
                data = Naro)

g_mlm(Naro_fit, p_const = c(0,1), r_const = c(1,0,1))
