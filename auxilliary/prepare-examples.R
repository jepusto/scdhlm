#--------------------
# Anglesea
#--------------------

Anglesea <- read.csv("auxilliary/Anglesea.csv", stringsAsFactors = FALSE)
Anglesea <- within(Anglesea, {
  case <- factor(paste("Case",case))
  condition <- factor(condition, levels = 0:1, labels = c("baseline","treatment"))
})
str(Anglesea)

save(Anglesea, file = "data/Anglesea.RData", compress = TRUE)

#--------------------
# Barton-Arwood
#--------------------

BartonArwood <- read.csv("auxilliary/Barton-Arwood-2005.csv", stringsAsFactors = FALSE)
BartonArwood$phase <- NULL
str(BartonArwood)

save(BartonArwood, file = "data/BartonArwood.RData", compress = TRUE)

#--------------------
# Carson
#---------------------

Carson <- read.csv("auxilliary/Carson.csv", stringsAsFactors = FALSE)
str(Carson)

save(Carson, file = "data/Carson.RData", compress = TRUE)


#--------------------
# Lambert
#--------------------

Lambert <- read.csv("auxilliary/Lambert.csv", stringsAsFactors = FALSE)
Lambert <- subset(Lambert, !is.na(outcome))
Lambert <- within(Lambert, {
  case <- factor(paste("case",case))
  treatment <- factor(treatment, levels = c("SSR","RC"))
})
str(Lambert)

save(Lambert, file = "data/Lambert.RData", compress = TRUE)


#--------------------
# Laski
#--------------------

Laski <- read.csv("auxilliary/Laski.csv", stringsAsFactors = FALSE)
str(Laski)
Laski <- within(Laski, {
  case <- factor(paste("Case",case))
  treatment <- factor(treatment, levels = 0:1, labels = c("baseline","treatment"))
})

save(Laski, file = "data/Laski.RData", compress = TRUE)


#--------------------
# Musser
#--------------------

Musser <- read.csv("auxilliary/Musser.csv", stringsAsFactors = FALSE)
str(Musser)

Musser <- within(Musser, {
  student <- factor(student, levels = 1:5, labels = c("Student 1","Student 2","Student 3","Female Control","Male Control"))
  treatment <- factor(treatment, levels = 0:2, labels = c("baseline","treatment","follow-up"))
})

save(Musser, file = "data/Musser.RData", compress = TRUE)

#--------------------
# Rodriguez
#--------------------

Rodriguez <- read.csv("auxilliary/Rodriguez-2014.csv", stringsAsFactors = FALSE)
Rodriguez$phase <- NULL
str(Rodriguez)

save(Rodriguez, file = "data/Rodriguez.RData", compress = TRUE)

#--------------------
# Romaniuk
#--------------------

Romaniuk <- read.csv("auxilliary/Romaniuk-2002.csv", stringsAsFactors = FALSE)
Romaniuk <- within(Romaniuk, {
  condition <- factor(condition, levels = c("No Choice","Choice","Differential reinforcement of alternative behavior"))
  measurement <- ifelse(case=="Riley", "Responses per minute", "Percentage of session time")
})
str(Romaniuk)

save(Romaniuk, file = "data/Romaniuk.RData", compress = TRUE)


#--------------------
# Saddler
#--------------------

Saddler <- read.csv("auxilliary/Saddler.csv", stringsAsFactors = FALSE)
Saddler <- within(Saddler, {
  case <- factor(paste("Case",case))
  measure <- factor(measure, levels = 1:3, labels = c("writing quality", "T-unit length", "number of constructions"))
  treatment <- factor(treatment, levels = 0:1, labels = c("baseline","treatment"))
})
str(Saddler)
save(Saddler, file = "data/Saddler.RData", compress = TRUE)


#--------------------
# Schutte
#--------------------

Schutte <- read.csv("auxilliary/Schutte.csv", stringsAsFactors = FALSE)
str(Schutte)
Schutte <- within(Schutte, {
  case <- factor(case, levels = 1:13, labels = paste("Case",1:13))
  treatment <- factor(treatment, levels = c("baseline","treatment"))
})

save(Schutte, file = "data/Schutte.RData", compress = TRUE)


#--------------------
# Thorne
#--------------------

Thorne <- read.csv("auxilliary/Thorne.csv", stringsAsFactors = FALSE)
names(Thorne)[2] <- "measure"
names(Thorne)[5] <- "condition"

Thorne <- within(Thorne, {
  case <- factor(case, levels = paste("Participant",1:12))
  measure <- factor(measure)
  condition <- factor(condition, levels = 0:1, labels = c("A","B"))
})
str(Thorne)

save(Thorne, file = "data/Thorne.RData", compress = TRUE)
