#--------------------
# Alber-Morgan
#--------------------

AlberMorgan <- read.csv("auxilliary/Alber-Morgan-2007.csv", stringsAsFactors = FALSE)
str(AlberMorgan)
AlberMorgan <- within(AlberMorgan, {
  case <- factor(case, levels = c("Theo","Kelly","Brian","Andrew"))
  condition <- factor(phase, levels = c("baseline","treatment"))
  phase <- NULL
})
AlberMorgan <- AlberMorgan[c("case","condition","session","outcome")]
str(AlberMorgan)
levels(AlberMorgan$case)

save(AlberMorgan, file = "data/AlberMorgan.RData", compress = TRUE, version = 2)

#--------------------
# Anglesea
#--------------------

Anglesea <- read.csv("auxilliary/Anglesea.csv", stringsAsFactors = FALSE)
Anglesea <- within(Anglesea, {
  case <- factor(paste("Case",case))
  condition <- factor(condition, levels = 0:1, labels = c("baseline","treatment"))
})
str(Anglesea)

save(Anglesea, file = "data/Anglesea.RData", compress = TRUE, version = 2)

#--------------------
# Barton-Arwood
#--------------------

BartonArwood <- read.csv("auxilliary/Barton-Arwood-2005.csv", stringsAsFactors = FALSE)
BartonArwood$phase <- NULL
str(BartonArwood)
BartonArwood <- within(BartonArwood, {
  case <- factor(case, levels = c("Sam","Gerald","Rich","Jack","Emma","Kim"))
  condition <- factor(condition, levels = c("A","B"))
})
str(BartonArwood)

save(BartonArwood, file = "data/BartonArwood.RData", compress = TRUE, version = 2)

#--------------------
# Carson
#---------------------

Carson <- read.csv("auxilliary/Carson.csv", stringsAsFactors = FALSE)
str(Carson)

save(Carson, file = "data/Carson.RData", compress = TRUE, version = 2)


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

save(Lambert, file = "data/Lambert.RData", compress = TRUE, version = 2)


#--------------------
# Laski
#--------------------

Laski <- read.csv("auxilliary/Laski.csv", stringsAsFactors = FALSE)
str(Laski)
Laski <- within(Laski, {
  case <- factor(paste("Case",case))
  treatment <- factor(treatment, levels = 0:1, labels = c("baseline","treatment"))
})

save(Laski, file = "data/Laski.RData", compress = TRUE, version = 2)


#--------------------
# Musser
#--------------------

Musser <- read.csv("auxilliary/Musser.csv", stringsAsFactors = FALSE)
str(Musser)

Musser <- within(Musser, {
  student <- factor(student, levels = 1:5, labels = c("Student 1","Student 2","Student 3","Female Control","Male Control"))
  treatment <- factor(treatment, levels = 0:2, labels = c("baseline","treatment","follow-up"))
})

save(Musser, file = "data/Musser.RData", compress = TRUE, version = 2)

#--------------------
# Rodriguez
#--------------------

Rodriguez <- read.csv("auxilliary/Rodriguez-2014.csv", stringsAsFactors = FALSE)
Rodriguez$phase <- NULL
str(Rodriguez)
Rodriguez <- within(Rodriguez, {
  case <- factor(case, levels = unique(case))
  condition <- factor(condition, levels = c("A","B"))
})
str(Rodriguez)

save(Rodriguez, file = "data/Rodriguez.RData", compress = TRUE, version = 2)

#--------------------
# Romaniuk
#--------------------

Romaniuk <- read.csv("auxilliary/Romaniuk-2002.csv", stringsAsFactors = FALSE)
Romaniuk <- within(Romaniuk, {
  case <- factor(case, levels = unique(case))
  condition <- factor(condition, levels = c("No Choice","Choice","Differential reinforcement of alternative behavior"))
  measurement <- ifelse(case=="Riley", "Responses per minute", "Percentage of session time")
})
str(Romaniuk)

save(Romaniuk, file = "data/Romaniuk.RData", compress = TRUE, version = 2)

#--------------------
# Ruiz
#--------------------
library(readxl)
library(tidyverse)

Ruiz <- read_excel('auxilliary//Data_Sheet_1_A Multiple-Baseline Evaluation.xlsx', sheet = "Overall")


Ruiz <- Ruiz %>% 
  pivot_longer(cols= `PSWQ`:`VQ-OBSTRUCTION`, names_to = "measure", values_to = "outcome")


Ruiz <- Ruiz %>%
  select(case, time=session, measure, outcome) %>%
  mutate(
    treatment = case_when(
      time < 6 ~ "baseline",
      time == 6 ~ "treatment",
      time == 7 ~ "treatment",
      time == 8 ~ "post",
      time > 8 ~ "follow-up"
    ),
    treatment = as.factor(treatment),
    case = as.factor(case),
    measure = as.factor(measure)
  )

save(Ruiz, file = "data/Ruiz.RData", compress = TRUE, version = 2, version = 2)


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
save(Saddler, file = "data/Saddler.RData", compress = TRUE, version = 2)


#--------------------
# Schutte
#--------------------

Schutte <- read.csv("auxilliary/Schutte.csv", stringsAsFactors = FALSE)
str(Schutte)

Schutte <- within(Schutte, {
  case <- factor(case, levels = 1:13, labels = paste("Case",1:13))
  treatment <- factor(treatment, levels = c("baseline","treatment"))
})
Schutte <- droplevels(subset(Schutte, case != "Case 4"))

save(Schutte, file = "data/Schutte.RData", compress = TRUE, version = 2)


#--------------------
# Salazar
#--------------------

library(readxl)
library(tidyverse)

sheet <- excel_sheets('auxilliary//Raw Data.xlsx')
datFile <- lapply(setNames(sheet, sheet), function(x) read_excel("auxilliary//Raw Data.xlsx", sheet=x))
datFile <-  bind_rows(datFile, .id="Sheet")

datFile <- datFile %>% 
  select(Sheet:`GPQ-C`) %>%
  pivot_longer(cols= `AFQ-Y`:`GPQ-C`, names_to = "measure", values_to = "outcome")

datFile <- datFile %>%
  select(case = Sheet, measure, condition= `...1`, outcome) %>%
  mutate(
    treatment = case_when(
      str_detect(condition, "LB") ~ "baseline",
      str_detect(condition, "Int") ~ "treatment",
      str_detect(condition, "Post") ~ "post",
      str_detect(condition, "FU") ~ "follow-up",
    ),
    condition = as.factor(condition),
    measure = as.factor(measure),
    case = as.factor(case)
  ) %>%
  group_by(case, measure) %>%
  mutate(time = as.numeric(row_number())) %>%
  select(case, measure, treatment, time, outcome)

Salazar <- as.data.frame(datFile)

save(Salazar, file = "data/Salazar.RData", compress = TRUE, version = 2)


#--------------------
# Thorne
#--------------------

library(readr)
library(dplyr)

phase_pairs <- function(phase) {
  conditions <- levels(phase)
  n <- length(phase)
  y <- rep(1,n)
  for (i in 2:n) {
    (which_lev <- match(phase[i-1], conditions))
    (which_conditions <- conditions[c(which_lev, which_lev + 1)])
    !(phase[i] %in% which_conditions)
    (y[i] <- y[i - 1] + !(phase[i] %in% which_conditions))
  }
  y
}

Thorne <- 
  read_csv("auxilliary/Thorne.csv") %>%
  select(case = Case_number, measure = Measure, session = Session_number, 
         condition = Condition, phase_indicator = Trt, outcome = Outcome) %>%
  mutate(
    case = factor(case, levels = 1:12, labels = paste("Participant", 1:12)),
    measure = factor(measure),
    phase_id = factor(condition, levels = c("A","B"))
  ) %>%
  arrange(measure, case, session) %>%
  group_by(measure, case) %>% 
  mutate(
    phase_pair = phase_pairs(phase_id),
    phase_id = paste0(phase_id, phase_pair)
  ) %>%
  select(case, measure, session, phase_id, condition, phase_indicator, outcome) %>%
  as.data.frame()

str(Thorne)

save(Thorne, file = "data/Thorne.RData", compress = TRUE, version = 2)

#--------------------
# Bryant2018
#--------------------

Bryant2018 <- read.csv("auxilliary/Bryant2018.csv", stringsAsFactors = FALSE)
str(Bryant2018)

Bryant2018 <- within(Bryant2018, {
  Study_ID <- rep("Bryant2018", length(Bryant2018$school))
  school <- factor(paste("school", school))
  case <- factor(studentID)
  outcome <- AC
  treatment <- ifelse(phase == "Baseline", "baseline", "treatment")
  treatment <- factor(treatment)
})

Bryant2018 <- Bryant2018[c("Study_ID", "school","case","treatment","session","session_trt","outcome")]

# time-point constants
A <- 4
B <- 21

# center at follow-up time
Center <- B
Bryant2018$session_c <-Bryant2018$session - Center

save(Bryant2018, file = "data/Bryant2018.RData", compress = TRUE, version = 2)

#--------------------
# Thiemann2001
#--------------------

library(tidyverse)

Thiemann2001 <- read.csv("auxilliary/Thiemann2001.csv", stringsAsFactors = FALSE)
str(Thiemann2001)

Thiemann2001 <- Thiemann2001 %>%
  arrange(case, series, time) %>%
  group_by(case, series) %>%
  mutate(trt_time = pmax(0, time - max(time[treatment == "A"])))

Thiemann2001 <- within(Thiemann2001, {
  case <- factor(case)
  series <- factor(series)
  treatment <- ifelse(treatment == "A", "baseline", "treatment")
  treatment <- factor(treatment)
})

# time-point constants
A <- 8
B <- 30

# center at follow-up time
Center <- B
Thiemann2001$time_c <- Thiemann2001$time - Center

save(Thiemann2001, file = "data/Thiemann2001.RData", compress = TRUE, version = 2)


#--------------------
# Thiemann2004
#--------------------

Thiemann2004 <- read.csv("auxilliary/Thiemann2004.csv", stringsAsFactors = FALSE)
str(Thiemann2004)

Thiemann2004 <- Thiemann2004 %>%
  arrange(case, series, time) %>%
  group_by(case, series) %>%
  mutate(
    trt_time = pmax(0, time - max(time[treatment == "A"])))

Thiemann2004 <- within(Thiemann2004, {
  case <- factor(case)
  series <- factor(series)
  treatment <- ifelse(treatment == "A", "baseline", "treatment")
  treatment <- factor(treatment)
})

# time-point constants
A <- 10
B <- 33

# center at follow-up time
Center <- B
Thiemann2004$time_c <- Thiemann2004$time - Center

save(Thiemann2004, file = "data/Thiemann2004.RData", compress = TRUE, version = 2)
