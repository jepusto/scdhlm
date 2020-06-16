library(nlme)

test_that("scdhlm Works with Narozanic and Blair.", {
  
  skip(message = "Auxiliary dataset not included in package.")
  
  Naro <- read.csv("../../auxilliary/Narozanic and Blair_datasets_long_academic.csv", stringsAsFactors = FALSE)
  Naro$Session_int <- round(Naro$Session)
  
  Naro_dbl <- lme(Outcome ~ Phase,
                  random = ~ 1 | Participant, 
                  correlation = corAR1(0.1, ~ Session | Participant),
                  data = Naro)
  
  # g_mlm(Naro_dbl, p_const = c(0,1), r_const = c(1,0,1))
  # lmeInfo::Fisher_info(Naro_dbl)
  expect_error(g_mlm(Naro_dbl, p_const = c(0,1), r_const = c(1,0,1)))
  # This throw an error because of some wacky behavior in nlme::corMatrix()
  # where corAR1() gets converted to corARMA(), and Session spacing gets truncated
  # so that there are repeated values of Session within the same Participant
  
  Naro_int <- lme(Outcome ~ Phase,
                  random = ~ 1 | Participant, 
                  correlation = corAR1(0.1, ~ Session_int | Participant),
                  data = Naro)
  
  expect_s3_class(g_mlm(Naro_int, p_const = c(0,1), r_const = c(1,0,1)), "g_mlm")
  
})
