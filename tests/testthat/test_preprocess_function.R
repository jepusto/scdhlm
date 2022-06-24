context("Preprocess_SCD function")

# MB design
MB_dat <- data.frame(
  Student = rep(LETTERS[1:4], each = 10),
  Phase = rep(rep(c("Base","Trt"), each = 5), 4),
  Time = rep(1:10, 4),
  Behavior = rnorm(40)
)

dat_dup_session <- MB_dat
dat_dup_session$Time <- rep(1:8, 5)

MB_dat_clean <- preprocess_SCD(case = Student, phase = Phase, 
                               session = Time, outcome = Behavior, 
                               design = "MBP", data = MB_dat)


test_that("The returned dataset is consistent with the input dataset for MBP designs.", {
  
  expect_error(preprocess_SCD(case = Student, phase = Phase, 
                              session = Time, outcome = Behavior, 
                              design = "MB", data = dat_dup_session))
  
  expect_equal(names(MB_dat), names(MB_dat_clean)[1:4])
  
  data("Laski")
  
  dat1_Laski <- preprocess_SCD(case = case, phase = treatment, 
                               session = time, outcome = outcome, 
                               design = "MBP", data = Laski)
  
  dat2_Laski <- preprocess_SCD(case = case, phase = treatment, 
                               session = time, outcome = outcome, 
                               design = "MBP", center = 4, data = Laski)
  
  dat3_Laski <- preprocess_SCD(case = Laski$case, phase = Laski$treatment,
                               session = Laski$time, outcome = Laski$outcome,
                               design = "MB")
  
  expect_error(preprocess_SCD(data = Laski))
  expect_equivalent(dat1_Laski, dat3_Laski)
  expect_equal(Laski$case, dat1_Laski$case)
  expect_equal(Laski$treatment, dat1_Laski$treatment)
  expect_equal(Laski$time, dat1_Laski$time)
  expect_equal(Laski$outcome, dat1_Laski$outcome)
  expect_equal(Laski$time, dat2_Laski$time + 4)
  
})

test_that("The returned dataset is consistent with the input dataset for TR designs.", {
  
  data("Anglesea")
  
  dat1_Ang <- preprocess_SCD(case = case, phase = condition,
                             session = session, outcome = outcome, 
                             design = "TR", data = Anglesea)
  
  dat2_Ang <- preprocess_SCD(case = Anglesea$case, phase = Anglesea$condition,
                             session = Anglesea$session, outcome = Anglesea$outcome, 
                             design = "TR")
  
  expect_equivalent(dat1_Ang, dat2_Ang)
  expect_equal(Anglesea$case, dat1_Ang$case)
  expect_equal(Anglesea$condition, dat1_Ang$condition)
  expect_equal(Anglesea$session, dat1_Ang$session)
  expect_equal(Anglesea$outcome, dat1_Ang$outcome)
  
  Ang_case <- Anglesea$case
  Ang_condition <- Anglesea$condition
  Ang_session <- Anglesea$session
  Ang_outcome <- Anglesea$outcome
  
  dat3_Ang <- preprocess_SCD(case = Ang_case, phase = Ang_condition,
                             session = Ang_session, outcome = Ang_outcome, 
                             design = "TR")
  expect_equivalent(dat1_Ang, dat3_Ang)
  
  
  
})

test_that("The returned dataset is consistent with the input dataset for RMBB designs.", {
  
  data("Thiemann2001")
  
  dat1_Thiemann <- preprocess_SCD(design = "RMBB",
                                  case = case, series = series,
                                  phase = treatment, session = time,
                                  outcome = outcome, data = Thiemann2001)
  
  dat2_Thiemann <- preprocess_SCD(design = "RMBB",
                                  case = Thiemann2001$case, series = Thiemann2001$series,
                                  phase = Thiemann2001$treatment, session = Thiemann2001$time,
                                  outcome = Thiemann2001$outcome)
  
  expect_equivalent(dat1_Thiemann, dat2_Thiemann)
  expect_equal(Thiemann2001$case, dat1_Thiemann$case)
  expect_equal(Thiemann2001$series, dat1_Thiemann$series)
  expect_equal(Thiemann2001$treatment, dat1_Thiemann$treatment)
  expect_equal(Thiemann2001$time, dat1_Thiemann$time)
  expect_equal(Thiemann2001$outcome, dat1_Thiemann$outcome)
  
})

test_that("The returned dataset is consistent with the input dataset for CMB designs.", {
  
  data("Bryant2018")
  
  dat1_Bry <- preprocess_SCD(design = "CMB",
                             cluster = school, case = case,
                             phase = treatment, session = session,
                             outcome = outcome, data = Bryant2018)
  
  dat2_Bry <- preprocess_SCD(design = "CMB",
                             cluster = Bryant2018$school, case = Bryant2018$case,
                             phase = Bryant2018$treatment, session = Bryant2018$session,
                             outcome = Bryant2018$outcome)
  
  expect_equivalent(dat1_Bry, dat2_Bry)
  expect_equal(Bryant2018$school, dat1_Bry$school)
  expect_equal(Bryant2018$case, dat1_Bry$case)
  expect_equal(Bryant2018$treatment, dat1_Bry$treatment)
  expect_equal(Bryant2018$session, dat1_Bry$session)
  expect_equal(Bryant2018$outcome, dat1_Bry$outcome)
  
})
