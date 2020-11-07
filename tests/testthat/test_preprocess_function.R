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
                               design = "MB", data = MB_dat)
data("Laski")

dat1_Laski <- preprocess_SCD(case = case, phase = treatment, 
                             session = time, outcome = outcome, 
                             design = "MB", data = Laski)

dat2_Laski <- preprocess_SCD(case = case, phase = treatment, 
                             session = time, outcome = outcome, 
                             design = "MB", center = 4, data = Laski)

dat3_Laski <- preprocess_SCD(case = Laski$case, phase = Laski$treatment,
                             session = Laski$time, outcome = Laski$outcome,
                             design = "MB")

# TR design
data("Anglesea")

dat1_Ang <- preprocess_SCD(case = case, phase = condition,
                           session = session, outcome = outcome, 
                           design = "TR", data = Anglesea)

dat2_Ang <- preprocess_SCD(case = Anglesea$case, phase = Anglesea$condition,
                           session = Anglesea$session, outcome = Anglesea$outcome, 
                           design = "TR")

data("Lambert")

test_that("The returned dataset is consistent with the input dataset.", {
  
  expect_error(preprocess_SCD(case = Student, phase = Phase, 
                              session = Time, outcome = Behavior, 
                              design = "MB", data = dat_dup_session))
  
  # MB design
  expect_equal(names(MB_dat), names(MB_dat_clean)[1:4])
  
  expect_error(preprocess_SCD(data = Laski))
  expect_equal(dat1_Laski, dat3_Laski)
  expect_equal(Laski$case, dat1_Laski$case)
  expect_equal(Laski$treatment, dat1_Laski$treatment)
  expect_equal(Laski$time, dat1_Laski$time)
  expect_equal(Laski$outcome, dat1_Laski$outcome)
  expect_equal(Laski$time, dat2_Laski$time + 4)
  
  # TR design
  expect_equal(dat1_Ang, dat2_Ang)
  expect_equal(Anglesea$case, dat1_Ang$case)
  expect_equal(Anglesea$condition, dat1_Ang$condition)
  expect_equal(Anglesea$session, dat1_Ang$session)
  expect_equal(Anglesea$outcome, dat1_Ang$outcome)
  
})

