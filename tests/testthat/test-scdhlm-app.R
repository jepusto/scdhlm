context("Test scdhlm Shiny app")

skip_if_not_installed("shiny")
skip_if_not_installed("shinytest")
skip_if_not_installed("rvest")
skip_if_not_installed("xml2")
skip_if_not_installed("ggplot2")
skip_if_not_installed("markdown")
skip_if_not_installed("readxl")
skip_if_not_installed("glue")
skip_if_not_installed("janitor")
skip_if_not_installed("rclipboard")

suppressWarnings(library(shiny))
suppressWarnings(library(shinytest))
suppressWarnings(library(dplyr))
suppressWarnings(library(rvest))
suppressWarnings(library(xml2))
suppressWarnings(library(purrr))
suppressWarnings(library(nlme))

skip_if_not(dependenciesInstalled())

appDir <- system.file("shiny-examples", "scdhlm", package = "scdhlm")

test_that("Title and tabs are correct", {
  
  skip_on_cran()
  
  app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  
  # title
  appTitle <- app$getTitle()[[1]]
  expect_equal(appTitle, "Between-case standardized mean difference estimator")
  
  # tabs
  app$waitForValue("scdhlm_calculator")
  # app$findWidget("scdhlm_calculator")$listTabs()
  expect_equal(app$findWidget("scdhlm_calculator")$listTabs(), 
               c("scdhlm", "Load", "Inspect", "Model", "Effect size", "Syntax for R"))
  
})

# test the app output vs. README examples
check_readme <- function(data, estMethod, digits = 3) {
  app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  
  app$setInputs(scdhlm_calculator = "Load")
  app$setInputs(example = data)
  app$setInputs(scdhlm_calculator = "Inspect")
  app$setInputs(corStruct = "hom")
  app$setInputs(scdhlm_calculator = "Model")
  app$setInputs(method = estMethod)
  app$setInputs(scdhlm_calculator = "Effect size")
  
  Sys.sleep(0.5)
  output <- app$getValue(name = "effect_size_report")
  app_output <- 
    read_html(output) %>% 
    html_table(fill = TRUE) %>%
    as.data.frame() %>% 
    mutate_if(is.numeric, ~ round(., digits))
  
  return(app_output)
}

test_that("App output matches README example output", {
  
  skip_on_cran()
  
  # Laski
  app_output_Laski <- check_readme("Laski", estMethod = "RML")
  
  data("Laski")
  Laski_RML <- lme(fixed = outcome ~ treatment,
                   random = ~ 1 | case, 
                   correlation = corAR1(0, ~ time | case), 
                   data = Laski)
  Laski_ES_RML <- g_mlm(Laski_RML, p_const = c(0, 1), r_const = c(1, 0, 1))
  pkg_output_Laski <- 
    data.frame(
      g_AB = Laski_ES_RML$g_AB,
      SE_g_AB = Laski_ES_RML$SE_g_AB,
      df = Laski_ES_RML$nu
    ) %>% 
    mutate_if(is.numeric, ~ round(., 3))
    
  # CI_Laski_RML <- CI_g(Laski_ES_RML, symmetric = TRUE)
  
  expect_equal(app_output_Laski$BC.SMD.estimate, pkg_output_Laski$g_AB)
  expect_equal(app_output_Laski$Std..Error, pkg_output_Laski$SE_g_AB)
  expect_equal(app_output_Laski$Degrees.of.freedom, pkg_output_Laski$df)
  
  
  # Lambert
  app_output_Lambert <- check_readme("Lambert", estMethod = "HPS")
  
  data("Lambert")
  Lambert_ES <- effect_size_ABk(outcome = outcome, treatment = treatment, id = case, 
                                phase = phase, time = time, data = Lambert)
  pkg_output_Lambert <- 
    data.frame(
      g_AB = Lambert_ES$delta_hat,
      SE_g_AB = sqrt(Lambert_ES$V_delta_hat),
      df = Lambert_ES$nu
    ) %>% 
    mutate_if(is.numeric, ~ round(., 3))
  
  expect_equal(app_output_Lambert$BC.SMD.estimate, pkg_output_Lambert$g_AB)
  expect_equal(app_output_Lambert$Std..Error, pkg_output_Lambert$SE_g_AB)
  expect_equal(app_output_Lambert$Degrees.of.freedom, pkg_output_Lambert$df)
  
  # Saddler
  app_output_S <- check_readme("Saddler", estMethod = "HPS")
  
  data(Saddler)
  Saddler_quality <- subset(Saddler, measure=="writing quality")
  quality_ES <- effect_size_MB(outcome, treatment, case, time, data = Saddler_quality)
  pkg_output_S <- 
    data.frame(
      g_AB = quality_ES$delta_hat,
      SE_g_AB = sqrt(quality_ES$V_delta_hat),
      df = quality_ES$nu
    ) %>% 
    mutate_if(is.numeric, ~ round(., 3))
  
  expect_equal(app_output_S$BC.SMD.estimate, pkg_output_S$g_AB)
  expect_equal(app_output_S$Std..Error, pkg_output_S$SE_g_AB)
  expect_equal(app_output_S$Degrees.of.freedom, pkg_output_S$df)
  
})

# test the app output vs syntax output (RML)
check_syntax <- function(data, digits = 4L) {
  app <- ShinyDriver$new(appDir, loadTimeout = 6e+05)
  
  app$setInputs(scdhlm_calculator = "Load")
  app$setInputs(example = data)
  app$setInputs(scdhlm_calculator = "Inspect")
  app$setInputs(scdhlm_calculator = "Model")
  app$setInputs(corStruct = "AR(1)")
  app$setInputs(scdhlm_calculator = "Effect size")
  app$setInputs(scdhlm_calculator = "Syntax for R")
  app$setInputs(clipbtn = "click")
  
  Sys.sleep(0.5)
  output <- app$getValue(name = "effect_size_report")
  summary_output <- 
    read_html(output) %>% 
    html_table(fill = TRUE) %>%
    as.data.frame() %>% 
    mutate_if(is.numeric, ~ round(., digits)) %>% 
    select(g_AB = BC.SMD.estimate, SE_g_AB = Std..Error, df = Degrees.of.freedom)
  
  raw_syntax <- app$getValue(name = "syntax")
  raw_syntax_cut <- sub("summary\\(ES_RML).*", "", raw_syntax)
  # cat(raw_syntax, file = "C:\\Users\\C-ama\\OneDrive\\Desktop\\shh.txt")
  # source("C:\\Users\\C-ama\\OneDrive\\Desktop\\shh.txt")
  code_file <- tempfile(fileext = ".R")
  cat(raw_syntax_cut, file = code_file)
  source(code_file)
  
  pkg_output <- 
    data.frame(
      g_AB = ES_RML$g_AB,
      SE_g_AB = ES_RML$SE_g_AB,
      df = ES_RML$nu
    ) %>% 
    mutate_if(is.numeric, ~ round(., digits))
  
  return(identical(summary_output, pkg_output))
}

test_that("The summary table output matches the syntax results", {
  skip_on_cran()
  expect_true(check_syntax("AlberMorgan")) # how to round up
  expect_true(check_syntax("BartonArwood"))
  expect_true(check_syntax("Thiemann2001"))
  expect_true(check_syntax("Bryant2018"))
})

