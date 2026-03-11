context("Test scdhlm Shiny app")

skip_if_not_installed("shiny")
skip_if_not_installed("shinytest2")
skip_if_not_installed("rvest")
skip_if_not_installed("ggplot2")
skip_if_not_installed("markdown")
skip_if_not_installed("readxl")
skip_if_not_installed("glue")
skip_if_not_installed("janitor")
skip_if_not_installed("rclipboard")

suppressWarnings(library(shiny))
suppressWarnings(library(rvest))
suppressWarnings(library(nlme))
suppressWarnings(library(shinytest2))


appDir <- system.file("shiny-examples", "scdhlm", package = "scdhlm")

test_that("Title and tabs are correct", {
  
  skip_on_cran()
  
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  # title
  app_title <- app$get_js("document.title")
  expect_equal(app_title, "Between-case standardized mean difference estimator")
  
  # tabs
  app$wait_for_idle()
  tabs <- c("scdhlm", "Load", "Inspect", "Model", "Effect size", "Syntax for R")
  
  for (tab in tabs) {
    app$set_inputs(scdhlm_calculator = tab)
    app$wait_for_idle()
    expect_equal(app$get_value(input = "scdhlm_calculator"), tab)
  }
})

# test the app output vs. README examples
check_readme <- function(data, estMethod, digits = 3) {
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  app$set_inputs(scdhlm_calculator = "Load")
  app$set_inputs(example = data)
  app$set_inputs(scdhlm_calculator = "Inspect")
  app$set_inputs(corStruct = "hom")
  app$set_inputs(scdhlm_calculator = "Model")
  app$set_inputs(method = estMethod)
  app$set_inputs(scdhlm_calculator = "Effect size")
  
  Sys.sleep(0.5)
  output <- app$get_value(output = "effect_size_report")
  app_output <- 
    read_html(output) |> 
    html_table(fill = TRUE) |>
    as.data.frame()
  
  app_output <- lapply(app_output, \(x) if (is.numeric(x)) round(x, digits) else x)

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
    ) |> 
    round(digits = 3)
    
  # CI_Laski_RML <- CI_g(Laski_ES_RML, symmetric = TRUE)
  
  expect_equal(app_output_Laski$BC.SMD.estimate, pkg_output_Laski$g_AB)
  expect_equal(app_output_Laski$Std..Error, pkg_output_Laski$SE_g_AB)
  expect_equal(app_output_Laski$Degrees.of.freedom, pkg_output_Laski$df)
  
  
  # Lambert
  app_output_Lambert <- check_readme("Lambert", estMethod = "HPS")
  
  data("Lambert")
  Lambert_academic <- subset(Lambert, measure == "academic response")
  Lambert_ES <- effect_size_ABk(outcome = outcome, treatment = treatment, id = case, 
                                phase = phase, time = time, data = Lambert_academic)
  pkg_output_Lambert <- 
    data.frame(
      g_AB = Lambert_ES$delta_hat,
      SE_g_AB = sqrt(Lambert_ES$V_delta_hat),
      df = Lambert_ES$nu
    ) |> 
    round(digits = 3)
  
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
    ) |> 
    round(digits = 3)
  
  expect_equal(app_output_S$BC.SMD.estimate, pkg_output_S$g_AB)
  expect_equal(app_output_S$Std..Error, pkg_output_S$SE_g_AB)
  expect_equal(app_output_S$Degrees.of.freedom, pkg_output_S$df)
  
})

# test the app output vs syntax output (RML)
check_syntax <- function(data, corStruct = "AR1", varStruct = "hom", digits = 4L) {
  app <- AppDriver$new(appDir, load_timeout = 6e+05)
  
  app$set_inputs(scdhlm_calculator = "Load")
  app$set_inputs(example = data)
  app$set_inputs(scdhlm_calculator = "Inspect")
  app$set_inputs(scdhlm_calculator = "Model")
  # app$set_inputs(degree_base = degree_base) # consider using if
  # app$set_inputs(degree_trt = degree_trt)
  app$set_inputs(corStruct = corStruct)
  app$set_inputs(varStruct = varStruct)
  app$set_inputs(scdhlm_calculator = "Effect size")
  app$set_inputs(scdhlm_calculator = "Syntax for R")
  app$set_inputs(clipbtn = "click")
  
  Sys.sleep(0.5)
  output <- app$get_value(output = "effect_size_report")
  summary_output <- 
    read_html(output) |>
    html_table(fill = TRUE) |>
    as.data.frame() |> 
    subset(select = c(g_AB = BC.SMD.estimate, SE_g_AB = Std..Error, df = Degrees.of.freedom)) |>
    round(digits = digits)

  names(summary_output) <- c("g_AB","SE_g_AB","df")
  
  raw_syntax <- app$get_value(output = "syntax")
  raw_syntax_cut <- sub("summary\\(ES_RML).*", "", raw_syntax)
  code_file <- tempfile(fileext = ".R")
  cat(raw_syntax_cut, file = code_file)
  source(code_file)
  
  pkg_output <- 
    data.frame(
      g_AB = ES_RML$g_AB,
      SE_g_AB = ES_RML$SE_g_AB,
      df = ES_RML$nu
    ) |> 
    round(digits = digits)
  
  return(identical(summary_output, pkg_output))
}

test_that("The summary table output matches the syntax results", {
  skip_on_cran()
  # AlberMorgan
  expect_true(check_syntax("AlberMorgan"))
  expect_true(check_syntax("AlberMorgan", corStruct = "AR1", varStruct = "het"))
  expect_true(check_syntax("AlberMorgan", corStruct = "MA1", varStruct = "hom"))
  expect_true(check_syntax("AlberMorgan", corStruct = "IID", varStruct = "het"))
  
  # BartonArwood
  expect_true(check_syntax("BartonArwood", corStruct = "MA1", varStruct = "hom"))
  expect_true(check_syntax("BartonArwood", corStruct = "IID", varStruct = "hom"))

  # Anglese
  expect_true(check_syntax("Anglese"))
  expect_true(check_syntax("Anglese", corStruct = "AR1", varStruct = "het"))
  expect_true(check_syntax("Anglese", corStruct = "MA1", varStruct = "hom"))
  expect_true(check_syntax("Anglese", corStruct = "IID", varStruct = "hom"))

  # GunningEspie
  expect_true(check_syntax("GunningEspie", corStruct = "AR1", varStruct = "het"))

  # Thiemann2001
  expect_true(check_syntax("Thiemann2001"))
  expect_true(check_syntax("Thiemann2001", corStruct = "AR1", varStruct = "het"))
  expect_true(check_syntax("Thiemann2001", corStruct = "MA1", varStruct = "hom"))
  expect_true(check_syntax("Thiemann2001", corStruct = "MA1", varStruct = "het"))
  expect_true(check_syntax("Thiemann2001", corStruct = "IID", varStruct = "hom"))
  expect_true(check_syntax("Thiemann2001", corStruct = "IID", varStruct = "het"))
  
  # Bryant2018
  expect_true(check_syntax("Bryant2018", corStruct = "AR1", varStruct = "het"))
  expect_true(check_syntax("Bryant2018", corStruct = "MA1", varStruct = "hom"))

})

