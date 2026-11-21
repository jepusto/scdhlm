skip_if_not_installed("mockery")

test_that("shine_scd() validates required packages.", {

  mockery::stub(shine_scd, "check_for_package", function(pkg) pkg)

  expect_error(
    shine_scd(),
    "requires the following packages: shiny, ggplot2, markdown, glue, rclipboard, readxl, janitor",
    fixed = TRUE
  )

  mockery::stub(shine_scd, "check_for_package", function(pkg) {
    if (pkg == "shiny") pkg else NULL
  })

  expect_error(
    shine_scd(),
    "requires the shiny package",
    fixed = TRUE
  )
})

test_that("shine_scd() checks that the app files are available.", {

  skip_on_cran()
  
  mockery::stub(shine_scd, "system.file", "")

  expect_error(
    shine_scd(),
    "Could not find the application directory",
    fixed = TRUE
  )
})

test_that("shine_scd() launches the app with a data frame.", {

  skip_if_not_installed("shiny")

  app <- mockery::mock("scdhlm-app")
  run_app <- mockery::mock()

  mockery::stub(shine_scd, "shiny::shinyApp", app)
  mockery::stub(shine_scd, "shiny::runApp", run_app)

  data_frame <- data.frame(case = "A", outcome = 1)
  shine_scd(dataset = data_frame)

  mockery::expect_called(app, 1)
  mockery::expect_called(run_app, 1)

  app_args <- mockery::mock_args(app)[[1]]
  expect_s3_class(app_args[[1]], "shiny.tag.list")
  expect_identical(names(formals(app_args[[2]])), c("input","output","session"))

  run_args <- mockery::mock_args(run_app)[[1]]
  expect_equal(run_args[[1]], "scdhlm-app")
  expect_equal(run_args$display.mode, "normal")
  expect_true(run_args$launch.browser)
})

test_that("shine_scd() converts tibbles to data frames.", {

  skip_if_not_installed("shiny")

  app <- mockery::mock("scdhlm-app")

  mockery::stub(shine_scd, "shiny::shinyApp", app)
  mockery::stub(shine_scd, "shiny::runApp", mockery::mock())

  tibble_like <- dplyr::tibble(case = "A", outcome = 1)

  shine_scd(dataset = tibble_like)

  mockery::expect_called(app, 1)
})

test_that("shine_scd() reads xlsx datasets.", {

  skip_if_not_installed("shiny")

  read_excel <- mockery::mock(data.frame(case = "A", outcome = 1))

  mockery::stub(shine_scd, "shiny::shinyApp", mockery::mock("scdhlm-app"))
  mockery::stub(shine_scd, "shiny::runApp", mockery::mock())
  mockery::stub(shine_scd, "readxl::read_xlsx", read_excel)

  shine_scd(dataset = "example.xlsx")
  mockery::expect_called(read_excel, 1)
  
})

test_that("shine_scd() reads CSV datasets.", {
  
  skip_if_not_installed("shiny")
  
  read_csv <- mockery::mock(data.frame(case = "A", outcome = 1))

  mockery::stub(shine_scd, "shiny::shinyApp", mockery::mock("scdhlm-app"))
  mockery::stub(shine_scd, "shiny::runApp", mockery::mock())
  mockery::stub(shine_scd, "utils::read.csv", read_csv)

  shine_scd(dataset = "example.csv")
  mockery::expect_called(read_csv, 1)

})

test_that("shine_scd() reads txt datasets.", {
  
  skip_if_not_installed("shiny")
  
  read_table <- mockery::mock(data.frame(case = "A", outcome = 1))
  
  mockery::stub(shine_scd, "shiny::shinyApp", mockery::mock("scdhlm-app"))
  mockery::stub(shine_scd, "shiny::runApp", mockery::mock())
  mockery::stub(shine_scd, "utils::read.table", read_table)

  shine_scd(dataset = "example.txt")
  mockery::expect_called(read_table, 1)

})


test_that("check_for_package() identifies unavailable packages.", {

  unavailable <- paste0("scdhlm_unavailable_", as.integer(Sys.time()))

  expect_equal(check_for_package(unavailable), unavailable)
  expect_null(check_for_package("base"))
})
