library(shiny)

source("ui-def.R", local = TRUE)
source("server-def.R", local = TRUE)

shinyApp(ui = ui, server = server)