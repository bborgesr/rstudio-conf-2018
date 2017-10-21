
library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(pool)
library(rlang)

source("modules/overview.R", local = TRUE)
source("modules/createTable.R", local = TRUE)
source("modules/createEntry.R", local = TRUE)
source("modules/read.R", local = TRUE)
source("modules/update.R", local = TRUE)
source("modules/delete.R", local = TRUE)

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

tbls <- reactiveFileReader(500, NULL, "db.sqlite",
  function(x) db_list_tables(pool)
)

ui <- dashboardPage(
  dashboardHeader(title = "A CRUD app"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Overview", tabName = "overview"),
      menuItem("Create table", tabName = "createTable"),
      menuItem("Create entry", tabName = "createEntry"),
      menuItem("Read", tabName = "read"),
      menuItem("Update entry", tabName = "update"),
      menuItem("Delete", tabName = "delete")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", overviewUI("overview-module")),
      tabItem("createTable", createTableUI("createTable-module")),
      tabItem("createEntry", createEntryUI("createEntry-module")),
      tabItem("read", readUI("read-module")),
      tabItem("update", updateUI("update-module")),
      tabItem("delete", deleteUI("delete-module"))
    )
  )
)

server <- function(input, output, session) {
  goHome <- function() updateTabItems(session, "tabs", "overview")
  
  reqTable <- function(tableName) {
    tbls()
    req(tableName)
    req(tableName %in% db_list_tables(pool))
  }
  
  reqColInTable <- function(tableName, colName) {
    reqTable(tableName)
    req(colName)
    req(colName %in% db_query_fields(pool, tableName))
  }
  
  callModule(overview, "overview-module", pool)
  callModule(createTable, "createTable-module", pool, goHome)
  callModule(createEntry, "createEntry-module", pool, reqTable, goHome)
  callModule(read, "read-module", pool, reqTable, reqColInTable)
  callModule(update, "update-module", pool, reqTable, reqColInTable, goHome)
  callModule(delete, "delete-module", pool, reqTable, reqColInTable, goHome)
}

shinyApp(ui, server)
