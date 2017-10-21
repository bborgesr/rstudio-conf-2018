
readUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Table and columns", width = 6, solidHeader = TRUE, status = "primary",
      selectInput(ns("tableName"), "Choose a table", character(0)),
      checkboxGroupInput(ns("select"), "Choose columns to read")
    ),
    box(title = "Rows (optional)", width = 6, solidHeader = TRUE, status = "info",
      selectInput(ns("filter"), "Choose column to filter on", NULL),
      checkboxGroupInput(ns("vals"), "Choose values to include")
    ),
    box(tableOutput(ns("res")), width = 12)
  )
}

read <- function(input, output, session, pool, reqTable, reqColInTable) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateCheckboxGroupInput(session, "select", 
      choices = cols, selected = cols, inline = TRUE)
  })
  
  observe({
    reqTable(input$tableName)
    req(input$select)
    updateSelectInput(session, "filter", choices = input$select)
  })
  
  observe({
    reqColInTable(input$tableName, input$filter)
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(input$filter))
    allUniqueVals <- unique(df[[input$filter]])
    updateCheckboxGroupInput(session, "vals", 
      choices = allUniqueVals, selected = allUniqueVals, inline = TRUE)
  })
  
  output$res <- renderTable({
    reqColInTable(input$tableName, input$filter)
    
    filterVar <- sym(input$filter)
    vals <- input$vals
    
    pool %>% tbl(input$tableName) %>% select(input$select) %>% 
      filter(filterVar %in% vals)
  })
}
