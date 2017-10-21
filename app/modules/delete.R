
deleteUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("deleteTable"), "Remove whole table", class = "pull-right btn-danger"),
    selectInput(ns("tableName"), "Choose a table", character(0)),
    actionButton(ns("deleteRows"), "Remove selected rows", class = "pull-right btn-warning"),
    selectInput(ns("col"), "Choose column to filter on", NULL),
    checkboxGroupInput(ns("vals"), "Choose values to include (will remove whole row)"),
    box(title = "Selected rows (to be deleted)", status = "warning", 
      solidHeader = TRUE, width = 12,
        tableOutput(ns("current"))
    )
  )
}

delete <- function(input, output, session, pool, reqTable, reqColInTable, goHome) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  observe({
    reqTable(input$tableName)
    cols <- db_query_fields(pool, input$tableName)
    updateSelectInput(session, "col", choices = cols)
  })
  
  observe({
    reqColInTable(input$tableName, input$col)
    req(db_query_rows(pool, input$tableName) > 0)
    
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(input$col))
    allUniqueVals <- unique(df[[input$col]])
    updateCheckboxGroupInput(session, "vals", choices = allUniqueVals, inline = TRUE)
  })
  
  output$current <- renderTable({
    reqTable(input$tableName)
    req(input$col)
    req(input$vals)
    
    filterVar <- sym(input$col)
    pool %>% tbl(input$tableName) %>% filter(filterVar %in% input$vals)
  })
  
  observeEvent(input$deleteTable, {
    dbRemoveTable(pool, input$tableName)
    goHome()
  })
  
  observeEvent(input$deleteRows, {
    
    col <- if (input$col %in% db_query_fields(pool, input$tableName)) {
      input$col
    } else {
       showModal(modalDialog(
          title = "Invalid column name",
          "The selected column must be a column of the DB table",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    df <- as_data_frame(pool %>% tbl(input$tableName) %>% select(col))
    allUniqueVals <- unique(df[[col]])
    results <- lapply(as_list(input$vals), `%in%`, allUniqueVals)
    
    vals <- if (all(results)) {
      if (is(df[["count"]], "integer")) input$vals
      else lapply(input$vals, sql_escape_string, con = pool)
    } else {
       showModal(modalDialog(
          title = "Invalid column values",
          "The selected values do not exist in the selected table column",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    sql <- paste0("DELETE FROM ?table WHERE ", col, " IN (",
      paste0(vals, collapse = ", "), ");")

    query <- sqlInterpolate(pool, sql, table = input$tableName)
    
    dbExecute(pool, query)
    goHome()
  })
}
