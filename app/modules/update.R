
updateUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("update"), "Update entry", class = "pull-right btn-info"),
    selectInput(ns("tableName"), "Choose a table", character(0)),
    selectInput(ns("col"), "Select the ID column", NULL),
    radioButtons(ns("val"), "Choose ID of entry to update", ""),
    box(title = "Current entry", status = "primary", 
      solidHeader = TRUE, width = 12,
        tableOutput(ns("current"))
    ),
    box(title = "New proposed entry", status = "success", 
      solidHeader = TRUE, width = 12,
        uiOutput(ns("fields"))
    )
  )
}

update <- function(input, output, session, pool, reqTable, reqColInTable, goHome) {
  
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  fields <- reactive({
    reqTable(input$tableName)
    pool %>% tbl(input$tableName) %>% select_(paste0("-", input$col)) %>% 
      head %>% collect %>% lapply(type_sum) %>% unlist
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
    updateRadioButtons(session, "val", choices = allUniqueVals, inline = TRUE)
  })
  
  output$current <- renderTable({
    reqColInTable(input$tableName, input$col)
    req(input$val)
    
    filterVar <- sym(input$col)
    pool %>% tbl(input$tableName) %>% filter(filterVar == input$val)
  })
  
  output$fields <- renderUI({
    fieldNames <- names(fields())
    fieldTypes <- unname(fields())
    selections <- vector("list", length(fieldNames))
    for (i in seq_len(length(fieldNames))) {
      nm <- fieldNames[i]
      id <- paste0("field", nm)
      selections[[i]] <- box(width = 3,
        switch(fieldTypes[i],
          int = numericInput(session$ns(id), nm, NULL),
          chr = textInput(session$ns(id), nm, NULL)
        )
      )
    }
    selections
  })
  
  observeEvent(input$update, {
    entryValues <- data.frame(stringsAsFactors = FALSE,
      lapply(fields(), type.convert)
    )
    
    for (name in names(entryValues)) {
      id <- paste0("field", name)
      
      if (!isTruthy(input[[id]])) {
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
      
      entryValues[name] <- input[[id]]
    }
    
    col <- if (input$col %in% db_query_fields(pool, input$tableName)) {
      input$col
    } else {
       showModal(modalDialog(
          title = "Invalid ID column name",
          "The ID column must be a column of the DB table",
          easyClose = TRUE, footer = NULL
        ))
        return()
    }
    
    sql <- paste0("UPDATE ?table SET ", 
      paste0(names(entryValues), " = ?", names(entryValues), collapse = ", "), 
      " WHERE ", col, " = ?idVal;")
    
    query <- sqlInterpolate(pool, sql, .dots = c(
      list(table = input$tableName), 
      as_list(entryValues),
      list(idVal = input$val)
    ))

    dbExecute(pool, query)
    goHome()
  })
}
