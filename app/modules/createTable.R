
createTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("create"), "Create table", class = "pull-right btn-info"),
    textInput(ns("tableName"), "Table name"),
    numericInput(ns("ncols"), "Number of columns", 1, min = 1),
    uiOutput(ns("cols"))
  )
}

createTable <- function(input, output, session, pool, goHome) {
  
  output$cols <- renderUI({
    input$tableName
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 4, solidHeader = TRUE, status = "primary",
        textInput(session$ns(paste0("colName", i)), "Column name"),
        selectInput(session$ns(paste0("colType", i)), "Column type", 
          c(Integer = "INT", Character = "VARCHAR")
        )
      )
    }
    cols
  })
  
  observeEvent(input$create, {
    if (input$ncols < 1) {
      showModal(modalDialog(
        title = "No columns", 
        "Each table must have one or more columns",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    if (input$tableName %in% c(tbls(), "")) {
      if (input$tableName == "") {
        msg <- "All tables must be named"
      } else {
        msg <- "There's already a table with this name in the DB"
      }
      showModal(modalDialog(
        title = "Invalid table name", msg,
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    finalCols <- character(0)
    for (i in seq_len(input$ncols)) {
      colNameID <- paste0("colName", i)
      colTypeID <- paste0("colType", i)
      colName <- input[[colNameID]]
      colType <- input[[colTypeID]]
      finalCols[colName] <- colType
    }
    
    if (any(names(finalCols) == "")) {
      showModal(modalDialog(
        title = "Invalid column name",
        "All columns must be named",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    db_create_table(pool, input$tableName, finalCols)
    goHome()
  })
}
