## DEPENDENCIES -------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(DT)
library(googlesheets)
library(colourpicker)
source("helpers.R")

## DATA IMPORT (googlesheets) -----------------------------------------------------------
dat <- gs_title("rstudio_conf_2018_BUDGET") %>% gs_read()

## UI -----------------------------------------------------------------------------------
## |__sidebar ---------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  includeCSS("www/style.css"),
  sidebarMenu(id = "sidebar_menu",
    selectInput("year", "Year: ", years, 2017, selectize = FALSE),
    selectInput("month", "Month: ", months, selected = "All Year", selectize = FALSE),
    hr(), 
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Studio", tabName = "studio", icon = icon("th")),
    hr(), 
    uiOutput("menuItem_specific_ui")
  )
)

## |__body ------------------------------------------------------------------------------
body <- dashboardBody(
  ## |_____persistent info --------------------------------------------------------------
  fluidRow(
    column(8, helpText("Repo: github.com/bborgesr/rstudio-conf-2018")),
    column(4, helpText("Data viewable at: goo.gl/aFLbFz"))
  ),
  fluidRow(
    div(valueBoxOutput("incoming"), class = "special"),
    div(valueBoxOutput("outgoing"), class = "special"),
    div(valueBoxOutput("left"), class = "special")
  ),
  tabItems(
    ## |_____dashboard tab --------------------------------------------------------------
    tabItem("dashboard",
      tabBox(id = "tabs", width = 12,
        tabPanel(title = "Cash flows", value = "main",
          fluidRow(
            box(width = 6, DT::dataTableOutput("main_table")),
            box(width = 6, plotOutput("main_plot", click = "main_plot_click"))
          )
        )
      )
    ),
    ## |_____studio tab -----------------------------------------------------------------
    tabItem("studio",
      helpText("A space to dynamically create value boxes corresponding",
        "to an aggregate summary of the data by subcategory for the selected", 
        "period of time."),
      fluidRow(
        column(3, 
          selectInput("studio_subcategory", "Choose a subcategory", unique(dat$subcategory))
        ),
        column(2, 
          numericInput("studio_amount", "Choose an amount", 0)
        ),
        column(2, 
          radioButtons("studio_amount_direction", "Amount qualifier", c("Over", "Under"))
        ),
        column(2, 
          selectInput("studio_function", "Choose a function", 
            c("sum", "mean", "count", "min", "max"))
        ),
        column(3, 
          colourInput("studio_box_color", "Choose a color", palette = "limited",
            allowedCols = c(
              "#db4c3f", "#f19b2c", "#20c1ed", "#1074b5", "#408eba", "#17a55d", "#02203e", "#42cccb", 
              "#419871", "#2afd77", "#fd852f", "#ed25bc", "#605ea6", "#d62161", "#111111"
            ))
        )
      ),
      fluidRow(
        tags$div(id = "placeholder")
      )
    )
  )
)

## |__page ------------------------------------------------------------------
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Personal Budget"), sidebar, body
)


## SERVER -------------------------------------------------------------------------------
server <- function(input, output, session) { 
  
  ## UTILITIES --------------------------------------------------------------------------
  tab_list <- NULL
  
  timeColumn <- reactive({ if (input$month == "All Year") "year" else "month" })
  timeValue <- reactive({ if (input$month == "All Year") input$year else input$month })
  
  subsettedData <- reactive({
    dat %>% filter(rowMeans(is.na(.)) < 1) %>%
      group_by(category) %>%
      filter(year == input$year) %>% 
      filter(timeValue() == !!as.name(timeColumn()))
  })
  
  subsetData <- function(categoryValues) {
    subsettedData() %>% filter(category %in% categoryValues) %>% 
      mutate(total = sum(amount))
  }
  
  output$menuItem_specific_ui <- renderUI({
    if (input$sidebar_menu == "dashboard") {
      actionLink("remove_tabs", "Remove detail tabs")
    } else if (input$sidebar_menu == "studio") {
      tagList(
        actionButton("create_box", "Create new box", class = "color_btn"),
        actionLink("remove_boxes", "Delete dynamic boxes")
      )
    }
  })
  
  
  ## VALUE BOXES ------------------------------------------------------------------------
  output$incoming <- renderValueBox({
    sub_dat <- subsetData("income")
    prettifyValueBox(sub_dat$total[1], "Incoming", "maroon")
  })
  
  output$outgoing <- renderValueBox({
    sub_dat <- subsetData("expenses")
    prettifyValueBox(sub_dat$total[1], "Outgoing", "blue")
  })
  
  output$left <- renderValueBox({
    sub_dat <- subsetData(categories) %>% group_by(category) %>% summarise(total = mean(total))
    vals <- map(categories, function(c) {
      res <- filter(sub_dat, category == c)$total[1]
      names(res) <- c
      res
    }) %>% unlist
    val <- sum(vals["income"], - vals["savings"], - vals["expenses"], na.rm = TRUE)
    prettifyValueBox(val, "$$$ left!", "navy")
  })
  
  
  ## MAIN TABLE -------------------------------------------------------------------------
  output$main_table <- DT::renderDataTable({
    sub_data <- subsettedData() %>% select(year, month, day, amount, category)
    DT::datatable(sub_data, rownames = FALSE, options = list(dom = "tp"))
  }, server = TRUE)
  
  main_table_proxy <- dataTableProxy("main_table")
  
  showTransactionInfo <- function(id) {
    row <- subsettedData()[id, ]
    all <- wellPanel(
      p(tags$b("Date: "), glue("{row$day}, {row$month}, {row$year}")),
      p(tags$b("Amount: "), row$amount),
      p(tags$b("Category: "), row$category),
      p(tags$b("Subcategory: "), row$subcategory),
      if (!is.na(row$origin)) p(tags$b("Origin: "), row$origin),
      if (!is.na(row$description)) p(tags$b("Description: "), row$description)
    )
    return(list(id = row$id, all = all))
  }
  
  observeEvent(input$main_table_rows_selected, {
    info <- showTransactionInfo(input$main_table_rows_selected)
    showModal(modalDialog(
      title = div(tags$b(glue("Transaction #{info$id}")), style = "color: #605ea6;"),
      info$all,
      footer = actionButton("close_modal", label = "Close", class = "color_btn")
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$close_modal, {
    selectRows(main_table_proxy, NULL)
    removeModal()
  })
  
  
  ## MAIN PLOT --------------------------------------------------------------------------
  subcategory_dat <- reactive({
    subsettedData() %>% 
      group_by(category, subcategory) %>% 
      mutate(total = sum(amount)) %>% 
      summarise(total = mean(total))
  })
  
  output$main_plot <- renderPlot({
    base_ggplot <- subcategory_dat() %>% basePlot
    renderLandingPagePlot(base_ggplot)
  })
  
  observeEvent(input$main_plot_click, {
    tree_dat <- subcategory_dat() %>% treemapified_dat
    clicked_square <- getClickedPoint(tree_dat, input$main_plot_click)
    clicked_label <- as.character(clicked_square[1, "label"])
    outputID <- glue("dt-{clicked_label}")
    btnID <- glue("hide-{outputID}")
    
    if (!(clicked_label %in% tab_list)) {
      appendTab(inputId = "tabs",
        tabPanel(clicked_label,
          actionButton(btnID, "Hide this tab", class = "color_btn pull-right"),
          DT::dataTableOutput(outputID)
        )
      )
      tab_list <<- c(tab_list, clicked_label)
    }
    
    output[[outputID]] <- DT::renderDataTable({
      details <- subsettedData() %>% filter(subcategory == clicked_label)
      DT::datatable(details, rownames = FALSE, options = list(dom = "tp"))
    })
    
    showTab(inputId = "tabs", target = clicked_label, select = TRUE)
    
    observeEvent(input[[btnID]],{
      hideTab("tabs", clicked_label)
    }, ignoreInit = TRUE)
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_tabs,{
    tab_list %>% walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  }, ignoreInit = TRUE)
  
  ## STUDIO TAB -------------------------------------------------------------------------
  # actionButton("create_box", "Create new box", class = "create_box"),
  # actionLink("remove_boxes", "Delete dynamic boxes")
  
  studioTabData <- reactive({
    subsettedData() %>% 
      filter(subcategory %in% input$studio_subcategory) %>% 
      filter(studioTabAmountDirection()(amount, input$studio_amount)) %>% 
      mutate(total = studioTabFunction()(amount))
  })
  
  studioTabAmountDirection <- reactive({
    if (input$studio_amount_direction == "Over") `>`
    else if (input$studio_amount_direction == "Under") `<`
  })
  
  studioTabFunction <- reactive({
    if (input$studio_function == "sum") sum
    else if (input$studio_function == "mean") mean
    else if (input$studio_function == "count") count
    else if (input$studio_function == "min") min
    else if (input$studio_function == "max") max
  })
  
  studioBoxColor <- reactive({
    if (input$studio_box_color == "#DB4C3F") "red"
    else if (input$studio_box_color == "#F19B2C") "yellow"
    else if (input$studio_box_color == "#20C1ED") "aqua"
    else if (input$studio_box_color == "#1074B5") "blue"
    else if (input$studio_box_color == "#408EBA") "light-blue"
    else if (input$studio_box_color == "#17A55D") "green"
    else if (input$studio_box_color == "#02203E") "navy"
    else if (input$studio_box_color == "#42CCCB") "teal"
    else if (input$studio_box_color == "#419871") "olive"
    else if (input$studio_box_color == "#2AFD77") "lime"
    else if (input$studio_box_color == "#FD852F") "orange"
    else if (input$studio_box_color == "#ED25BC") "fuchsia"
    else if (input$studio_box_color == "#605EA6") "purple"
    else if (input$studio_box_color == "#D62161") "maroon"
    else if (input$studio_box_color == "#111111") "black"
  })
  
  observeEvent(input$create_box, {
    divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
    divClass <- glue("user-dynamic-box")
    btnID <- glue("remove-{divID}")
    
    sub_dat <- studioTabData()
    val <- prettyNum(sub_dat$total[1], big.mark = ",")
    
    direction <- if (input$studio_amount_direction == "Over") ">" else "<"
    
    insertUI(
      selector = "#placeholder",
      ui = div(id = divID, class = divClass,
        column(4, 
          actionButton(btnID, "X", class = "grey_btn pull-right"),
          valueBox(width = NULL,
            value = glue("{val} €"), 
            subtitle = glue("{input$studio_function} over {input$studio_subcategory} ", 
              "for {direction} {input$studio_amount} €"), 
            color = studioBoxColor()
          )
        )
      )
    )
    
    observeEvent(input[[btnID]], {
      removeUI(glue("#{divID}"))
    }, ignoreInit = TRUE, once = TRUE)
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_boxes,{
    removeUI(".user-dynamic-box", multiple = TRUE)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
