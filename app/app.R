library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
source("helpers.R")

## move this to Google Sheets
dat <- read_csv("www/budget.csv")

sidebar <- dashboardSidebar(
  includeCSS("www/style.css"),
  selectInput("year", "Year: ", years, 2017, selectize = FALSE),
  sidebarMenu(
    selectInput("month", "Month: ", months, selected = "All Year", selectize = FALSE),
    actionLink("remove", "Remove detail tabs")
  )
)

body <- dashboardBody(      
  fluidRow(
    valueBoxOutput("incoming"),
    valueBoxOutput("outgoing"),
    valueBoxOutput("left")
  ),
  tabBox(id = "tabs", width = 12,
    tabPanel(title = "Cash flows", value = "main",
      fluidRow(
        box(height = 650, width = 5, DT::dataTableOutput("main_table")),
        box(height = 450, width = 7, plotOutput("main_plot", click = "main_plot_click"))
      )
    )
  )
)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Personal Budget"),
  sidebar,
  body
)

server <- function(input, output, session) { 
  ## UTILITIES --------------------------------------------------------------------------
  
  # tab_list <- NULL
  
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
  
  
  ## VALUE BOXES ------------------------------------------------------------------------
  
  output$incoming <- renderValueBox({
    sub_dat <- subsetData("income")
    prettifyValueBox(sub_dat$total[1], "Incoming", "navy")
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
    prettifyValueBox(val, "$$$ left!", "maroon")
  })
  
  
  ## MAIN TABLE -------------------------------------------------------------------------
  
  output$main_table <- DT::renderDataTable({
    sub_data <- subsettedData() %>% select(year, month, day, amount, category)
    DT::datatable(sub_data, rownames = FALSE)
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
      footer = actionButton("close_modal",label = "Close")
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$close_modal, {
    selectRows(main_table_proxy, NULL)
    removeModal()
  })
  
  
  ## MAIN PLOT --------------------------------------------------------------------------
  
  treemapified_dat <- function(dat) {
    treemapify(dat, 
      area = "total", fill = "category", label = "subcategory", 
      xlim = c(0, 1), ylim = c(0, 1)
    )
  }
  
  basePlot <- function(dat) {
    ggplot(dat, aes(
      area = total, fill = category, label = subcategory,
      subgroup = subcategory
    ))
  }
  
  output$main_plot <- renderPlot({
    category_dat <- subsettedData() %>%  
      summarise(category_total = mean(amount))
    
    subcategory_dat <- subsettedData() %>% 
      group_by(category, subcategory) %>% 
      mutate(total = sum(amount)) %>% 
      summarise(total = mean(total))
    
    renderLandingPagePlot(basePlot(subcategory_dat))
  })
  
  observeEvent(input$main_plot_click, {
    getClickedPoint <- function(treeDat) {
      click <- input$main_plot_click
      treeDat %>%
        filter(xmin < click$x) %>% filter(xmax > click$x) %>%
        filter(ymin < click$y) %>% filter(ymax > click$y)
    }
    
    category_dat <- subsettedData() %>%  
      summarise(category_total = mean(amount))
    
    subcategory_dat <- subsettedData() %>% 
      group_by(category, subcategory) %>% 
      mutate(total = sum(amount)) %>% 
      summarise(total = mean(total))
    
    dat <- treemapified_dat(subcategory_dat)
    
    print(getClickedPoint(dat))
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)