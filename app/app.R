library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)

months <- list("All Year" = 99,
  "January" = 1,  "February" = 2,  "March"     = 3,
  "April"   = 4,  "May"      = 5,  "June"      = 6,
  "July"    = 7,  "August"   = 8,  "September" = 9,
  "October" = 10, "November" = 11, "December"  = 12)

categories <- list(
  income = c("salary", "other"), 
  savings = c("savings", "investments"), 
  expenses = c("rent", "utilities", "transportation", "groceries", 
    "daily", "entertainment", "medical", "vacation", "personal")
)

dat <- read_csv("budget.csv")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Personal Budget"),
  dashboardSidebar(
    includeCSS("style.css"),
    selectInput("year", "Year: ", c(2015:2018), 2017, selectize = FALSE),
    sidebarMenu(
      selectInput("month", "Month: ", names(months), selected = "All Year", selectize = FALSE),
      actionLink("remove", "Remove detail tabs")
    )
  ),
  dashboardBody(      
    fluidRow(
      valueBoxOutput("incoming"),
      valueBoxOutput("outgoing"),
      valueBoxOutput("left")
    ),
    tabBox(id = "tabs", width = 12,
      tabPanel(title = "Cash flow", value = "main",
        fluidRow(
          box(height = 650, width = 4,
            "Click on a row to see the monthly flow", br(),
            DT::dataTableOutput("main_table")
          ),
          box(height = 450, width = 8,
            "Click on a row to see the monthly flow", br(),
            plotOutput("main_plot")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) { 
  tab_list <- NULL
  
  timeColumn <- reactive({
    if (input$month == "All Year") "year"
    else "month"
  })
  
  timeValue <- reactive({
    if (input$month == "All Year") input$year
    else input$month
  })
  
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
  
  output$incoming <- renderValueBox({
    sub_dat <- subsetData("income")
    val <- prettyNum(sub_dat$total[1], big.mark = ",")
    valueBox(value = glue("{val} €"), subtitle = "Incoming", color = "navy")
  })
  
  output$outgoing <- renderValueBox({
    sub_dat <- subsetData("expenses")
    val <- prettyNum(sub_dat$total[1], big.mark = ",")
    valueBox(value = glue("{val} €"), subtitle = "Outgoing", color = "blue")
  })
  
  output$left <- renderValueBox({
    sub_dat <- subsetData(names(categories)) %>%
      group_by(category) %>% summarise(total = mean(total))
    vals <- map(names(categories), function(c) {
      res <- filter(sub_dat, category == c)$total[1]
      names(res) <- c
      res
    }) %>% unlist
    val <- sum(vals["income"], - vals["savings"], - vals["expenses"], na.rm = TRUE) %>%
      prettyNum(big.mark = ",")
    valueBox(glue("{val} €"), subtitle = "$$$ left!", color = "maroon")
  })
  
  output$main_table <- DT::renderDataTable({
    sub_data <- subsettedData() %>% select(year, month, day, amount, category)
    DT::datatable(sub_data, rownames = FALSE)
  })
  
  output$main_plot <- renderPlot({
    colors <- c(
      income = "violetred4", expenses = "darkorchid4", 
      savings = "royalblue")
    
    sub_dat <- subsettedData() %>% 
      group_by(category, subcategory) %>% 
      mutate(total = sum(amount)) %>% 
      summarise(total = mean(total))
    
    ggplot(sub_dat, aes(
      area = total, 
      fill = category,
      label = subcategory,
      subgroup = subcategory
    )) + 
      geom_treemap() + 
      geom_treemap_text() +
      scale_fill_manual(values = colors)
  })
}

shinyApp(ui, server)