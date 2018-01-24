library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(treemapify)
source("helpers.R")

dat <- read_csv("budget.csv")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Personal Budget"),
  dashboardSidebar(
    includeCSS("style.css"),
    selectInput("year", "Year: ", c(2015:2018), 2017, selectize = FALSE),
    sidebarMenu(
      selectInput("month", "Month: ", months, selected = "All Year", selectize = FALSE),
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
      tabPanel(title = "Cash flows", value = "main",
        fluidRow(
          box(height = 650, width = 5,
            # "Click on a row to see the monthly flow", br(),
            DT::dataTableOutput("main_table")
          ),
          box(height = 450, width = 7,
            # "Click on a row to see the monthly flow", br(),
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
      geom_treemap_text(color = "white", fontface = 2) +
      scale_fill_manual(values = colors) +
      theme(
        legend.key.width = unit(2, "cm"),
        legend.key.size = unit(1, "cm"),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
        legend.title = element_text(
          face = "bold", 
          inherit.blank = TRUE)
      )
  })
}

shinyApp(ui, server)