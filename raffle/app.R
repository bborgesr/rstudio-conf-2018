
library("shiny")
library("shinythemes")
library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

peeps <- gs_title("WSDS2017") %>% gs_read()

ui <- fluidPage(theme = shinytheme("superhero"),
  tags$head(tags$style(HTML("#shiny-modal { top: 100px}"))),
  titlePanel("RRRaffle - WSDS 2017"),
  actionButton("go", "Choose WINNER!!!")
)

server <- function(input, output, session) {
  
  observeEvent(input$go, {
    chosen <- sample.int(nrow(peeps), 1)
    showModal(modalDialog(
      title = "WINNER",
      peeps$`Enter your name.`[chosen]
    ))
  })
}

shinyApp(ui, server)
