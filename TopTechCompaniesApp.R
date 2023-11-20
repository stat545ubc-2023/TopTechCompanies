library(shiny)
library(tidyverse)

TTC <- read_csv("https://raw.githubusercontent.com/stat545ubc-2023/TopTechCompanies/main/Top%2050%20US%20Tech%20Companies%202022%20-%202023.csv")

ui <- fluidPage(
  sliderInput("id_slider", "Founding Year Range", min = 1950, max = 2023, 
              value = c(1990, 2010)),
  plotOutput("Revenue"),
  tableOutput("Table")
)

server <- function(input, output) {
  observe(print(input$id_slider))
  
  output$Revenue <- renderPlot({
    TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2]) %>%
      ggplot(aes(x = `Company Name`, y = `Annual Revenue 2022-2023 (USD in Billions)`)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })


  
  output$Table <- renderTable({
    TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2])
  })
}

shinyApp(ui = ui, server = server)

