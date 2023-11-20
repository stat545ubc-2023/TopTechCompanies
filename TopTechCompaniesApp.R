library(shiny)
library(tidyverse)

TTC <- read_csv("https://raw.githubusercontent.com/stat545ubc-2023/TopTechCompanies/main/Top%2050%20US%20Tech%20Companies%202022%20-%202023.csv")

ui <- fluidPage(
  sliderInput("id_slider", "Founding Year Range", min = 1950, max = 2023, 
              value = c(1990, 2010), width = "100%"),
  textOutput("result_text"),
  
  fluidRow(
    column(6, 
      plotOutput("Revenue"),
      downloadButton("download_csv", "Download Table as CSV"),
      tableOutput("Table")
    ),
    column(6, 
      plotOutput("PieChart")
    )
  )
)


server <- function(input, output) {
  observe(print(input$id_slider))
  
  output$Revenue <- renderPlot({
    filtered_data <- TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2])
    
    max_revenue_company <- filtered_data %>%
      slice_max(order_by = `Annual Revenue 2022-2023 (USD in Billions)`)
    
    ggplot(filtered_data, aes(
      x = reorder(`Company Name`, -`Annual Revenue 2022-2023 (USD in Billions)`), 
      y = `Annual Revenue 2022-2023 (USD in Billions)`, 
      fill = `Company Name`
    )) +
      geom_col() +
      geom_col(data = max_revenue_company, aes(fill = "Highlight"), color = "red", alpha = 0.6) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      scale_fill_manual(
        values = c("Highlight" = "red", "OtherCompanies" = "#023047", `Company Name` = "#023047")
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
  })
  
  
  
  
  output$Table <- renderTable({
    TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2]) %>%
      arrange(desc(`Annual Revenue 2022-2023 (USD in Billions)`))
  })
  
  output$result_text <- renderText({
    filtered_data <- TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2])
    paste("There are", nrow(filtered_data), "results.")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(TTC %>%
                  filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2]),
                file)
    }
  )
  
  output$PieChart <- renderPlot({
    TTC %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2]) %>%
      count(Sector) %>%
      ggplot(aes(x = "", y = n, fill = Sector)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Communications Equipment" = "#8ecae6", "Software Infrastructure" = "#219ebc", "Software Application" = "#023047", "Semiconductors" = "#ffb703", "Consumer Electronics" = "#fb8500", "IT Services" = "#9a130e", "Computer Hardware" = "#cb4c07" )) +
      coord_polar(theta = "y") +
      theme_void()
  })
}

shinyApp(ui = ui, server = server)
