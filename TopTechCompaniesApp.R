library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(usmap)
library(ggplot2)

Top50USCompanies2022 <- read_csv("https://raw.githubusercontent.com/stat545ubc-2023/TopTechCompanies/main/Top%2050%20US%20Tech%20Companies%202022%20-%202023.csv",
                                 col_types = cols(
                                   `Company Name` = col_character(),
                                   `Industry` = col_character(),
                                   `Sector` = col_character(),
                                   `HQ State` = col_character(),
                                   `Stock Name` = col_character(),
                                   `Founding Year` = col_double(),
                                   `Annual Revenue 2022-2023 (USD in Billions)` = col_double(),
                                   `Market Cap (USD in Trillions)` = col_double(),
                                   `Annual` = col_double(),
                                   # Add other columns as needed
                                 )
)

ui <- fluidPage(
  sliderInput("id_slider", "Founding Year Range", min = 1890, max = 2023, 
              value = c(1990, 2010), width = "100%"),
  textOutput("result_text"),
  
  fluidRow(
    column(12, 
           plotOutput("Revenue")
    )
  ),
  
  fluidRow(
    column(6, 
           DTOutput("Table"),
           downloadButton("download_csv", "Download Table as CSV")
    ),
    column(6,
           leafletOutput("Map")
    )
  ),
  
  fluidRow(
    column(12, 
           plotOutput("PieChart")
    )
  )
)


server <- function(input, output) {
  observe(print(input$id_slider))
  
  filtered_data <- reactive({
    Top50USCompanies2022 %>%
      filter(`Founding Year` >= input$id_slider[1] & `Founding Year` <= input$id_slider[2])
  })
  
  output$Revenue <- renderPlot({
    max_revenue_company <- filtered_data() %>%
      slice_max(order_by = `Annual Revenue 2022-2023 (USD in Billions)`)
    
    ggplot(filtered_data(), aes(
      x = reorder(`Company Name`, -`Annual Revenue 2022-2023 (USD in Billions)`), 
      y = `Annual Revenue 2022-2023 (USD in Billions)`, 
      fill = factor(`Company Name` == max_revenue_company$`Company Name`, levels = c(FALSE, TRUE))
    )) +
      geom_col() +
      scale_fill_manual(
        values = c("#2b2d42", "#d90429"),
        name = "Company",
        labels = c("Other Companies", "Top Revenue Company")
      ) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
  })
  
  output$Table <- renderDT({
    filtered_data() %>%
      arrange(desc(`Annual Revenue 2022-2023 (USD in Billions)`)) %>%
      datatable(options = list(dom = 't'))
  })
  
  output$result_text <- renderText({
    paste("There are", nrow(filtered_data()), "results.")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    })
  
  output$PieChart <- renderPlot({
    filtered_data() %>%
      count(Sector) %>%
      ggplot(aes(x = "", y = n, fill = Sector)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Communications Equipment" = "#8ecae6", "Software Infrastructure" = "#219ebc", "Software Application" = "#023047", "Semiconductors" = "#ffb703", "Consumer Electronics" = "#fb8500", "IT Services" = "#9a130e", "Computer Hardware" = "#cb4c07" )) +
      coord_polar(theta = "y") +
      theme_void()
  })
  
  output$Map <- renderPlot({
    # Create a data frame with state names
    state_data <- data.frame(state = state.abb, stringsAsFactors = FALSE)
    
    # Merge with the count data
    merged_data <- merge(state_data, filtered_data() %>% count(`HQ State`, name = "Count"), 
                         by.x = "state", by.y = "HQ State", all.x = TRUE)
    
    # Plot the map
    plot_usmap(data = merged_data, values = "Count", color = "red") +
      scale_fill_continuous(name = "Count", label = scales::comma) +
      theme(legend.position = "right")
  })
  }

shinyApp(ui = ui, server = server)
