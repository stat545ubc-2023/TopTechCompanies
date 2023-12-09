library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(ggplot2)
library(plotly)
library(rsconnect)

Top50USCompanies2022 <-
  read_csv(
    "https://raw.githubusercontent.com/stat545ubc-2023/TopTechCompanies/main/Top%2050%20US%20Tech%20Companies%202022%20-%202023.csv",
    col_types = cols(
      `Company Name` = col_character(),
      `Industry` = col_character(),
      `Sector` = col_character(),
      `state` = col_character(),
      `Stock Name` = col_character(),
      `Founding Year` = col_double(),
      `Annual Revenue 2022-2023 (USD in Billions)` = col_double(),
      `Market Cap (USD in Trillions)` = col_double(),
      `Annual Income Tax in 2022-2023 (USD in Billions)` = col_double()
    )
  )

# Assignment Description:
# In this assignment, I explored a dataset that contained information regarding the top 50 Tech companies in 2022-2023. I decided to use the following features to break down the data
#
# 1. I included a slider that allows the user to input the founding year or year range for companies they are interested in exploring. This slicer is particularly useful in analyzing
# the emergence of the tech sectors starting with Roper Technologies with Electronic Components.
#
# 2. Under the slider is a textoutput feature that indicates to the user how many companies are available within their selected founding year range. This feature is dynamic and updates
# live every time a new year range is selected.
#
# 3. The third feature I have selected is a bar chart that displays the annual revenue of the companies in descending order from highest to least within the slicer founding year range.
# An additional feature I added is dynamic color coding.This allows the company with the highest revenue to appear in red whilst the other companies are blue. From a visual perspective
# the descending order and difference in color hopes to make the entire dashboard more aesthetic.
#
# 4. The fourth feature is a table that breaks down the companies based on their financial metrics. Normally more financial metrics would be included, but I am limited to the data columns
# provided by my dataset. Within this table, the user can also sort the rows' rankings based on the metric they are most interested in.
#
# 5. The fifth feature is beneath the table. It is a "download csv" button that allows the user to download the financial metrics in case they would like to conduct further analysis.
#
# 6. The sixth feature is a pie chart to the right of the table that showcases a breakdown of the companies by sector. This breakdown allows the user to take note of sector trends based on
# the age of the company. A notable trend includes the dominance of the semiconductor sector leading up to the 2000s. Another trend is that out of all 50 companies,
# more than 50% are involved in Software related sectors.
#
# 7. The seventh feature is a heatmap of the United States of America that breaks down the HQ state of each company. There is an integrated hover feature that allows the user to see the
# specific number of companies that are hosted in each state.

Top50USCompanies2022 <-
  read_csv(
    "https://raw.githubusercontent.com/stat545ubc-2023/TopTechCompanies/main/Top%2050%20US%20Tech%20Companies%202022%20-%202023.csv",
    col_types = cols(
      `Company Name` = col_character(),
      `Industry` = col_character(),
      `Sector` = col_character(),
      `state` = col_character(),
      `Stock Name` = col_character(),
      `Founding Year` = col_double(),
      `Annual Revenue 2022-2023 (USD in Billions)` = col_double(),
      `Market Cap (USD in Trillions)` = col_double(),
      `Annual Income Tax in 2022-2023 (USD in Billions)` = col_double()
    )
  )

# Assignment Description:
# In this assignment, I explored a dataset that contained information regarding the top 50 Tech companies in 2022-2023. I decided to use the following features to break down the data
#
# 1. I included a slider that allows the user to input the founding year or year range for companies they are interest in exploring. This slicer is particularly useful in analyzing
# the emergence of the tech sectors starting with Roper Technologies with Electronic Components.
#
# 2. Under the slider is a textoutput feature that indicates to the user how many companies are available within their selected founding year range. This feature is dynamic and updates
# live every time a new year range is selected.
#
# 3. The third feature I have selected is a bar chart that displays the annual revenue of the companies in descending order from highest to least within the slicer founding year range.
# An additional feature I added is the dynamic color coding.This allows the company with the highest revenue to appear in red whilst the other companies are blue. From a visual perspective
# the descending order and difference in color hopes to make the entire dashboard more aesthetic.
#
# 4. The fourth feature is a table that breaks down the companies based on their financial metrics. Normally more financial metrics would be included, but I am limited to the data columns
# provided by my dataset. Within this table, the user can also sort the rows' rankings based on the metric they are most interested in.
#
# 5. The fifth feature is beneath the table. It is a "download csv" button that allows the user to download the financial metrics in case they would like to conduct further analysis.
#
# 6. The sixth feature is a pie chart to the right of the table that showcases a breakdown of the companies by sectors. This breakdown allows the user to take note of sector trends based on
# the age of the company. A notable trend includes the dominance of the semiconductor sector leading up to the 2000s. Another trend is that out of all 50 companies,
# more than 50% are involved in Software related sectors.
#
# 7. The seventh feature is a heatmap of the United States of American that breaks down the HQ state of each company. There is an integrated hover feature that allows the user to see the
# specific number of companies that are hosted in each state.


ui <- fluidPage(
  titlePanel("Top 50 Tech Companies"),
  h4(
    "Use this App to explore various metrics pertaining to the most impactful tech companies in the world! - Andrew Feng"
  ),
  sliderInput(
    "id_slider",
    "Founding Year Range",
    min = 1890,
    max = 2023,
    value = c(1990, 2010),
    width = "100%"
  ),
  textOutput("result_text"),
  fluidRow(column(12,
                  plotOutput("Revenue"))),
  h5(
    "Company financial performance metrics"
  ),
  fluidRow(
    column(
      6,
      DTOutput("Table"),
      downloadButton("download_csv", "Download Table as CSV")
    ),
    column(3,
           plotOutput("PieChart"),
           style = "width: 40%;",
           align = "center"),
    column(3,
           plotlyOutput("Map"),
           style = "width: 40%;",
           align = "center")
  )
)

# Define the server function
server <- function(input, output) {
  observe(print(input$id_slider))
  
  filtered_data <- reactive({
    Top50USCompanies2022 %>%
      filter(`Founding Year` >= input$id_slider[1] &
               `Founding Year` <= input$id_slider[2])
  })
  
  output$Revenue <- renderPlot({
    max_revenue_company <- filtered_data() %>%
      slice_max(order_by = `Annual Revenue 2022-2023 (USD in Billions)`)
    
    ggplot(
      filtered_data(),
      aes(
        x = reorder(`Company Name`, -`Annual Revenue 2022-2023 (USD in Billions)`),
        y = `Annual Revenue 2022-2023 (USD in Billions)`,
        fill = factor(
          `Company Name` == max_revenue_company$`Company Name`,
          levels = c(FALSE, TRUE)
        )
      )
    ) +
      geom_col() +
      scale_fill_manual(
        values = c("#2b2d42", "#d90429"),
        name = "Company",
        labels = c("Other Companies", "Top Revenue Company")
      ) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      guides(fill = guide_legend(override.aes = list(alpha = 1))) +
      labs(x = "Company Name", y = "Revenue USD in Billions", title = "Annual Revenue 2023 (USD in Billions)")   # Add this line to set x-axis label
  })
  
  
  output$Table <- renderDT({
    filtered_data() %>%
      arrange(desc(`Annual Revenue 2022-2023 (USD in Billions)`)) %>%
      select(-Industry, -Sector, -state, -`Founding Year`, -`Stock Name`, -`Employee Size`) %>%
      datatable(
        options = list(
          dom = 't',
          scrollY = 500,
          scrollX = TRUE,
          pageLength = 50
        ),
        style = 'bootstrap')
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
    }
  )
  
  output$PieChart <- renderPlot({
    sector_counts <- filtered_data() %>%
      count(Sector) %>%
      mutate(percent = n / sum(n) * 100)
    
    ggplot(sector_counts,
           aes(
             x = "",
             y = n,
             fill = Sector,
             label = scales::percent(percent / 100)
           )) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(percent / 100)), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(
        values = c(
          "Communication Equipments" = "#8ecae6",
          "Software Infrastructure" = "#219ebc",
          "Software Application" = "#023047",
          "Semiconductors" = "#ffb703",
          "Consumer Electronics" = "#fb8500",
          "IT Services" = "#9a130e",
          "Computer Hardware" = "#cb4c07",
          "Electronic Components" = "lightgrey"
        )
      ) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Company breakdown by Sector")
  })
  
  output$Map <- renderPlotly({
    map_data <- filtered_data() %>%
      count(state, name = "Count")
    
    plot_ly(
      data = map_data,
      type = "choropleth",
      locations = ~ state,
      z = ~ Count,
      locationmode = "USA-states",
      colors = c("#2b2d42", "#d90429")
    ) %>%
      colorbar(title = "Count") %>%
      layout(geo = list(scope = 'usa'), title = "Company breakdown by HQ State")
  })
}

shinyApp(ui = ui, server = server)
