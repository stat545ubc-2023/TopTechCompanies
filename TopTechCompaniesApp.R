library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(ggplot2)
library(plotly)
library(rsconnect)

Top50USCompanies2022 <- read_csv(
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
# 
# For this assignment, I expanded upon my previous app from Assignment B-3, introducing several enhancements:
#   
#   1. Tabs Addition:
#      I incorporated two tabs. The first, named "Company Comparison," retains features from the previous assignment, offering a general overview of company information. The second tab, 
#      labeled "Financial Analysis," introduces two new scatter plots focused on analyzing the financial health of the companies.
# 
#   2. Enhanced Slicer Functionality:
#     I implemented a new slicer named "Select Companies" for each tab. When users choose a founding year range, this slicer dynamically displays the companies available within that timeframe. 
#     Users can then select specific companies for further exploration, enhancing customization. Notably, I included a "Select All" button for users to conveniently choose all companies within a
#     selected timeframe without the need for individual selections.
# 
#   3. Financial Health Scatter Plots:
#     Under the "Financial Analysis" tab, I created two scatter plots that process multiple columns of data, providing interpretable results for financial health analysis. Leveraging insights from 
#     the ggplot2 course, I incorporated various subfeatures to customize the size, shape, and color of data points.
# 
#   4. Interpretation Assistance:
#     Recognizing the potential complexity of interpreting scatter plots, especially for users without a finance background, I added two textboxes containing detailed descriptions on how to interpret 
#     the results from each scatterplot. This addition aims to enhance user understanding and facilitate meaningful analysis.
#   
#   5. CSS Formatting Integration:
#     As a final touch, I integrated formatting using CSS to enhance the overall user experience (UX/UI). This includes adjustments to fonts, colors, sizing, and layout. These components collectively 
#     contribute to a personalized and user-friendly interface, making navigation more intuitive for users.


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
           .yellow-background {
              background-color: #ffb703 !important;
           }
      ")
    )
  ),
  titlePanel(tags$div("Top 50 Tech Companies", style = "color: #023047; font-weight: bold;")),
  h4(tags$div("Explore metrics of impactful tech companies! - Andrew Feng", style = "color: #ffb703; font-weight: bold;")),
  
  # Create tabs
  tabsetPanel(
    tabPanel("Company Comparison",
             fluidRow(
               column(6, 
                      h4(tags$strong("Overview of Tech Companies. To view graphs:",
                                     tags$span("1. Choose Founding Year Range, 2. Select companies via dropdown menu.", style = "color: red;")
                      ))
               )
             ),
             fluidRow(
               column(
                 6,
                 div(
                   sliderInput(
                     "id_slider",
                     "Founding Year Range",
                     min = 1890,
                     max = 2012,
                     value = c(1990, 2010),
                     width = "100%"
                   ),
                   class = "yellow-background"
                 )
               ),
               column(
                 6,
                 div(
                   selectInput(
                     "company_selector",
                     "Select Companies",
                     choices = c("Select All", unique(Top50USCompanies2022$`Company Name`)),
                     multiple = TRUE,
                     width = "100%"
                   ),
                   class = "yellow-background"
                 )
               )
             ),
             fluidRow(
               column(12, textOutput("result_text"))
             ),
             fluidRow(
               column(12, plotOutput("Revenue"))
             ),
             fluidRow(
               column(
                 4,
                 plotOutput("PieChart"),
                 style = "width: 100%;",  # Adjust as needed
                 align = "center"
               ),
               column(
                 4,
                 plotlyOutput("Map"),
                 style = "width: 100%;",  # Adjust as needed
                 align = "center"
               )
             )
    ),
    
    tabPanel("Financial Analysis",
             fluidRow(
               column(6, 
                      h4(tags$strong("In-depth exploration of data and financial well-being. To view graphs: ", 
                                     tags$span("1. Choose Founding Year Range, 2. Select companies via dropdown menu.", style = "color: red;")
                      ))
               )
             ),
             fluidRow(
               column(6,
                      div(
                        sliderInput(
                          "id_slider_comparison",
                          "Founding Year Range",
                          min = 1890,
                          max = 2012,
                          value = c(1990, 2010),
                          width = "100%"
                        ),
                        class = "yellow-background"
                      )
               ),
               column(6,
                      div(
                        selectInput(
                          "company_selector_comparison",
                          "Select Companies",
                          choices = c("Select All", unique(Top50USCompanies2022$`Company Name`)),
                          multiple = TRUE,
                          width = "100%"
                        ),
                        class = "yellow-background"
                      )
               )
             ),
             fluidRow(
               column(12, textOutput("result_text_comparison"))
             ),
             h4("How to Interpret Integrated Profitability Analysis Graph"),
             fluidRow(
               column(12, uiOutput("scatterplot_description"))
             ),
             h4("How to Interpret Market Cap vs. Revenue Graph"),
             fluidRow(
               column(12, uiOutput("scatterplot2_description"))
             ),
             fluidRow(
               column(12, plotOutput("Revenue_comparison"))
             ),
             fluidRow(
               column(6,
                      plotOutput("scatter_plot_comparison"),
                      style = "width: 100%;",  # Adjust as needed
                      align = "center"
               ),
               column(6,
                      plotOutput("ScatterPlot_comparison"),
                      style = "width: 100%;",  # Adjust as needed
                      align = "center"
               )
             ),
             fluidRow(
               column(12,
                      h5("Raw Company Financial Data"),
                      DTOutput("Table_comparison"),
                      downloadButton("download_csv_comparison", "Download Table as CSV")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Function to filter companies based on year range
  filter_companies <- function(data, year_range) {
    data %>%
      filter(`Founding Year` >= year_range[1] & `Founding Year` <= year_range[2]) %>%
      pull(`Company Name`)
  }
  
  # Function to filter data based on year range and company selector
  filter_data <- function(data, year_range, company_selector) {
    if ("Select All" %in% company_selector) {
      data %>%
        filter(`Founding Year` >= year_range[1] & `Founding Year` <= year_range[2])
    } else {
      data %>%
        filter(
          `Founding Year` >= year_range[1] &
            `Founding Year` <= year_range[2] &
            `Company Name` %in% company_selector
        )
    }
  }
  
  # Function to display result text
  display_results_summary <- function(data) {
    if (nrow(data) > 0) {
      paste("There are", nrow(data), "results.")
    } else {
      "No results for the selected year range"
    }
  }
  
  # Function to render revenue plot
  render_revenue_plot <- function(data) {
    max_revenue_company <- data %>%
      slice_max(order_by = `Annual Revenue 2022-2023 (USD in Billions)`)
    
    ggplot(
      data,
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
      labs(x = "Company Name", y = "Revenue USD in Billions", title = "Annual Revenue 2023 (USD in Billions)")
  }
  
  # Function to render data table
  render_data_table <- function(data) {
    if (nrow(data) > 0) {
      data %>%
        arrange(desc(`Annual Revenue 2022-2023 (USD in Billions)`)) %>%
        select(-Industry, -Sector, -state, -`Founding Year`, -`Stock Name`, -`Employee Size`) %>%
        datatable(
          options = list(
            dom = 't',
            scrollY = 500,
            scrollX = TRUE,
            pageLength = 50
          ),
          style = 'bootstrap'
        )
    } else {
      datatable(data.frame(), options = list(info = FALSE, searching = FALSE))
    }
  }
  
  # Function to render pie chart
  render_pie_chart <- function(data) {
    if (nrow(data) > 0) {
      sector_counts <- data %>%
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
    } else {
      ggplot()
    }
  }
  
  # Function to render map plot
  render_map_plot <- function(data) {
    map_data <- data %>%
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
  }
  
  # Function to render scatter plot
  render_scatter_plot <- function(data) {
    if (nrow(data) > 0) {
      ggplot(
        data,
        aes(
          x = `Annual Revenue 2022-2023 (USD in Billions)`,
          y = `Market Cap (USD in Trillions)`,
          color = `Company Name`
        )
      ) +
        geom_point() +
        labs(
          title = "Market Cap vs. Annual Revenue",
          x = "Annual Revenue (USD in Billions)",
          y = "Market Cap (USD in Trillions)",
          color = "Company Name"
        ) +
        theme_minimal()
    } else {
      ggplot() + theme_void() +
        ggtitle("No results for the selected year range")
    }
  }
  
  # Function to render integrated profitability plot
  render_integrated_profitability_plot <- function(data) {
    ggplot(
      data,
      aes(
        x = `Annual Revenue 2022-2023 (USD in Billions)`,
        y = `Market Cap (USD in Trillions)`,
        size = `Annual Revenue 2022-2023 (USD in Billions)`,
        color = `Employee Size`
      )
    ) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(name = "Annual Revenue") +
      labs(
        title = "Integrated Profitability Analysis",
        x = "Annual Revenue (USD in Billions)",
        y = "Market Cap (USD in Trillions)",
        color = "Employee Size"
      )
  }
  
  # Observer for updating slider values
  observeEvent(input$update_button, {
    updateSliderInput(session, "id_slider", value = input$id_slider)
  })
  
  # Observer for updating company selector based on year range
  observe({
    year_range <- input$id_slider
    selected_companies <- filter_companies(Top50USCompanies2022, year_range)
    updateSelectInput(session, "company_selector", choices = c("Select All", selected_companies))
  })
  
  # Observer for the second tab
  observe({
    year_range_comparison <- input$id_slider_comparison
    selected_companies_comparison <- filter_companies(Top50USCompanies2022, year_range_comparison)
    updateSelectInput(session, "company_selector_comparison", choices = c("Select All", selected_companies_comparison))
  })
  
  # Reactive for common filtered data for both tabs
  filtered_data <- reactive({
    filter_data(Top50USCompanies2022, input$id_slider, input$company_selector)
  })
  
  # Reactive for additional data for the second tab
  filtered_data_comparison <- reactive({
    filter_data(Top50USCompanies2022, input$id_slider_comparison, input$company_selector_comparison)
  })
  
  # Output for the "Company Comparison" tab
  output$result_text <- renderText({
    display_results_summary(filtered_data())
  })
  
  # Output for the "Financial Analysis" tab
  output$result_text_comparison <- renderText({
    display_results_summary(filtered_data_comparison())
  })
  
  # Output for revenue plot
  output$Revenue <- renderPlot({
    render_revenue_plot(filtered_data())
  })
  
  # Output for data table
  output$Table_comparison <- renderDT({
    render_data_table(filtered_data_comparison())
  })
  
  # Output for pie chart
  output$PieChart <- renderPlot({
    render_pie_chart(filtered_data())
  })
  
  # Output for map plot
  output$Map <- renderPlotly({
    render_map_plot(filtered_data())
  })
  
  # Output for scatter plot in the second tab
  output$ScatterPlot_comparison <- renderPlot({
    render_scatter_plot(filtered_data_comparison())
  })
  
  # Reactive for integrated profitability analysis
  integrated_analysis <- reactive({
    summarise(
      filtered_data_comparison(),
      AvgRevenue = mean(`Annual Revenue 2022-2023 (USD in Billions)`),
      AvgMarketCap = mean(`Market Cap (USD in Trillions)`),
      AvgIncomeTax = mean(`Annual Income Tax in 2022-2023 (USD in Billions)`),
      AvgEmployees = mean(`Employee Size`)
    )
  })
  
  # Output for scatter plot in the second tab
  output$scatter_plot_comparison <- renderPlot({
    render_integrated_profitability_plot(filtered_data_comparison())
  })
  
  # Output for scatterplot description
  output$scatterplot_description <- renderUI({
    HTML('<div style="width: 1400px; text-align: left;"><strong>When interpreting an Integrated Profitability Analysis scatterplot, each data point represents a specific company, with the X-axis typically illustrating annual revenue and the Y-axis representing market capitalization. The color of each point signifies employee size, offering insights into workforce scale, while the point\'s size corresponds to annual revenue, reflecting the company\'s financial magnitude. Explore trends, patterns, or clusters in the plot to understand variable relationships. Positive trends indicate a positive correlation, while descending trends suggest a negative correlation. Outliers may represent unique cases. Correlation coefficients quantify relationship strength. Consider the broader business context, industry norms, and objectives. Clear communication is crucial. Now, examining each quadrant: In the top-right corner, a company exhibits high revenue and market capitalization, reflecting robust financial performance. The top-left corner indicates high market value relative to lower revenue, possibly signaling investor optimism or strategic strength. The bottom-right corner suggests significant market value despite lower revenue, possibly due to growth potential or strategic investments. The bottom-left corner represents smaller companies with lower revenue and market capitalization, indicating emerging entities.</strong></div>')
  })
  
  # Output for scatterplot2 description
  output$scatterplot2_description <- renderUI({
    HTML('<div style="width: 1400px; text-align: left;"><strong>The "Market Cap vs. Revenue" graph serves as a powerful tool in financial analysis, illustrating the correlation between a company\'s market capitalization and its annual revenue. In this graphical representation, the top-right quadrant accommodates large, well-established companies boasting both high market capitalization and substantial annual revenue, signifying industry leadership and financial stability. Conversely, the top-left quadrant includes companies with elevated market valuations despite comparatively lower revenue, hinting at investor optimism regarding future growth, brand strength, or strategic positioning. The bottom-right quadrant highlights companies generating considerable revenue yet undervalued by the market, suggesting growth potential. The bottom-left quadrant is populated by smaller or emerging companies in early growth stages, such as startups yet to achieve high revenue or market capitalization. This framework facilitates a comprehensive evaluation of companies, aiding investors in making informed decisions based on financial performance and growth prospects.</strong></div>')
  })
  
}


shinyApp(ui = ui, server = server)
