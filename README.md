# TopTechCompanies

The goal of this project is to take a dataset with information regarding the top 50 Tech Companies in the United States and to create a dashboard to analyze the data. The finished product is a Shiny 
App hosted on shinyapps.io.

### Data Source

Dataset is open source taken from Kaggle. The link to the dataset download can be found here: https://www.kaggle.com/datasets/lamiatabassum/top-50-us-tech-companies-2022-2023-dataset
- Note: Column "HQ State" was renamed to "state" for formatting purposes.
- Note: I converted the names of the states in the "state" column to their associated 2-letter state code. This was done as it is the only data type that plotly allowed to create the United States heatmap.

## Feature Explanation

In this assignment, I explored a dataset that contained information regarding the top 50 Tech companies in 2022-2023. I decided to use the following features to break down the data

 1. I included a slider that allows the user to input the founding year or year range for companies they are interested in exploring. This slicer is particularly useful in analyzing
 the emergence of the tech sectors starting with Roper Technologies with Electronic Components.

2. Under the slider is a textoutput feature that indicates to the user how many companies are available within their selected founding year range. This feature is dynamic and updates
live every time a new year range is selected.

3. The third feature I have selected is a bar chart that displays the annual revenue of the companies in descending order from highest to least within the slicer founding year range.
An additional feature I added is dynamic color coding. This allows the company with the highest revenue to appear in red whilst the other companies are blue. From a visual perspective
the descending order and difference in color hope to make the entire dashboard more aesthetic.

4. The fourth feature is a table that breaks down the companies based on their financial metrics. Normally more financial metrics would be included, but I am limited to the data columns
provided by my dataset. Within this table, the user can also sort the rows' rankings based on the metric they are most interested in.

5. The fifth feature is beneath the table. It is a "download csv" button that allows the user to download the financial metrics in case they would like to conduct further analysis.

6. The sixth feature is a pie chart to the right of the table that showcases a breakdown of the companies by sector. This breakdown allows the user to take note of sector trends based on
the age of the company. A notable trend includes the dominance of the semiconductor sector leading up to the 2000s. Another trend is that out of all 50 companies,
more than 50% are involved in Software related sectors.

7. The seventh feature is a heatmap of the United States of America that breaks down the HQ state of each company. There is an integrated hover feature that allows the user to see the
specific number of companies that are hosted in each state.

## ShinyApp link

https://andyrooooo16.shinyapps.io/TopTechCompanies/
