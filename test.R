library(shiny)
library(plotly)
library(dplyr)

# Assuming cleanup_country_year and tr_data are already loaded in your environment
# Note: I renamed tr_2019 to a generic tr_data to handle multiple years

ui <- fluidPage(
  titlePanel("Environmental Metrics Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "selected_years",
        label = "Select Years:",
        choices = sort(unique(cleanup_country_year$year)),
        selected = 2019 # Default selection
      )
    ),
    
    mainPanel(
      plotlyOutput("efficiency_plot")
    )
  )
)

server <- function(input, output) {
  
  # Reactive data processing
  filtered_data <- reactive({
    req(input$selected_years) # Ensure something is selected
    
    cleanup_country_year |>
      filter(year %in% input$selected_years) |>
      mutate(
        iso = countrycode::countrycode(country_clean, "country.name", "iso3c", warn = FALSE)
      ) |>
      # Assuming tr_data contains a 'year' column to join correctly
      inner_join(tr_data, by = c("iso" = "country", "year" = "year")) |> 
      filter(
        !is.na(gdp_per_capita_nominal),
        total_volunteers > 0,
        avg_efficiency > 0
      )
  })
  
  output("efficiency_plot") <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~gdp_per_capita_nominal,
      y = ~avg_efficiency,
      color = ~region,
      type = "scatter",
      mode = "markers",
      hovertext = ~paste0(
        "<b>", country_clean, "</b>",
        "<br>Year: ", year,
        "<br>Region: ", region,
        "<br>GDP per capita: $", round(gdp_per_capita_nominal, 0),
        "<br>Total volunteers: ", total_volunteers,
        "<br>Cleanup efficiency: ", round(avg_efficiency, 2)
      ),
      hoverinfo = "text"
    ) |>
      layout(
        title = list(text = "GDP, Volunteers, and Cleanup Efficiency by Region", y = 0.95),
        xaxis = list(title = "GDP per Capita", type = "log"),
        yaxis = list(title = "Cleanup Efficiency", type = "log")
      )
  })
}

shinyApp(ui, server)