# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(usmap)

# Pre-process data and load dataset - Do heavy lifting once at startup
accident_data <- read.csv("accident.csv")

# Pre-calculate metrics for the heat map
state_metrics <- accident_data %>%
  group_by(STATENAME) %>%
  summarize(
    accident_rate = first(Acc_Per_100K),  # Assuming these are already per-state
    fatality_rate = first(Fatal_Per_100K),
    response_time = mean(as.numeric(substr(ST_Ave_RT, 1, 2)) * 60 +
                           as.numeric(substr(ST_Ave_RT, 4, 5)) +
                           as.numeric(substr(ST_Ave_RT, 7, 8)) / 60,
                         na.rm = TRUE)
  ) %>%
  ungroup()

# Pre-process unique values for filters
unique_weather <- sort(unique(accident_data$WEATHERNAME))
unique_states <- sort(unique(accident_data$STATENAME))

# Define the UI
ui <- fluidPage(
  titlePanel("Accident Analysis Dashboard"),
  tabsetPanel(
    # Tab 1: State Heat Map
    tabPanel("State Heat Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("mapMetric", "Select Metric:",
                             choices = c("Accident Rate (per 100k)" = "accident_rate",
                                         "Average Response Time (min)" = "response_time",
                                         "Fatality Rate (per 100k)" = "fatality_rate"),
                             selected = "accident_rate")
               ),
               mainPanel(
                 plotOutput("stateMap", height = "600px")
               )
             )
    ),
    
    # Tab 2: Comparative Analysis
    tabPanel("State Comparison",
             sidebarLayout(
               sidebarPanel(
                 h3("Filter Set 1"),
                 selectInput('Fatality_Input_1', 'Select Fatality Count:',
                             choices = c('All Fatalities' = 'All', '1' = 1, '2' = 2, 
                                         '3' = 3, '4' = 4, '5+' = '5+'),
                             selected = 'All'),
                 selectInput('Light_Input_1', 'Select Lighting Condition:',
                             choices = c('All Lightings' = 'All', 'Dawn', 'Daylight', 
                                         'Dusk', 'Dark', 'Unknown'),
                             selected = 'All'),
                 selectInput('State_Input_1', 'Select a State:',
                             choices = c('All States' = 'All', unique_states),
                             selected = 'All'),
                 selectInput('Time_Input_1', 'Select Time of Day:',
                             choices = c('All Times' = 'All', 'Dawn (4am-8am)', 
                                         'Forenoon (8am-12pm)', 'Afternoon (12pm-4pm)',
                                         'Dusk (4pm-8pm)', 'Early Night (8pm-12am)', 
                                         'Late Night (12am-4am)', 'Unknown'),
                             selected = 'All'),
                 selectInput('Month_Input_1', 'Select a Month:',
                             choices = c('All Year' = 'All', month.name),
                             selected = 'All'),
                 selectInput('Weather_Input_1', 'Select Condition:',
                             choices = c('All Weather Conditions' = 'All', unique_weather),
                             selected = 'All'),
                 
                 hr(),
                 
                 h3("Filter Set 2"),
                 selectInput('Fatality_Input_2', 'Select Fatality Count:',
                             choices = c('All Fatalities' = 'All', '1' = 1, '2' = 2,
                                         '3' = 3, '4' = 4, '5+' = '5+'),
                             selected = 'All'),
                 selectInput('Light_Input_2', 'Select Lighting Condition:',
                             choices = c('All Lightings' = 'All', 'Dawn', 'Daylight',
                                         'Dusk', 'Dark', 'Unknown'),
                             selected = 'All'),
                 selectInput('State_Input_2', 'Select a State:',
                             choices = c('All States' = 'All', unique_states),
                             selected = 'All'),
                 selectInput('Time_Input_2', 'Select Time of Day:',
                             choices = c('All Times' = 'All', 'Dawn (4am-8am)',
                                         'Forenoon (8am-12pm)', 'Afternoon (12pm-4pm)',
                                         'Dusk (4pm-8pm)', 'Early Night (8pm-12am)',
                                         'Late Night (12am-4am)', 'Unknown'),
                             selected = 'All'),
                 selectInput('Month_Input_2', 'Select a Month:',
                             choices = c('All Year' = 'All', month.name),
                             selected = 'All'),
                 selectInput('Weather_Input_2', 'Select Condition:',
                             choices = c('All Weather Conditions' = 'All', unique_weather),
                             selected = 'All')
               ),
               mainPanel(
                 plotOutput("histogram_comparison", height = "600px")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create reactive values for caching
  rv <- reactiveValues(
    map_data = NULL,
    filter_data_1 = NULL,
    filter_data_2 = NULL
  )
  
  # Reactive expression for map data - only updates when mapMetric changes
  observe({
    rv$map_data <- state_metrics %>%
      select(state = STATENAME, !!input$mapMetric)
  })
  
  # Render heat map
  output$stateMap <- renderPlot({
    req(rv$map_data)
    
    plot_usmap(data = rv$map_data, values = input$mapMetric, color = "white") +
      scale_fill_viridis_c(
        name = switch(input$mapMetric,
                      accident_rate = "Accidents per 100K",
                      response_time = "Avg Response Time (min)",
                      fatality_rate = "Fatalities per 100K")
      ) +
      labs(title = paste("Geographical Heat Map of",
                         switch(input$mapMetric,
                                accident_rate = "Accidents Per 100K",
                                response_time = "Response Time (min)",
                                fatality_rate = "Fatalities Per 100K"))) +
      theme_minimal()
  })
  
  # Reactive expressions for filtered data
  observe({
    rv$filter_data_1 <- filter_data(accident_data, input, 1)
  })
  
  observe({
    rv$filter_data_2 <- filter_data(accident_data, input, 2)
  })
  
  # Helper function for filtering data
  filter_data <- function(data, input, set) {
    suffix <- as.character(set)
    fatality_input <- input[[paste0('Fatality_Input_', suffix)]]
    light_input <- input[[paste0('Light_Input_', suffix)]]
    state_input <- input[[paste0('State_Input_', suffix)]]
    time_input <- input[[paste0('Time_Input_', suffix)]]
    month_input <- input[[paste0('Month_Input_', suffix)]]
    weather_input <- input[[paste0('Weather_Input_', suffix)]]
    
    data %>%
      filter(
        (fatality_input == 'All' | 
           (fatality_input == '5+' & FATALS >= 5) | 
           (FATALS == as.numeric(fatality_input))) &
          (LGT2_CONDNAME == light_input | light_input == 'All') &
          (STATENAME == state_input | state_input == 'All') &
          (TIMENAME == time_input | time_input == 'All') &
          (MONTHNAME == month_input | month_input == 'All') &
          (WEATHERNAME == weather_input | weather_input == 'All') &
          !is.na(ST_22POP)
      )
  }
  
  # Render histogram comparison
  output$histogram_comparison <- renderPlot({
    req(rv$filter_data_1, rv$filter_data_2)
    
    # Process data for plotting
    plot_data <- bind_rows(
      process_filter_data(rv$filter_data_1, "Filter Set 1"),
      process_filter_data(rv$filter_data_2, "Filter Set 2")
    )
    
    if (nrow(plot_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "No cases found for the selected criteria.",
                   size = 6, color = "red") +
          theme_void()
      )
    }
    
    ggplot(plot_data, aes(x = DAY_WEEKNAME, y = accidents_per_100k, fill = FilterSet)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      scale_fill_manual(values = c("Filter Set 1" = "blue", "Filter Set 2" = "black")) +
      labs(title = "Accident Rate by Day of the Week (Comparison)",
           x = "Day of the Week",
           y = "Accidents per 100,000 Population") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "top"
      )
  })
}

# Helper function to process filter data for plotting
process_filter_data <- function(data, filter_set) {
  state_populations <- data %>%
    select(STATENAME, ST_22POP) %>%
    distinct()
  
  total_population <- sum(state_populations$ST_22POP)
  
  data %>%
    group_by(DAY_WEEKNAME) %>%
    summarize(
      total_accidents = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      accidents_per_100k = total_accidents / total_population * 100000,
      FilterSet = filter_set,
      DAY_WEEKNAME = factor(DAY_WEEKNAME,
                            levels = c("Monday", "Tuesday", "Wednesday",
                                       "Thursday", "Friday", "Saturday", "Sunday"))
    )
}

# Run the application
shinyApp(ui = ui, server = server)
