library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(lubridate)

final <- st_read("final_dfshp.shp")
Sys.setlocale("LC_TIME", "English")
final <- final %>%
  mutate(JOUR = as.Date(JOUR, format = "%Y-%m-%d"), 
         Month = format(JOUR, "%m"),
         Year = format(JOUR, "%Y"),
         Season = case_when(
           between(as.numeric(format(JOUR, "%m")), 3, 5) ~ "Spring",
           between(as.numeric(format(JOUR, "%m")), 6, 8) ~ "Summer",
           between(as.numeric(format(JOUR, "%m")), 9, 11) ~ "Autumn",
           TRUE ~ "Winter"),
         WeekDay = weekdays(JOUR))

# Create new column 'week_number'
final$week_number <- ceiling(day(final$JOUR) / 7)

# Function distribution of validations per day of the week
calculateDistribution <- function(data, selected_year1, selected_month1, selected_week1, selected_year2, selected_month2, selected_week2) {
  filtered_data1 <- data %>%
    filter(year(JOUR) == selected_year1 & month(JOUR) == selected_month1 & week_number == selected_week1)
  
  filtered_data2 <- data %>%
    filter(year(JOUR) == selected_year2 & month(JOUR) == selected_month2 & week_number == selected_week2)
  
  combined_data <- bind_rows(
    mutate(filtered_data1, Period = "Period 1"),
    mutate(filtered_data2, Period = "Period 2")
  )
  
  sum_by_WeekDay <- combined_data %>%
    mutate(WeekDay = weekdays(as.Date(JOUR))) %>%
    group_by(Period, WeekDay) %>%
    summarise(Sum_NB_VALD = sum(NB_VALD, na.rm = TRUE))
  
  weekdays_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  sum_by_WeekDay$WeekDay <- factor(sum_by_WeekDay$WeekDay, levels = weekdays_order, ordered = TRUE)
  
  return(sum_by_WeekDay)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Shiny Dashboard Example"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Compare 2 periods of time", tabName = "compareTab"),
      menuItem("Get station statistics", tabName = "stationTab"),
      menuItem("Train Station Polygons", tabName = "polygonTab")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab Compare 2 periods of time
      tabItem(tabName = "compareTab",
              titlePanel("Passenger Validations Distribution by WeekDay"),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("year1", "Select Year for Period 1", choices = unique(year(final$JOUR))),
                  selectInput("month1", "Select Month for Period 1", choices = 1:12),
                  selectInput("week1", "Select Week for Period 1", choices = 1:4),
                  
                  selectInput("year2", "Select Year for Period 2", choices = unique(year(final$JOUR))),
                  selectInput("month2", "Select Month for Period 2", choices = 1:12),
                  selectInput("week2", "Select Week for Period 2", choices = 1:4),
                  
                  actionButton("plotBtn", "Generate Plot")
                ),
                
                mainPanel(
                  plotOutput("distributionPlot")
                )
              )
      ),
      
      # Tab station statistics
      tabItem(tabName = "stationTab",
              titlePanel("Station Statistics"),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("station", "Select Station", choices = NULL),
                  selectInput("group_var", "Select Grouping Variable", choices = c("Year", "Month", "Season", "CATEGOR")),
                  actionButton("generatePlotBtn", "Generate Plot")
                ),
                
                mainPanel(
                  plotOutput("plot")
                )
              )
      ),
      
      # Tab map Polygons
      tabItem(tabName = "polygonTab",
              titlePanel("Train Station Polygons"),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("yearInput", "Select Year", choices = unique(final$Year)),
                  selectInput("monthInput", "Select Month", choices = unique(final$Month))
                ),
                
                mainPanel(
                  leafletOutput("map")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Generate plot for Compare 2 periods of time
  generatePlotCompare <- eventReactive(input$plotBtn, {
    selected_year1 <- input$year1
    selected_month1 <- input$month1
    selected_week1 <- input$week1
    
    selected_year2 <- input$year2
    selected_month2 <- input$month2
    selected_week2 <- input$week2
    
    distribution_data <- calculateDistribution(final, selected_year1, selected_month1, selected_week1, selected_year2, selected_month2, selected_week2)
    
    return(distribution_data)
  })
  
  # Render plot
  output$distributionPlot <- renderPlot({
    ggplot(generatePlotCompare(), aes(x = WeekDay, y = Sum_NB_VALD, group = Period, color = Period)) +
      geom_line() +
      geom_text(aes(label = Sum_NB_VALD), vjust = -0.5, hjust = 0.5) +
      labs(title = "Number of Validations Over Time",
           x = "WeekDay",
           y = "Sum of NB_VALD") +
      theme_minimal()
  })
  
  # Generate plot for Get station statistics
  observe({
    updateSelectInput(session, "station", choices = as.character(unique(final$LIBELLE)))
  })
  
  generatePlotStation <- eventReactive(input$generatePlotBtn, {
    filtered_data <- final %>%
      filter(LIBELLE == input$station) %>%
      group_by_at(vars(input$group_var)) %>%
      summarise(Sum_NB_VALDS = sum(NB_VALD))
    
    return(ggplot(filtered_data, aes_string(x = input$group_var, y = "Sum_NB_VALDS")) +
             geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
             labs(x = input$group_var, y = "Sum of NB_VALDS", 
                  title = paste("Sum of NB_VALDS grouped by", input$group_var, "for", input$station)))
  })
  
  # Render plot
  output$plot <- renderPlot({
    if (input$generatePlotBtn > 0) {
      print(generatePlotStation())
    }
  })
  
  # Generate map for Train Station Polygons
  output$map <- renderLeaflet({
    filtered_data <- final[
      final$Year == input$yearInput & final$Month == input$monthInput, ]
    
    myMap <- leaflet() %>%
      addTiles() %>%
      setView(lng = 2.3522, lat = 48.8566, zoom = 6)  # Centered around France
    
    for (i in 1:nrow(filtered_data)) {
      polygon <- filtered_data$geometry[i]
      station_name <- filtered_data$LIBELLE[i]
      vald_value <- filtered_data$NB_VALD[i]
      label_content <- paste("Station:", station_name, " ", "NB_VALD:", vald_value)
      
      myMap <- addPolygons(
        map = myMap,
        data = polygon,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "black",
        weight = 1,
        label = label_content  # Display station name and NB_VALD when hovering over the polygon
      )
    }
    
    myMap
  })
}

#Run
shinyApp(ui, server)