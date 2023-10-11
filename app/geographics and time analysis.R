

dis <- read.csv("/Users/wshmac/Documents/GitHub/ads-fall2023-project2-group6/data/Disaster_data.csv")
url <- "/Users/wshmac/Documents/GitHub/ads-fall2023-project2-group6/data/MajorDisaster_data.csv"
#geographic analysis 

library(shiny) 
library(usmap)
library(ggplot2)

incidentType <- unique(dis$incidentType)
states <- unique(dis$state)
all <- aggregate(incidentType ~ state, data = dis, FUN = length)
all
specific <- data.frame(incidentType = character(0), state = character(0))

# generate table with info of state, disasters and # of disasters
for (stateName in states) {
  for (disas in incidentType) {
    subset_data <- subset(dis, state == stateName & incidentType == disas)
    count <- nrow(subset_data)
    specific <- rbind(specific, data.frame(incidentType = disas, state = stateName, count = count))
  }
}
specific[is.na(specific$count), "count"] <- 0 # fill na

# define the heatmap function
plot_disaster_heatmap <- function(disaster_name) {
  disasterSub <- subset(specific, incidentType == disaster_name)
  
  usmap_plot <- plot_usmap(data = disasterSub, values = "count") +
    scale_fill_continuous(
      low = "white", high = "red", name = "Count"
    ) +
    labs(
      title = paste("Disaster Heatmap by State -", disaster_name),
      subtitle = paste("Incident Type:", disaster_name),
      fill = "Count"
    )
  print(usmap_plot)
}



ui1 <- fluidPage( 
  titlePanel("Disaster Geographics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", h3("Select box"), 
                  choices = list(
                    'All','Fire', 'Severe Storm', 'Winter Storm', 'Flood', 'Tornado',
                    'Snowstorm', 'Earthquake', 'Biological', 'Hurricane',          
                    'Mud/Landslide', 'Coastal Storm', 'Other', 'Severe Ice Storm',
                    'Dam/Levee Break', 'Tropical Storm', 'Typhoon',
                    'Volcanic Eruption', 'Freezing', 'Toxic Substances', 'Chemical',
                    'Terrorist', 'Drought', 'Human Cause', 'Fishing Losses', 'Tsunami'), selected = 1),
      
      uiOutput("tooltip_text") #add an element to show the UI output
    ),
    mainPanel(
      verbatimTextOutput("disaster_chosen"),
      plotOutput("heatmap")
    )
  )
)


server1 <- function(input, output, session){
  tooltips <- c(
    'All','Fire', 'Severe Storm', 'Winter Storm', 'Flood', 'Tornado',
    'Snowstorm', 'Earthquake', 'Biological', 'Hurricane',          
    'Mud/Landslide', 'Coastal Storm', 'Other', 'Severe Ice Storm',
    'Dam/Levee Break', 'Tropical Storm', 'Typhoon',
    'Volcanic Eruption', 'Freezing', 'Toxic Substances', 'Chemical',
    'Terrorist', 'Drought', 'Human Cause', 'Fishing Losses', 'Tsunami'
  )
  
  output$tooltip_text <- renderUI({
    tooltip <- tooltips[input$select]
    HTML(paste("<div data-toggle='tooltip' data-placement='bottom' title='", tooltip, "'>Hover for info</div>"))
  })
  
  output$heatmap <- renderPlot({
    disaster_type <- input$select
    plot_disaster_heatmap(disaster_type) # Removed print here
  })
  
  output$disaster_chosen <- renderText({
    paste("The disaster you choose is:", input$select) # Use input$select directly
  })
}

#time analysis
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='figs/',
                      echo=T, warning=FALSE, message=FALSE)

if (!require("DT")) install.packages('DT')
if (!require("dtplyr")) install.packages('dtplyr')
if (!require("lubridate")) install.packages('lubridate')
if (!require("ggmap")) install.packages('ggmap')
if (!require("choroplethrZip")) {
  # install.packages("devtools")
  library(devtools)
  install_github('arilamstein/choroplethrZip@v1.5.0')}

library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(ggplot2)
library(shiny)
library(tidyr)


major_disaster_data <- read.csv(url)

#head(major_disaster_data) 
unique(major_disaster_data$incidentType)

#declarationDate -> Date format
major_disaster_data$declarationDate <- as.Date(major_disaster_data$declarationDate, format="%Y-%m-%d")

# Extracting Year and Month
major_disaster_data$Year <- as.numeric(format(major_disaster_data$declarationDate, "%Y"))
major_disaster_data$Month <- as.numeric(format(major_disaster_data$declarationDate, "%m"))

#Clean up Data to do state-wise
state_data <- major_disaster_data %>%
  select(state, declarationDate, Year, Month, disasterNumber, incidentType) %>%
  distinct(disasterNumber, .keep_all = TRUE)

## Yearly and Monthly Analysis

# Define UI
ui2<- fluidPage(
  titlePanel("Major Disaster Data Analysis by State and Disaster"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:",
                  choices = c("All", unique(major_disaster_data$state)),
                  selected = "All"),
      selectInput("disasterType", "Select Disaster Type:",
                  choices = c("All", unique(major_disaster_data$incidentType)),
                  selected = "All"),
      radioButtons("analysisType", "Choose Analysis Type:",
                   choices = c("Yearly Analysis", "Seasonal Analysis", "Average Duration Analysis"),
                   selected = "Yearly Analysis"),
      conditionalPanel(
        condition = "input.analysisType == 'Seasonal Analysis'",
        sliderInput("yearSlider", "Select Year:",
                    min = min(major_disaster_data$Year),
                    max = max(major_disaster_data$Year),
                    value = max(major_disaster_data$Year),
                    step = 1,
                    round = TRUE)
      )
    ),
    mainPanel(
      plotOutput("disasterPlot")
    )
  )
)

# SERVER
server2 <- function(input, output) {
  output$disasterPlot <- renderPlot({
    filtered_data <- major_disaster_data
    
    # Filter by state
    if (input$state != "All") {
      filtered_data <- filtered_data %>% filter(state == input$state)
    }
    
    # Filter by disaster type
    if (input$disasterType != "All") {
      filtered_data <- filtered_data %>% filter(incidentType == input$disasterType)
    }
    
    if (input$analysisType == "Yearly Analysis") {
      yearly_data <- filtered_data %>% group_by(Year) %>% summarise(Count = n())
      ggplot(yearly_data, aes(x=Year, y=Count)) + geom_line() + labs(title=paste("Yearly", input$disasterType, "Declarations"))
      
    } else if (input$analysisType == "Seasonal Analysis") {
      all_months <- data.frame(Month = 1:12)
      monthly_data <- filtered_data %>% 
        filter(Year == input$yearSlider) %>% 
        group_by(Month) %>% 
        summarise(Count = n())
      monthly_data <- all_months %>% 
        left_join(monthly_data, by = "Month") %>% 
        replace_na(list(Count = 0))
      monthly_data$Month <- factor(monthly_data$Month, levels = 1:12, labels = month.abb)
      ggplot(monthly_data, aes(x=Month, y=Count)) + geom_bar(stat="identity") + 
        labs(title=paste("Monthly", input$disasterType, "Declarations for", input$yearSlider))
      
    } else { # Average Duration Analysis
      filtered_data$Duration <- as.Date(filtered_data$incidentEndDate) - as.Date(filtered_data$incidentBeginDate)
      avg_duration <- filtered_data %>%
        group_by(incidentType) %>%
        summarise(AverageDuration = mean(Duration, na.rm = TRUE))
      ggplot(avg_duration, aes(x = incidentType, y = AverageDuration)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Average Duration of Disasters by Type", y = "Average Duration (days)", x = "Disaster Type")
    }
  })
}

stateShapes <- state_boundaries_wgs84

ui3 <- fluidPage(
  titlePanel("Most Frequent Incident Type Analysis by State"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider", "Select Year:",
                  min = min(major_disaster_data$Year),
                  max = max(major_disaster_data$Year),
                  value = min(major_disaster_data$Year),
                  step = 1,
                  round = TRUE,
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map"),
      DTOutput("stateData")
    )
  )
)

server3 <- function(input, output) {
  
  output$map <- renderLeaflet({
    data_year <- major_disaster_data %>%
      filter(Year == input$yearSlider) %>%
      group_by(state) %>%
      count(incidentType, sort = TRUE)
    
    merged_data <- merge(stateShapes, data_year, by.x = "STATE_ABBR", by.y = "state", all.x = TRUE)
    
    pal <- colorFactor(viridis(24), domain = unique(major_disaster_data$incidentType))
    
    leaflet(data = merged_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.583333, lat = 39.833333, zoom = 3.4) %>%
      addPolygons(
        fillColor = ~pal(incidentType),
        fillOpacity = 0.7,
        weight = 1,
        color = "#BDBDC3",
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = merged_data$incidentType, title = "Incident Types", opacity = 0.7, position = "bottomright")
  })
  
  output$stateData <- renderDT({
    data_year <- major_disaster_data %>% 
      filter(Year == input$yearSlider) %>%
      select(state, declarationDate, incidentType) %>%
      arrange(state)
    
    datatable(data_year, filter = 'top')
  })
  
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel('app 1', ui1),
    tabPanel('app 2', ui2),
    tabPanel('app 3', ui3)
  )
)

server <- function(input, output) {
  output$app1 <- renderUI({
    ui1
  })
  
  output$app2 <- renderUI({
    ui2
  })
  
  output$app3 <- renderUI({
    ui3
  })
  
  # Include the server logic for both apps within the main server function
  server1(input, output)
  server2(input, output)
  server3(input, output)
}

shinyApp(ui, server)
