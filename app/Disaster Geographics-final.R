library(shiny) 
install.packages("usmap")
library(usmap)
library(ggplot2)

dis <- read.csv("/Users/hannah/Documents/GitHub/ads-fall2023-project2-group6/data/Disaster_data.csv")
incidentType <- unique(dis$incidentType)
states <- unique(dis$state)
all <- aggregate(incidentType ~ state, data = dis, FUN = length)
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



ui <- fluidPage( 
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


server <- function(input, output, session){
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




shinyApp(ui = ui, server = server)
