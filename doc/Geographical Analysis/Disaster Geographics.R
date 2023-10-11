library(shiny) #调用shiny数据库
library(usmap)
library(ggplot2)

dis <- read.csv("/Users/mac/Documents/GitHub/ads-fall2023-project2-group6/data/Disaster_data.csv")
incidentType <- unique(dis$incidentType)
states <- unique(dis$state)
all <- aggregate(incidentType ~ state, data = dis, FUN = length)
specific <- data.frame(incidentType = character(0), state = character(0))
all
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
  if (disaster_name == "All"){
    usmap_plot <- plot_usmap(data=all, values="incidentType") +
      scale_fill_continuous(
        low = "white", high = "red", name = "Count"
      ) +
      labs(
        title = paste("Disaster Heatmap by All"),
        subtitle = paste("Incident Type: ALL"),
        fill = "Count"
      )}
  else{
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
  }
  print(usmap_plot)
}

ui <- fluidPage( 
  titlePanel("Disaster Geographics"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "slider1", 
        "Disaster Type",
        min = 1,
        max = 26,
        value = 1,
        step = 1
      ),
      uiOutput("tooltip_text") #add an element to show the UI output
  ),
    mainPanel(
      verbatimTextOutput("disaster_chosen"),
      plotOutput("heatmap")
    )
  )
)


server <- function(input, output, session){
  tooltips <- c( # when users put cursor on the tick of the slide, it will prompt the information
    'All','Fire', 'Severe Storm', 'Winter Storm', 'Flood', 'Tornado',
    'Snowstorm', 'Earthquake', 'Biological', 'Hurricane',          
    'Mud/Landslide', 'Coastal Storm', 'Other', 'Severe Ice Storm',
    'Dam/Levee Break', 'Tropical Storm', 'Typhoon',
    'Volcanic Eruption', 'Freezing', 'Toxic Substances', 'Chemical',
    'Terrorist', 'Drought', 'Human Cause', 'Fishing Losses', 'Tsunami')
  output$tooltip_text <- renderUI( # show the corresponding info
    {
      disaster_type <- tooltips[input$slider1]
      local({
        tooltip <- disaster_type
        HTML(paste("<div data-toggle='tooltip' data-placement='bottom' title='", disaster_type, "'>Hover for info</div>"))
      })
    }
  )
  
  output$heatmap <- renderPlot({
    disaster_type <- tooltips[input$slider1]
    heatmap_plot <- plot_disaster_heatmap(disaster_type)
    print(heatmap_plot)
  })
  
  output$disaster_chosen <- renderText({
    paste("the disaster you choose is:", tooltips[input$slider1])
    })
}

shinyApp(ui = ui, server = server)
