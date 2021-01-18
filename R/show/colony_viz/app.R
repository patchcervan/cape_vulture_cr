library(shiny)
library(dplyr)
library(magrittr)
library(readr)
library(leaflet)

# Define UI for application
ui <- fluidPage(

    # Show a leaflet output
    leafletOutput("colonyPlot", height = 900)
)

# Define server logic
server <- function(input, output) {

    output$colonyPlot <- renderLeaflet({
        
        # Load colony data --------------------------------------------------------
        
        col_join <- readRDS("col_viz_data.rds")
        
        col_join <- col_join %>% 
            mutate(size = case_when(avg_ad <= 20 ~ 4,
                           avg_ad > 20 & avg_ad <= 100 ~ 6,
                           avg_ad > 100 & avg_ad <= 1000 ~ 8,
                           avg_ad > 1000 ~ 10,
                           TRUE ~ 2))
        
        # Create a palette that maps factor levels to colors
        pal <- colorFactor(c("red", "purple"), domain = c(0, 1))
        
        # Function to modify legend circle size
        addLegendCustom <- function(map, colors, labels, sizes, ...){
            colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
            labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                                     sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                                     labels, "</div>")
            
            return(addLegend(map, colors = colorAdditions, 
                             labels = labelAdditions, ...))
        }
        
        col_join %>% 
            leaflet() %>% 
            addProviderTiles(providers$Stamen.Terrain) %>% 
            addCircleMarkers(label = col_join$name,
                             popup = paste("Name: ",col_join$name, "<br>",
                                           "Type: ", col_join$type, "<br>",
                                           "Avg_adults: ", round(col_join$avg_ad), "<br>",
                                           "Old names: ", col_join$names_old, "<br>"),
                             radius = col_join$size,
                             fillColor = pal(col_join$counted), fillOpacity = 0.5,
                             color = "black", weight = 1, stroke = T, opacity = 1
            ) %>% 
            addLegendCustom(colors = pal(c(0, rep(1,4))), 
                            labels = c("no count", "<20", "20-100", "100-1000", ">1000"), sizes = seq(4, 18, length.out = 5),
                            opacity = 1,
                            position = "bottomright",
                            title = "Adult individuals")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
