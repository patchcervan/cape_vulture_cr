
library(shiny)
library(ggplot2)
library(ggmap)
library(gganimate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Vulture visualizer"),
    
    # Code of vulture to visualize
    textInput(inputId = "bird_id", label = "Vulture ID (bird_id):",
              value = ""),
    
    actionButton(inputId = "goBird", label = "Go"),
    
    # Select dates
    dateRangeInput(inputId = "dates", label = "Show period"),
    
    actionButton(inputId = "goDate", label = "Update"),
    
    # Select zoom
    numericInput(inputId = "zoom", label = "Zoom for the map:",
                 value = 7, min = 5, max = 10, step = 1),
    
    # Show available dates
    textOutput("trackingPeriod"),
    
    # Show an animated plot of the vulture movements
    imageOutput("vulturePlot")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Load track
    data <- eventReactive(input$goBird, {
        
        readr::read_rds(paste0("../../data/working/bird_tracks/in_process/", input$bird_id, "_mxt.rds"))
        
    })
    
    output$trackingPeriod <- renderPrint({
        
        datedat <- data()
        
        range(datedat$datetime)
        
        
    })
    
    # Subset date
    subdata <- eventReactive(input$goDate, {
        
        sbdat <- data()
        
    })
    
    output$vulturePlot <- renderImage({
        
        trk <- subdata()
        
        mapbbox <- c(min(trk$lon)-0.1, min(trk$lat)-0.1, max(trk$lon)+0.1, max(trk$lat)+0.1)
        
        basemap <- get_stamenmap(bbox = mapbbox, zoom = input$zoom)
        
        mindate <- as.POSIXct(input$dates[1], format = "%Y-%m-%d")
        maxdate <- as.POSIXct(input$dates[2], format = "%Y-%m-%d")
        
        trk <- trk[which(trk$datetime > mindate & trk$datetime < maxdate),]
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # Plot
        vulplot <- ggmap(basemap) +
            geom_point(data = trk, aes(x = lon, y = lat), inherit.aes = FALSE) +
            transition_time(trk$datetime) +
            shadow_wake(wake_length = 0.1, size = 1.5, wrap = F) +
            ggtitle("Date and time: {frame_time}")
        
        # Animate
        anim_save("outfile.gif", animate(vulplot, detail = 1, nframes = 50))
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )
        
    }, deleteFile = TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
