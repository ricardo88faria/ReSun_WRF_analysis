library(shiny)
library(shinyBS)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)

#load("../tests_shiny/nwt_testing_subset.RData")
#load("../tests_shiny/nwt_locations.RData")
load("output/data.RData") # output/

#decades <- seq_time
seq_time <- seq(00, 12, by =1)
lon <- -16.9644
lat <- 32.7375

locs <- locs
d <- rbind(d, d_tot)
x <- raster_IGPH


# @knitr ui02
ui <- bootstrapPage(
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width="100%", height="100%"),
  absolutePanel(top=10, right=10,
    sliderInput("date", "Mes", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep=""), # , post=" Mês"
    checkboxInput("EMAS", "Mostrar EMAS", TRUE),
    checkboxInput("legend", "Mostrar legenda", TRUE), # NEW LINE
    conditionalPanel("input.EMAS == true",
      selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
      conditionalPanel("input.location !== null && input.location !== ''",
        actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),
    img(src="logo.png", height = 70, width = 100),
    img(src="ReSun.jpg", height = 70, width = 130)
#    img(src="MJi_clean.png", height = 70, width = 100)
  ),

  bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
    plotOutput("TestPlot"),
#    plotOutput("TestPlot1"),
    dataTableOutput("TestTable")
  )
)

# @knitr server03
server <- function(input, output, session) {
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")

  ras <- reactive({ subset(x, which(seq_time==input$date)) })
  ras_vals <- reactive({ values(ras()) })
  pal <- reactive({ colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), ras_vals(), na.color="transparent") }) #colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), ras_vals(), na.color="transparent")

  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, 11) %>% addTiles() %>%
      addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
  })

  observe({
    proxy <- leafletProxy("Map")
    proxy %>% removeTiles(layerId="rasimg") %>% addRasterImage(ras(), colors=pal(), opacity=0.8, layerId="rasimg")
  })

  observe({
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position="bottomright", pal=pal(), values=ras_vals(), title="Radiação Global [W/m^2]")
    }
  })

  # show or hide location markers
  observe({ 
    proxy <- leafletProxy("Map")
    if (input$EMAS) {
      proxy %>% showGroup("locations")
    } else {
      updateSelectInput(session, "location", selected="")
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })

  # update the map markers and view on map clicks
  observeEvent(input$Map_marker_click, { 
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })

  # update the location selectInput on map clicks
  observeEvent(input$Map_marker_click, { 
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })

  # update the map markers and view on location selectInput changes
  observeEvent(input$location, { 
    p <- input$Map_marker_click
    p2 <- subset(locs, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    }
  })

# @knitr server03pointdata
Data <- reactive({ d %>% filter(Location==input$location) })
Data_tot <- reactive({ d })

output$TestPlot <- renderPlot({ ggplot(Data(), aes(x = Month ,y = value, color = Location, group = Location)) + 
            geom_bar(stat = "identity", position = "dodge") + 
            scale_fill_brewer(palette="Paired") + 
            theme_bw() +
            ggtitle("Radiação mensal nas EMAS [W/m^2]") })

#output$TestPlot1 <- renderPlot({ ggplot(Data_tot(), aes(x = Month ,y = value, color = Location, group = Location)) + 
#            geom_bar(stat = "identity", position = "dodge") + 
#            scale_fill_brewer(palette="Paired") + 
#            theme_bw() +
#            ggtitle("Radiação anual nas EMAS [W/m^2]") })

output$TestTable <- renderDataTable({
  Data()
}, options = list(pageLength=5))
# @knitr server03remainder
}

shinyApp(ui, server)
