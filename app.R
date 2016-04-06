library(shiny)
library(shinyBS)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)
library(markdown)


load("output/data.RData") # output/
load("output/data_coords.RData")
load("output/data_merge.RData")

#decades <- seq_time
seq_time <- seq(00, 12, by =1)
lon <- -16.7
lat <- 32.7375

locs <- locs
d <- rbind(d, subset(d_tot, Month=="TMY_corr"))
x <- raster_IGPH
# porto santo
xx <- raster_IGPH_merg

maxIGPH <- round(max(as.numeric(max_IGPH)+5),-1)

# knitr ui02
ui <- bootstrapPage(
      tags$style(type="text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("Map", width="100%", height="100%"),
      absolutePanel(top=10, right=10, draggable = F,
                    sliderInput("date", "Mês", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep="", 
                                animate = animationOptions(interval = 2500, loop = F)), # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                    checkboxInput("EMAS", "Mostrar EMAS", TRUE),
                    checkboxInput("legend", "Mostrar legenda", TRUE), # NEW LINE
                    sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.8, step=.1, sep=""), # para opacidade
                    conditionalPanel("input.EMAS == true",
                                     selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
                                     conditionalPanel("input.location !== null && input.location !== ''",
                                                      actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),
                    style = "opacity: 0.9"
                    #    img(src="MJi_clean.png", height = 70, width = 100)
      ),
      absolutePanel(top = 10, left = 50, draggable = TRUE,
                    img(src="LREC.png", height = 70, width = 110),
                    img(src="ReSun.jpg", height = 70, width = 130),
                    style = "opacity: 0.8"
      ),
      absolutePanel(bottom = 5, left = 5, width = 700, draggable = TRUE,
                    wellPanel(
                          HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                "Notas:
                                * O intante Mês 0, corresponde à média anual do TMY ajustado com as medições.
                                * Os valores mensais são representados sem correção, uma vez que a correção foi aplicada à média anual.
                                * Os meses que compõem o Ano Meteorológico Tipico a simular com o ReSun tiveram por base 10 anos medidos na sua estimativa.
                                * EMAS (Estações Meteorológicas Automáticas).
                                "
                          )))),
                    style = "opacity: 0.8"
                          ),
      
      bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
              plotOutput("TestPlot"),
              #    plotOutput("TestPlot1"),
              dataTableOutput("TestTable")
      )
                          )

# knitr server03
server <- function(input, output, session) { # added xx for another raster, porto santo
      acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
      
      ras <- reactive({ subset(x, which(seq_time==input$date)) })
      ras_x <- reactive({ subset(xx, which(seq_time==input$date)) })
      ras_vals <- reactive({ c(0, seq(50, 220, 5), maxIGPH) }) #c(0, maxIGPH)
      pal <- reactive({ colorNumeric(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), na.color="transparent") }) #colorNumeric(palette = c("#0C2C84", "#41B6C4", "#FFFFCC"), domain = ras_vals(), na.color="transparent")
      
      output$Map <- renderLeaflet({
            leaflet() %>% setView(lon, lat, 10) %>% addTiles() %>%
                  addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
      })
      
      observe({
            proxy <- leafletProxy("Map")
            # proxy %>% addRasterImage(ras(), colors=pal(), opacity=input$opac) %>% addRasterImage(ras_x(), colors=pal(), opacity=input$opac)
            proxy %>% removeTiles(layerId="raster") %>% addRasterImage(ras(), colors=pal(), opacity=input$opac, layerId="raster") %>% addRasterImage(ras_x(), colors=pal(), opacity=input$opac, layerId="porto_santo")
      })
      
      
      observe({
            proxy <- leafletProxy("Map")
            proxy %>% clearControls()
            if (input$legend) {
                  proxy %>% addLegend(position="bottomright", pal=pal(), values=ras_vals(), maxIGPH, title="Radiação Global [W/m^2]") # values= seq(50, 220, 5)
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
      
      # knitr server03pointdata
      Data <- reactive({ d %>% filter(Location==input$location) })
      Data_tot <- reactive({ d })
      
      output$TestPlot <- renderPlot({ ggplot(Data(), aes(x = Month ,y = value, color = Location, group = Location)) + 
                  geom_bar(stat = "identity", position = "dodge") + 
                  scale_fill_brewer(palette="Paired") + 
                  theme_bw() +
                  ggtitle("Radiação mensal sem TMY_corr nas EMAS [W/m^2]") })
      
      #output$TestPlot1 <- renderPlot({ ggplot(Data_tot(), aes(x = Month ,y = value, color = Location, group = Location)) + 
      #            geom_bar(stat = "identity", position = "dodge") + 
      #            scale_fill_brewer(palette="Paired") + 
      #            theme_bw() +
      #            ggtitle("Radiação anual nas EMAS [W/m^2]") })
      
      output$TestTable <- renderDataTable({
            Data()
      }, options = list(pageLength=5))
      # knitr server03remainder
}

shinyApp(ui, server)
