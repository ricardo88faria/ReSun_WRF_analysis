library(shiny)
library(shinythemes)
library(shinyBS)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)
library(markdown)
library(httr)
library(reshape2)



load("data/data.RData") # output/
load("data/data_coords.RData")
load("data/data_merge.RData")
source("../matrix_rotation.R")
lati <- lat

#decades <- seq_time
seq_time <- seq(00, 12, by =1)
lon <- -16.7
lat <- 32.7375

#hgt to contour
#Contour <- rasterToContour(raster(hgt, xmn = long[1], xmx = long[length(long)], ymn = lati[1], ymx = lati[length(lati)], CRS('+proj=longlat +datum=WGS84')),maxpixels=100000,nlevels=10)       #toGeoJSON(as.vector(hgt), )
#Contour_Leaflet <- toGeoJSON(Contour)

locs <- locs
d <- rbind(d, subset(d_tot, Month=="TMY_corr"))

rasterOptions(timer = T, progress = "text")
x <- raster_IGPH
#x <- disaggregate(raster_IGPH, fact=c(2, 2), method='bilinear')

# porto santo
ps <- raster_IGPH_merg
#ps <- disaggregate(raster_IGPH_merg, fact=c(2, 2), method='bilinear')

#maxIGPH <- round(max(as.numeric(max_IGPH)+5),-1)

#popup
# test : 
#mat_test <- t(matrix(raster_IGPH$IGPH_tot[], ncol = raster_IGPH@ncols, nrow = raster_IGPH@nrows, byrow = T))

# matrix to data.frame
dimnames(hgt) = list(lati, long)
hgt_melt <- melt(hgt)

popup_test <- paste0("<span style='color: #7f0000'><strong>Valor da Radiação anual com a correção TMY no ponto selecionado.</strong></span>", 
                     "<br><span style='color: salmon;'><strong>Radiação Global [W/m2]: </strong></span>",
                     hgt_melt$value)

# knitr ui
ui <- bootstrapPage( #theme = shinytheme("journal"),
      tags$style( "html, body {width:100%;height:100%}"), #type="text/css",
      leafletOutput("Map", width="100%", height="100%"),
      absolutePanel(top=10, right=10, draggable = F,
                    sliderInput("date", "Mês", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep="", 
                                animate = animationOptions(interval = 2500, loop = F)), # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                    checkboxInput("EMAS", "Mostrar EMAS", TRUE),
                    checkboxInput("legend", "Mostrar legenda", TRUE), # NEW LINE
                    sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.6, step=.1, sep=""), # para opacidade
                    conditionalPanel("input.EMAS == true",
                                     selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
                                     conditionalPanel("input.location !== null && input.location !== ''",
                                                      actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),
                    #actionButton("about", "Sobre", class="btn-block"),
                    
                    style = "opacity: 0.9"
                    #    img(src="MJi_clean.png", height = 70, width = 100)
      ),
      
      absolutePanel(top = 10, left = 50, draggable = TRUE,
                    img(src="LREC.png", height = 70, width = 110),
                    img(src="ReSun.jpg", height = 70, width = 130),
                    style = "opacity: 0.9"
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
                          ))), actionButton("about", "Sobre", class="btn-block")
                          ),
                    style = "opacity: 0.9"
                          ),
      
      bsModal("abouttext", "Sobre o Atlas Solar", "about", 
              HTML(markdownToHTML(fragment.only=TRUE, text=c(
                    "Estre trabalho é realizado em parceria pelo LREC e MJInovação, com a utilização do software WRF e ReSun.
            
                    Autor: Ricardo Faria"
              )))
      ),
      
      bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
              plotOutput("TestPlot"),
              #    plotOutput("TestPlot1"),
              dataTableOutput("TestTable"))
                          )

# knitr server
server <- function(input, output, session) { # added ps for another raster, porto santo
      acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
      
      ras <- reactive({ subset(x, which(seq_time==input$date)) })
      #hgt_polylines <- reactive({ hgt_melt })
      ras_ploy <- reactive({ hgt_melt })
      ras_ps <- reactive({ subset(ps, which(seq_time==input$date)) })
      ras_vals <- reactive({ c(seq(round(min(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), round(max(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), 10)) }) #c(0, maxIGPH)
      
      # color paletes examples
      #colorNumeric(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), na.color="transparent")
      #colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = c(0, ras_vals(), Inf), na.color="transparent", alpha = F)
      
      pal <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = c(-Inf, ras_vals(), Inf), na.color="transparent", alpha = F) }) 
      pal_legend <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = ras_vals(), na.color="transparent", alpha = F) }) 
      
      output$Map <- renderLeaflet({ 
            leaflet() %>% 
                  setView(lon, lat, 10) %>% 
                  addTiles() %>% 
                  #addWMSTiles("http://www.lrec.pt/", attribution = "Mapa Rad. Solar © 2015 - Ricardo Faria, LREC, MJInovação") %>%
                  addPolygons(ras_ploy(), lng = long, lat = lati, opacity=0.9, popup = popup_test) %>%
                  #addPolylines(hgt_polylines(), lng = long, lat = lati, color = "red") %>% 
                  addProviderTiles("OpenTopoMap") %>% # modify thebackground map "Esri.WorldImagery"
                  addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
      })
      
      observe({
            proxy <- leafletProxy("Map")
            # proxy %>% addRasterImage(ras(), colors=pal(), opacity=input$opac) %>% addRasterImage(ras_ps(), colors=pal(), opacity=input$opac)
            proxy %>% removeTiles(layerId="raster") %>% #addPopups(ras()) %>% 
                  addRasterImage(ras(), colors=pal(), opacity=input$opac, layerId="raster") %>% addRasterImage(ras_ps(), colors=pal(), opacity=input$opac, layerId="porto_santo")
      })
      
      
      observe({
            proxy <- leafletProxy("Map")
            proxy %>% clearControls()
            if (input$legend) {
                  proxy %>% addLegend(position="bottomright", pal=pal_legend(), values=ras_vals(), title="Radiação Global [W/m^2]") # values= seq(50, 220, 5)
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
      
      # knitr server pointdata
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
      
      # about text
      #output$aboutText <- renderText({
      #      "Estre trabalho é realizado em parceria pelo LREC e MJInovação, com a utilização do software WRF e ReSun.
      #      Autor: Ricardo Faria
      #      "
      #})
      }

shinyApp(ui, server)
