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
#library(reshape2)
#library(sp)
library(ggmap)

#options(shiny.error=browser)

load("data/data.RData") # output/
load("data/data_coords.RData")
load("data/data_merge.RData")
source("../matrix_rotation.R")

lat_res <- abs(round((lat[1] - lat[2])/2, digits = 5))
lon_res <- abs(round((long[1] - long[2])/2, digits = 5))

#decades <- seq_time
seq_time <- seq(00, 12, by =1)
lon_view <- -16.7
lat_view <- 32.7375

#hgt to contour
#Contour <- rasterToContour(raster(hgt, xmn = long[1], xmx = long[length(long)], ymn = lat[1], ymx = lat[length(lat)], CRS('+proj=longlat +datum=WGS84')),maxpixels=100000,nlevels=10)       #toGeoJSON(as.vector(hgt), )
#Contour_Leaflet <- toGeoJSON(Contour)

locs <- locs
d <- rbind(d, subset(d_tot, Month=="TMY_corr"))

rasterOptions(timer = T, progress = "text")
x <- raster_IGPH
#x <- disaggregate(raster_IGPH, fact=c(2, 2), method='bilinear')   # bilinear interpolation to bigger resolution
min_vals <- round(min(raster_IGPH@data@min), digits = -1)
max_vals <- round(max(raster_IGPH@data@max), digits = -1)

# porto santo
ps <- raster_IGPH_merg
#ps <- disaggregate(raster_IGPH_merg, fact=c(2, 2), method='bilinear')   # bilinear interpolation to bigger resolution

#maxIGPH <- round(max(as.numeric(max_IGPH)+5),-1)

# matrix to data.frame do IGPH, hgt
# x <- subset(x, which(seq_time==00))
# IGPH mad & ps
IGPH_melt <- IGPH[,,1]
dimnames(IGPH_melt) = list(lat, long)
IGPH_melt <- melt(IGPH_melt)
colnames(IGPH_melt) <- c("lat", "lon", "val")
# porto santo
IGPH_merg_melt <- IGPH_merg[,,1]
dimnames(IGPH_merg_melt) = list(lat_ps, long_ps)
IGPH_merg_melt <- melt(IGPH_merg_melt)
colnames(IGPH_merg_melt) <- c("lat", "lon", "val")
IGPH_melt <- rbind(IGPH_merg_melt, IGPH_melt)
# HGT
dimnames(hgt) = list(lat, long)
hgt_melt <- melt(hgt)
colnames(hgt_melt) <- c("lat", "lon", "val")
# porto santo
dimnames(hgt_ps) = list(lat_ps, long_ps)
hgt_ps_melt <- melt(hgt_ps)
colnames(hgt_ps_melt) <- c("lat", "lon", "val")
hgt_melt <- rbind(hgt_ps_melt, hgt_melt)

# knitr ui
ui <- bootstrapPage( #theme = shinytheme("journal"),
      tags$style( "html, body {width:100%;height:100%}"), #type="text/css",
      leafletOutput("Map", width="100%", height="100%"),
      absolutePanel(top=10, right=10, draggable = F,
                    sliderInput("date", "Mês", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep="", #format="## Months", timeFormat = "%B",
                                animate = animationOptions(interval = 2500, loop = F)), # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                    checkboxInput("legend", "Legenda adaptada para valores anuais (desabilitar para maior gama valores)", TRUE),
                    checkboxInput("units", "Mudar unidades para [Wh/m^2.dia]", FALSE), 
                    sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.8, step=.1, sep=""), # para opacidade
                    checkboxInput("EMAS", "Mostrar EMAS e pontos do Utilizador", TRUE),
                    conditionalPanel("input.EMAS == true",
                                     selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
                                     conditionalPanel("input.location !== null && input.location !== ''",
                                                      actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),
                    #actionButton("about", "Sobre", class="btn-block"),
                    
                    style = "opacity: 0.9"
                    #    img(src="MJi_clean.png", height = 70, width = 100)
      ),
      
      absolutePanel(top = 10, left = 50, draggable = FALSE,
                    img(src="LREC.png", height = 70, width = 110),
                    img(src="ReSun.jpg", height = 70, width = 130),
                    style = "opacity: 0.9"
      ),
      
      absolutePanel(bottom = 0, right = 10, width = 700, height = "auto", draggable = F, fixed = TRUE,
                    bsCollapse(id = "collapseExample", open = NULL,
                               bsCollapsePanel("Notas:", 
                                               "1- O intante Mês 0, corresponde à média anual do TMY ajustado com as medições.
                                               2- Os valores mensais são representados sem correção, uma vez que a correção foi aplicada à média anual.
                                               3- Os meses que compõem o Ano Meteorológico Tipico a simular com o ReSun tiveram por base 10 anos medidos na sua estimativa.
                                               4- EMAS (Estações Meteorológicas Automáticas).
                                               5- Clique num ponto no mapa para ver mais detalhes sobre esse ponto.",
                                               style = "info", 
                                               actionButton("about", "Sobre", class="btn-block"))
                    ), 
                    style = "opacity: 0.9"
      ),
      
      #absolutePanel(bottom = 0, left = 10, width = 700, height = "auto", draggable = TRUE, fixed = TRUE,
      #              wellPanel(
      #                    HTML(markdownToHTML(fragment.only=TRUE,title = T, text=c(
      #                          "Notas:
      #                          1- O intante Mês 0, corresponde à média anual do TMY ajustado com as medições.
      #                          2- Os valores mensais são representados sem correção, uma vez que a correção foi aplicada à média anual.
      #                          3- Os meses que compõem o Ano Meteorológico Tipico a simular com o ReSun tiveram por base 10 anos medidos na sua estimativa.
      #                          4- EMAS (Estações Meteorológicas Automáticas).
      #                          5- Clique num ponto no mapa para ver mais detalhes sobre esse ponto."
      #                    ))), actionButton("about", "Sobre", class="btn-block")
      #                    ),
      #              style = "opacity: 0.9"
      #                    ),
      
      bsModal("abouttext", "Sobre o Atlas Solar", "about",    
              wellPanel(     #HTML(markdownToHTML(fragment.only=TRUE, text=c(
                    "Este trabalho foi realizado no âmbito do estágio professional de Ricardo Jorge Agrela Faria (LREC), através do código de modelação solar ReSun (PereiraR., 2013).
                    
                    Autor: Ricardo Jorge Agrela Faria", a("GitHub", href="https://github.com/ricardo88faria/ReSun_WRF_analysis")
              )
              ),
      
      bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
              plotOutput("TestPlot"),
              #    plotOutput("TestPlot1"),
              dataTableOutput("TestTable"))
              )

# knitr server
server <- function(input, output, session) { # added ps for another raster, porto santo
      acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
      
      # month
      month_input <- reactive({ 
            months_name[input$date+1]
      })
      
      # rasters
      ras <- reactive({ 
            if (input$units) {
                  24*subset(x, which(seq_time==input$date))
            } else { 
                  subset(x, which(seq_time==input$date))
            }
      })
      #hgt_polylines <- reactive({ hgt_melt })
      #ras_ploy <- reactive({ hgt_melt })
      ras_ps <- reactive({ 
            if (input$units) {
                  24*subset(ps, which(seq_time==input$date)) 
            } else { 
                  subset(ps, which(seq_time==input$date))
            }
      })
      
      # color pallete bins
      minmax_vals <- reactive({ 
            if (input$units) {
                  c(seq(24*min_vals, 24*max_vals)) 
            } else { 
                  c(seq(min_vals, max_vals)) 
            }  
      }) #c(0, maxIGPH)
      minmax_vals_leg <- reactive({ 
            if (input$units) {
                  c(seq(24*min_vals, 24*max_vals, 400)) 
            } else { 
                  c(seq(min_vals, max_vals, 15)) 
            } 
      })
      minmax_vals_anual <- reactive({ 
            if (input$units) {
                  c(seq(24*round(min(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), 24*round(max(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1))) 
            } else { 
                  c(seq(round(min(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), round(max(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1))) 
            } 
      })
      minmax_vals_anual_leg <- reactive({ 
            if (input$units) {
                  c(seq(24*round(min(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), 24*round(max(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), 240)) 
            } else { 
                  c(seq(round(min(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), round(max(raster_IGPH$IGPH_tot[], na.rm = T), digits = -1), 10)) 
            } 
      })
      legend_tytle <- reactive({ 
            if (input$units) {
                  paste("Radiação Global [Wh/m^2.dia]")
            } else { 
                  paste("Radiação Global [W/m^2]")
            } 
      })
      
      # color paletes
      #colorNumeric(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), na.color="transparent")
      #colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = c(0, ras_vals(), Inf), na.color="transparent", alpha = F)
      #colorBin(c('#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'), bins = c(0, 5, 8, 10, 12, 14, 18, 24, 26))
      pal <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals(), bins = c(-Inf, minmax_vals(), Inf), na.color="transparent", alpha = F) }) 
      pal_legend <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals_leg(), bins = minmax_vals_leg(), na.color="transparent", alpha = F) }) 
      pal_anual <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals_anual(), bins = c(-Inf, minmax_vals_anual(), Inf), na.color="transparent", alpha = F) }) 
      pal_legend_anual <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals_anual_leg(), bins = minmax_vals_anual_leg(), na.color="transparent", alpha = F) }) 
      
      output$Map <- renderLeaflet({ 
            leaflet() %>% 
                  setView(lng = lon_view, lat = lat_view, 10) %>% 
                  addTiles(options = providerTileOptions(noWrap = TRUE))  %>% 
                  #addWMSTiles("http://www.lrec.pt/", attribution = "Mapa Rad. Solar © 2015 - Ricardo Faria, LREC, MJInovação") %>%
                  #addPolygons(ras_ploy(), lng = long, lat = lat, opacity=0.9, popup = popup_test) %>%
                  #addPolylines(hgt_polylines(), lng = long, lat = lat, color = "red") %>% 
                  #addProviderTiles("OpenTopoMap") %>% # modify thebackground map "Esri.WorldImagery" "OpenTopoMap"
                  addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
      })
      
      observe({
            proxy <- leafletProxy("Map")
            proxy %>% clearControls()
            if (input$legend) {
                  proxy %>% addLegend(position="bottomleft", pal=pal_legend_anual(), values=minmax_vals_anual_leg(), title=legend_tytle()) %>% # values= seq(50, 220, 5)
                        addRasterImage(ras(), colors=pal_anual(), opacity=input$opac, layerId="raster") %>% 
                        addRasterImage(ras_ps(), colors=pal_anual(), opacity=input$opac, layerId="porto_santo")
            } else {
                  proxy %>% addLegend(position="bottomleft", pal=pal_legend(), values=minmax_vals_leg(), title=legend_tytle()) %>% # values= seq(50, 220, 5)
                        addRasterImage(ras(), colors=pal(), opacity=input$opac, layerId="raster") %>% 
                        addRasterImage(ras_ps(), colors=pal(), opacity=input$opac, layerId="porto_santo")
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
      
      # Observe mouse clicks and add circles
      observeEvent(input$Map_click, {
            # Get the click info
            click <- input$Map_click
            clat <- round(click$lat, digits = 4)
            clng <- round(click$lng, digits = 4)
            
            # global rad 
            rad_val <- subset(IGPH_melt, lat<(clat+lat_res) & lat>(clat-lat_res) & lon<(clng+lon_res) & lon>(clng-lon_res))
            if (input$units) {
                  rad_val <- 24*rad_val
            } else { 
                  rad_val <- rad_val
            } 
            rad_val <- round(rad_val, digits = 3)
            hgt_val <- subset(hgt_melt, lat<(clat+lat_res) & lat>(clat-lat_res) & lon<(clng+lon_res) & lon>(clng-lon_res)) # 0.00045 = resolução da matriz = long[2] - long[1]
            hgt_val <- round(hgt_val, digits = 3)
            
            # monthly rad 
            x_month_rad <- mat_rot(mat_rot(mat_rot(t(as.matrix(as.data.frame(as.matrix(ras())))))))
            dimnames(x_month_rad) = list(lat, long)
            x_month_rad <- melt(x_month_rad)
            colnames(x_month_rad) <- c("lat", "lon", "val")
            #porto santo:
            ps_month_rad <- mat_rot(mat_rot(mat_rot(t(as.matrix(as.data.frame(as.matrix(ras_ps())))))))
            dimnames(ps_month_rad) = list(lat_ps, long_ps)
            ps_month_rad <- melt(ps_month_rad)
            colnames(ps_month_rad) <- c("lat", "lon", "val")
            
            month_rad <- rbind(ps_month_rad, x_month_rad)
            month_rad <- subset(month_rad, lat<(clat+lat_res) & lat>(clat-lat_res) & lon<(clng+lon_res) & lon>(clng-lon_res))
            month_rad <- round(month_rad, digits = 3)
            
            if (input$units) {
                  rad_units <- paste(" [Wh/m^2.dia]")
            } else { 
                  rad_units <- paste(" [W/m2]")
            }
            # html colors #7f0000, #0375DB
            popup_click <- paste0("<span style='color: #0375DB'><strong>Valor da Radiação anual com a correção TMY no ponto selecionado.</strong></span>", 
                                  "<br><span style='color: salmon;'><strong>Radiação Global anual: </strong></span>",
                                  rad_val, rad_units,
                                  "<br><span style='color: salmon;'><strong>Radiação Mensal s/ correção TMY: </strong></span>",
                                  month_rad, rad_units, paste0(" (",month_input(), ")"),
                                  "<br><span style='color: salmon;'><strong>Altura do nível do mar: </strong></span>",
                                  hgt_val, " [m]",
                                  "<br><span style='color: salmon;'><strong>Coordenadas (Long, Lat): </strong></span>",
                                  clng, ", ",clat, " [º]")
            # Add the circle to the map proxy
            # option to hideGroup('circles')
            proxy <- leafletProxy("Map") # use the proxy to save computation
            proxy %>% addCircles(lng=clng, lat=clat, group='locations',
                                 weight=5, radius=100, color='#0375DB', fillColor='#0375DB',
                                 popup=popup_click, fillOpacity=1, opacity=1, stroke=T, layerId="Selected") # se retirar o (layerId="Selected") passa a aceitar varios pontos 
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
      Data <- reactive({ 
            if (input$units) {
                  d$value <- d$value*24
                  d %>% filter(Location==input$location)
            } else { 
                  d %>% filter(Location==input$location)
            } 
            
      })
      Data_tot <- reactive({ d })
      
      output$TestPlot <- renderPlot({ 
            if (input$units) {
                  rad_units <- paste("[Wh/m^2.dia]")
            } else { 
                  rad_units <- paste("[W/m2]")
            }
            ggplot(Data(), aes(x = Month ,y = value, color = Location, group = Location)) + 
                  geom_bar(stat = "identity", position = "dodge") + 
                  scale_fill_brewer(palette="Paired") + 
                  theme_bw() +
                  ggtitle(paste("Radiação mensal sem TMY_corr nas EMAS ", rad_units)) })
      
      #output$TestPlot1 <- renderPlot({ ggplot(Data_tot(), aes(x = Month ,y = value, color = Location, group = Location)) + 
      #            geom_bar(stat = "identity", position = "dodge") + 
      #            scale_fill_brewer(palette="Paired") + 
      #            theme_bw() +
      #            ggtitle("Radiação anual nas EMAS [W/m^2]") })
      
      output$TestTable <- renderDataTable({
            Data()
      }, options = list(pageLength=5))
      
}

shinyApp(ui, server)
