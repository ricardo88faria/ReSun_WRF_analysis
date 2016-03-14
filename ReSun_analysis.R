#!/usr/bin/env Rscript

#packages:
library(lubridate)
library(ncdf4)
#library(R.matlab)
library(maptools)
library(raster)
library(rts)
library(plotKML)
#library(RNetCDF)
library(ggplot2)
library(plotGoogleMaps)
library(zoo)
library(htmlwidgets)
library(leaflet)
library(devtools)
library(plotly)

#plotly to server username nad api key
Sys.setenv("plotly_username" = "ricardo88faria")
Sys.setenv("plotly_api_key" = "ourxlm26la")
#py <- plotly(username = 'ricardo88faria', key = 'ourxlm26la')


#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
cat("Programado por Ricardo Faria \n
    ")
#####################################

t <- Sys.time()

#source("config.txt")
source("matrix_rotation.R")


#create folders
system("mkdir kmz Images GIFs graphs GoogleMaps widgets")

#cores dos graficos
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")


#open .nc
fileNames <- Sys.glob("input/*.nc")
nc <- nc_open(fileNames)
names(nc$var)               #variav names

#sist. de coordenadas, projecao e coordenadas (N-S, E-O)
proj <- CRS('+proj=longlat +datum=WGS84')
land <- readShapeSpatial("input/map/PRT_adm2.shp", proj4string = proj)


#lat_min <- min(ncvar_get(nc, "YDIM")[,,1])
#lat_max <- max(ncvar_get(nc, names(nc$var)[2])[,,1])
#lat <- unique(as.vector(ncvar_get(nc, "XLAT")[,,1]))
lat_min <- min(ncvar_get(nc)[,,2])
lat_max <- max(ncvar_get(nc)[,,2])
lat <- unique(as.vector(ncvar_get(nc)[,,2]))

#long_min <- min(ncvar_get(nc, "XLONG")[,,1])
#long_max <- max(ncvar_get(nc, names(nc$var)[3])[,,1])
#long <- unique(as.vector(ncvar_get(nc, names(nc$var)[3])[,,1]))
long_min <- min(ncvar_get(nc)[,,1])
long_max <- max(ncvar_get(nc)[,,1])
long <- unique(as.vector(ncvar_get(nc)[,,1]))

#hgt <- ncvar_get(nc, "HGT")[,,1]
hgt <- ncvar_get(nc)[,,3]

nc_close(nc)

ncols <- length(long)
nrows <- length(lat)

#hour_list <- c(seq(from = 1, to = 145, by = 6))
#seq_i <- c(seq(from = 1, to = 145-6, by = 6))
#seq_f <- c(seq(from = 6, to = 145, by = 6))
seq_time <- seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "month")
seq_months <- format(seq_time, format="%b")

variavs <- c("IGPH",
             "IDIR",
             "IDIF",
             "DIFGPH",
             "Kt")

#increase memmory to raster function:
rasterOptions(maxmemory=5e+08,chunksize=5e+07)

max_IGPH <- character(0)
max_IDIR <- character(0)
max_IDIF <- character(0)
max_DIFGPH <- character(0)
max_Kt <- character(0)
IGPH <- c()
IDIR <- c()
IDIF <- c()
DIFGPH <- c()
Kt <- c()
raster_IGPH <- c()
raster_IDIR <- c()
raster_IDIF <- c()
raster_DIFGPH <- c()
raster_Kt <- c()
#ciclo abrir ficheiros
for (i in 1:length(fileNames)){ 
      temp_nc <- nc$filename[i]
      temp_nc <- nc_open(temp_nc)
      
      # retirar valores das variaveis
      IGPH[[i]] <- ncvar_get(nc)[,,8] 
      IDIR[[i]] <- ncvar_get(nc)[,,7]
      IDIF[[i]] <- ncvar_get(nc)[,,6]  
      DIFGPH[[i]] <- ncvar_get(nc)[,,5]  
      Kt[[i]] <- ncvar_get(nc)[,,4] 
      
      # criar raster files
      temp <- raster(mat_rot(mat_rot(mat_rot(t(IGPH[[i]])))), 
                     xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      temp <- crop(temp, extent(land))
      temp <- mask(temp, land)
      raster_IGPH <- c(raster_IGPH, temp)
      print(paste("mes", i, "- 20%"))
      
      temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIR[[i]])))), 
                                 xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      temp <- crop(temp, extent(land))
      temp <- mask(temp, land)
      raster_IDIR <- c(raster_IDIR, temp)
      print(paste("mes", i, "- 40%"))
      
      temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIF[[i]])))), 
                                 xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      temp <- crop(temp, extent(land))
      temp <- mask(temp, land)
      raster_IDIF <- c(raster_IDIF, temp)
      print(paste("mes", i, "- 60%"))
      
      temp <- raster(mat_rot(mat_rot(mat_rot(t(DIFGPH[[i]])))), 
                                 xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      temp <- crop(temp, extent(land))
      temp <- mask(temp, land)
      raster_DIFGPH <- c(raster_DIFGPH, temp)
      print(paste("mes", i, "- 80%"))
      
      temp <- raster(mat_rot(mat_rot(mat_rot(t(Kt[[i]])))), 
                                 xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      temp <- crop(temp, extent(land))
      temp <- mask(temp, land)
      raster_Kt <- c(raster_Kt, temp)
      print(paste("mes", i, "- 100%"))
      
      #retirar valor maximo de radiacao para grafico
      max_IGPH[[i]] <- max(IGPH[[i]])
      max_IDIR[[i]] <- max(IDIR[[i]])
      max_IDIF[[i]] <- max(IDIF[[i]])
      max_DIFGPH[[i]] <- max(DIFGPH[[i]], na.rm = T)
      max_Kt[[i]] <- max(Kt[[i]], na.rm = T)
      
      nc_close(temp_nc)
      
}
rm(temp)

#IGPH <- array(unlist(IGPH), dim=c(ncols, nrows, length(fileNames)))
#IDIR <- array(unlist(IDIR), dim=c(ncols, nrows, length(fileNames)))
#IDIF <- array(unlist(IDIF), dim=c(ncols, nrows, length(fileNames)))
#DIFGPH <- array(unlist(DIFGPH), dim=c(ncols, nrows, length(fileNames)))
#Kt <- array(unlist(Kt), dim=c(ncols, nrows, length(fileNames)))

IGPH <- matrix(IGPH)
IDIR <- matrix(IDIR)
IDIF <- matrix(IDIF)
DIFGPH <- matrix(DIFGPH)
Kt <- matrix(Kt)

#test_rst_stack <- stack(raster_IGPH)
raster_IGPH <- rts(stack(raster_IGPH), seq_time[1:length(fileNames)])
raster_IDIR <- rts(stack(raster_IDIR), seq_time[1:length(fileNames)])
raster_IDIF <- rts(stack(raster_IDIF), seq_time[1:length(fileNames)])
raster_DIFGPH <- rts(stack(raster_DIFGPH), seq_time[1:length(fileNames)])
raster_Kt <- rts(stack(raster_Kt), seq_time[1:length(fileNames)])

#ciclo gerar mapas e kmz
for (j in 1:length(variavs)) {
      
      for (i in 1:length(seq_months)) {
            ##filled contour grafs
            variav_name <- paste0(variavs[j])
            max_axis <- ceiling(as.numeric(max(get(paste0("max_", variavs[j])))))
            
            name_png = paste0("Images/", variavs[j], "_", seq_months[i], ".png")
            png(name_png, width = ncols*14, height = nrows*14, units = "px", res = 500)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
            
            filled.contour(long, lat, t(get(variav_name)[[i]]), asp = 1, color = rgb.palette.rad, levels = seq(0, max_axis, max_axis/10), # nlevels = 400, #axes = F #(12), nlev=13,
                           plot.title = title(main = as.expression(paste("Radiação Solar diária na Ilha da Madeira a", seq_months[i])), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                           plot.axes = {axis(1); axis(2); 
                                 contour(long, lat, t(hgt), add = T, col = terrain.colors(21), lwd=0.4, labcex=0.5, levels = c(.1, seq(min(hgt), max(hgt), length.out = 21)));
                                 grid(lty = 5, lwd = 0.5)},
                           key.title = title(main = as.expression(paste("[W/m^2]"))))
            
            #plot(getMap(resolution = "high"), add = T)
            #contour(long, lat, hgt, add=TRUE, lwd=1, labcex=1, levels=0.99, drawlabels=FALSE, col="grey30")
            
            dev.off()
      }     
            
            ##kmz
            #test <- raster(mat_rot(mat_rot(mat_rot(t(IGPH[[i]])))), 
            #                xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            #proj4string(test) <- CRS("+proj=longlat +datum=WGS84") #proj
            
            #make contourline in kml
            #cut(test, breaks= 10)
            #rasterToPolygons(test, dissolve=T)
            
            #crop map land
            #test <- crop(test, extent(land))
            #test <- mask(test, land)
            #image(test)
            #filledContour(test)
            
            setwd("kmz")
            #system(paste("mkdir", seq_months[i]))
            #setwd(paste0(seq_months[i]))
            system(paste("mkdir", variavs[j]))
            setwd(paste0(variavs[j]))
            
            #KML(test, file = paste("Rad_", as.Date(times[i]), ".kmz", sep = ""), colour = rgb.palette.rad)
            plotKML(obj = test, folder.name = variavs[j], file.name = paste0(variavs[j], "_", seq_months[i]), colour_scale = rgb.palette.rad(400), open.kml = FALSE)
            
            setwd("../../../")
            
       
}

#GIFs
gif_name <- paste("GIFs/", "Rad_", as.Date(times[i]), ".gif", sep="")

system(paste("convert -verbose -resize 30% -delay 80 -loop 0", paste("Images/", "*", sep=""), gif_name))


#GoogleMaps in .html
setwd("GoogleMaps")
plotGoogleMaps(test, filename="rad.html", layerName="Radiação na Madeira", fillOpacity=0.4, strokeWeight=0, colPalette = rgb.palette.rad(30), openMap = FALSE)
setwd("../")


#map raster leaflet #ifelse(x < 0,'red','green')
pal.rad <- colorNumeric(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), values(test),
                        na.color = "transparent")

raster_map <- leaflet() %>% addTiles() %>%
      addRasterImage(test, colors = pal.rad, opacity = 0.4) %>%
      addLegend(pal = pal.rad, values = values(test),
                title = "Radiação Solar")

setwd("widgets")
htmlwidgets::saveWidget(as.widget(raster_map), "raster_map.html")
setwd("../")

#contour plotly
#data <- t(get(variav_name))
#plot_ly(x = long, y = lat,  z = data, type = "contour")

#plotly
widget_1 <- ggplotly(plot_time_ser)
widget_2 <- ggplotly(plot_day_med)
widget_3 <- ggplotly(plot_time_med)

#enviar plotly para servidor
#plotly_POST(plot_time_ser, filename = "rad")

#hgt[ncol(hgt):1, ] # ncol reordena numero esq - direita & nrow reordena numero cima - baixo

z <- t(hgt)
#z[z < 0] <- NA

scene = list(zaxis = list(title = "HGT", range = c(10, 15000)))
topog <- plot_ly(x = long, y = lat, z = z, type = "surface", colors = terrain.colors(20)) %>%
      layout(title = "3D Scatter plot", scene = scene)

setwd("widgets")
htmlwidgets::saveWidget(as.widget(topog), "topo.html")
setwd("../")

#plotly_POST(topog, filename = "Madeira_topo")

for (i in 1:3) {
      
      setwd("widgets")
      htmlwidgets::saveWidget(as.widget(get(paste("widget_", i, sep = ""))), paste("radiacao_", i,".html", sep = ""))
      setwd("../")
      
}



t <- (Sys.time() - t)

cat("Programado por Ricardo Faria \n
    Finalizado em :")

print(t)
