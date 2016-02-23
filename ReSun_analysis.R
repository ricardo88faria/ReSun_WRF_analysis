#!/usr/bin/env Rscript

#packages:
library(lubridate)
library(ncdf4)
#library(rworldmap)
#library(rworldxtra)
library(raster)
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

#create folders
system("mkdir kmz Images GIFs graphs GoogleMaps widgets")

#cores dos graficos
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")


#open .nc
fileNames <- Sys.glob("*.nc")
nc <- nc_open(fileNames)
names(nc$var)               #variav names

#sist. de coordenadas, projecao e coordenadas (N-S, E-O)
proj <- CRS('+proj=longlat +datum=WGS84')

lat_min <- min(ncvar_get(nc, "XLAT")[,,1])
lat_max <- max(ncvar_get(nc, names(nc$var)[2])[,,1])
lat <- unique(as.vector(ncvar_get(nc, "XLAT")[,,1]))

long_min <- min(ncvar_get(nc, "XLONG")[,,1])
long_max <- max(ncvar_get(nc, names(nc$var)[3])[,,1])
long <- unique(as.vector(ncvar_get(nc, names(nc$var)[3])[,,1]))

hgt <- ncvar_get(nc, "HGT")[,,1]

hour_list <- c(seq(from = 1, to = 145, by = 6))
seq_i <- c(seq(from = 1, to = 145-6, by = 6))
seq_f <- c(seq(from = 6, to = 145, by = 6))

times <- c()
max_graph <- c()
rad_hd <- 0
rad_hd_dataf <- data.frame(row.names = seq(1, 24, by=1))
#ciclo abrir ficheiros
for (i in 1:length(fileNames)){ 
      temp_nc <- nc$filename[i]
      temp_nc <- nc_open(temp_nc)
      variav_rad <- ncvar_get(temp_nc, "SWDOWN")  
      
      #test <- ncvar_get(temp_nc, "SWDOWN", start = c(1, 1, 114), count=c(-1, -1, 10))
      #ou usar antes o ncdim_def
      #ou ainda test[lat, long, time]
      
      #list of output dates
      times <- append(times, ncvar_get(temp_nc, names(nc$var)[1])[1])
      
      rad <- 0
      count <- 0
      start <- 0
      #ciclo para is buscar valores de todos os 10 mnts
      for (j in 1:144) {
            #ciclo que so contabiliza espaco temporal com valores de radiacao > 0
            if (min(variav_rad[,,j]) > 1) {
                  
                  rad <- rad + ncvar_get(temp_nc, names(nc$var)[12])[,,j]   #24/144*60 = 10mnts
                  count <- count + 1
                  
            } else {
                  
                  print("nao ha sol nestes 10 minutos")
                  start <- start + 1
                  
            }
            print(paste(fileNames[i], "mnt", count + start))
            
      }
      
      rad_h <- c()
      for (l in 1:24) {
            rad_h <- append(rad_h, median(ncvar_get(temp_nc, names(nc$var)[12])[,,seq_i[l]:seq_f[l]]))
            #rad_h <- (rad_h[,,1] + rad_h[,,2] + rad_h[,,3] + rad_h[,,4] + rad_h[,,5] + rad_h[,,6])/6 # media dessa hora 
            
            #assign(paste("rad_h_", l, sep = ""), rad_h)
            
            count_h <- l
            print(paste(fileNames[i], "hora", l))
      }
      
      #media diaria de contando somente horas com radiacao
      rad = rad/count                 #W/m^2/day
      #rad_h = rad /count          #W/m^2/h
      
      #retirar valor maximo de radiacao para grafico
      max_graph <- append(max_graph, max(rad))
      
      #somar media das horas num vector
      rad_hd <- rad_hd + rad_h
      rad_hd_dataf[, format(as.POSIXct(strptime(times[i], "%Y-%m-%d_%H:%M:%S")), "%Y-%m-%d")] <- rad_h
      
      #for (l in 1:24) {
      #      assign(paste("rad_h_", l, "_", as.Date(times[i]), sep = ""), get(paste("rad_h_", l, sep = "")))
      #}
      
      print(paste("numero de horas de sol =", dhours(count/6), ", primeiro raio de sol às", dhours(start/(6*2)), "hora solar"))
      
      assign(paste("rad_", as.Date(times[i]), sep = ""), rad)    
      #assign(paste("rad_h", i, sep = ""), rad_h) 
      
      nc_close(temp_nc)
      
}

#fazer média da rad das horas somadas dos dias em estudo
rad_hd <- rad_hd/length(fileNames)
rad_hd_dataf$media <- apply(rad_hd_dataf, 1, median)

nc_close(nc)

media_day <- c()
#ciclo para fazer lista da rad media de todos os dias
for (i in 1:length(times)) {
      
      variav_name <- paste(paste("rad_", as.Date(times[i]), sep = ""))
      media_day <- append(media_day, median(get(variav_name)))
      
}

time_series <- zoo(rad_hd_dataf)

x_dias = seq(1, length(times), by= 1)
x_horas = seq(1, 24, by= 1)
#rad_hd_dataf$hora <- x_horas

#gráfico
graph_name_png <- paste("graphs/Rad_hour_TS_", format(as.POSIXct(strptime(times[1], "%Y-%m-%d_%H:%M:%S")), "%Y-%m-%d"), ".png", sep = "")
png(graph_name_png, width = 5950, height = 4500, units = "px", res = 500)

plot_time_ser <- autoplot(time_series, facets = NULL) + #ggplot
      geom_line() +
      labs (title = "Radiação Solar diária na Ilha da Madeira") +
      scale_x_continuous (name = "Hora") +
      scale_y_continuous (name = "Radiação [W/m^2]")
plot(plot_time_ser)

dev.off()


graph_name_png <- paste("graphs/Rad_month_", format(as.POSIXct(strptime(times[1], "%Y-%m-%d_%H:%M:%S")), "%Y-%m-%d"), ".png", sep = "")
png(graph_name_png, width = 5950, height = 4500, units = "px", res = 500)

plot_day_med <- ggplot(data.frame(x_horas, rad_hd_dataf$media)) +
      geom_line(aes(x = x_horas ,y = rad_hd_dataf$media), color = "blue") +
      labs (title = "Radiação Solar diária na Ilha da Madeira") +
      scale_x_continuous (name = "Hora") +
      scale_y_continuous (name = "Radiação [W/m^2]")
plot(plot_day_med)

dev.off()


graph_name_png <- paste("graphs/Rad_daily_", format(as.POSIXct(strptime(times[1], "%Y-%m-%d_%H:%M:%S")), "%Y-%m-%d"), ".png", sep = "")
png(graph_name_png, width = 5950, height = 4500, units = "px", res = 500)

plot_time_med <- ggplot(data.frame(x_dias,media_day)) +
      geom_line(aes(x = x_dias ,y = media_day), color = "blue") +
      geom_point(aes(x = x_dias ,y = media_day)) +
      labs (title = "Radiação Solar diária na Ilha da Madeira") +
      scale_x_continuous (name = "Nº do Dia") +
      scale_y_continuous (name = "Radiação [W/m^2]")
plot(plot_time_med)

dev.off()


#equacao rotacao de matriz
matrix_rotate <- function(x)
      t(apply(x, 2, rev))

max_axis <- max(max_graph) + 50 #if not working do unlist

#ciclo gerar mapas e kmz
for (i in 1:length(times)) {
      
      ##filled contour grafs
      variav_name <- paste(paste("rad_", as.Date(times[i]), sep = ""))
      
      name_png = paste("Images/", "Rad_", as.Date(times[i]), ".png", sep = "")
      png(name_png, width = 5950, height = 4500, units = "px", res = 500)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
      
      filled.contour(long, lat, get(variav_name), asp = 1, color = rgb.palette.rad, levels = seq(0, max_axis, 20), # nlevels = 400, #axes = F #(12), nlev=13,
                     plot.title = title(main = as.expression(paste("Radiação Solar diária na Ilha da Madeira a", as.Date(times[i]))), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                     plot.axes = {axis(1); axis(2); contour(long, lat, hgt, add=TRUE, lwd=0.5, labcex=0.7, levels=c(10, seq(10, 500, 50), seq(500, 1900, 100)), drawlabels=  T, col = "grey30"); grid()},
                     key.title = title(main =  as.expression(paste("[W/m^2]"))))
      
      #plot(getMap(resolution = "high"), add = T)
      #contour(long, lat, hgt, add=TRUE, lwd=1, labcex=1, levels=0.99, drawlabels=FALSE, col="grey30")
      
      dev.off()

      
      ##kmz
      test <-  raster(matrix_rotate(matrix_rotate(matrix_rotate(get(variav_name)))), 
                      xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))   # + plot.axes={ works??????
      #proj4string(test) <- CRS("+proj=longlat +datum=WGS84") #proj
      
      setwd("kmz")
      system(paste("mkdir", paste("Rad_", as.Date(times[i]), sep = "")))
      setwd(paste("Rad_", as.Date(times[i]), sep = ""))
      
      #KML(test, file = paste("Rad_", as.Date(times[i]), ".kmz", sep = ""), colour = rgb.palette.rad)
      plotKML(obj=test, folder.name="RAD", file.name=paste("Rad_", as.Date(times[i]), ".kmz", sep = ""), colour_scale = rgb.palette.rad(400), open.kml = FALSE)
      
      setwd("../../")
      
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
