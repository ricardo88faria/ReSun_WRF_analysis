#!/usr/bin/env Rscript

#packages:

library(lubridate)
library(ncdf4)
#library(rworldmap)
#library(rworldxtra)
library(raster)
library(plotKML)
#library(RNetCDF)


#limpeza ambiente e objetos:
#rm(list=ls())
#cat("\014")

#####################################
cat("Programado por Ricardo Faria \n
    ")
#####################################

t <- Sys.time()

#create folders
system("mkdir kmz Images GIFs graphs")

#cores dos graficos
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")

#open .nc
fileNames <- Sys.glob("wrfout_ReSun_d03*.nc")
nc <- nc_open(fileNames)
names(nc$var)               #variav names

#sist. de coordenadas, projecao e coordenadas (N-S, E-O)
proj <- CRS('+proj=longlat +datum=WGS84')

lat_min <- min(ncvar_get(nc, names(nc$var)[2]))
lat_max <- max(ncvar_get(nc, names(nc$var)[2]))
lat <- unique(as.vector(ncvar_get(nc, names(nc$var)[2])[,,1]))

long_min <- min(ncvar_get(nc, names(nc$var)[3]))
long_max <- max(ncvar_get(nc, names(nc$var)[3]))
long <- unique(as.vector(ncvar_get(nc, names(nc$var)[3])[,,1]))

hgt <- ncvar_get(nc, names(nc$var)[9])[,,1]

times <- list()

#ciclo abrir ficheiros
for (i in 1:length(fileNames)){ 
      temp_nc <- nc$filename[i]
      temp_nc <- nc_open(temp_nc)
      variav_rad <- ncvar_get(temp_nc, names(nc$var)[12])
      
      #list of output dates
      times <- append(times, ncvar_get(temp_nc, names(nc$var)[1])[1])
      
      rad <- 0
      count <- 0
      start <- 0
      #ciclo para is buscar valores de todos os 10 mnts
      for (j in 1:144) {
            #ciclo que so contabiliza espaco temporal com valores de radiacao > 0
            if (variav_rad[,,j] > 1) {
                  
                  rad <- rad + ncvar_get(temp_nc, names(nc$var)[12])[,,j]   #24/144*60 = 10mnts
                  count <- count + 1
                  
            } else {
                  print("nao ha sol nestes 10 minutos")
                  start <- start + 1
            }
            print(paste(count))
            
      }
      #rad = rad                 #W/m^2/day
      #rad_h = rad /count          #W/m^2/h
      print(paste("numero de horas de sol =", dhours(count/6), ", primeiro raio de sol às", dhours(start/(6*2)), "hora solar"))
      
      assign(paste("rad_", as.Date(times[[i]]), sep = ""), rad)    
      #assign(paste("rad_h", i, sep = ""), rad_h) 
      nc_close(temp_nc)
      
}

nc_close(nc)

media_day <- list()
#ciclo para fazer lista da rad media de todos os dias
for (i in 1:length(times)) {
      
      variav_name <- paste(paste("rad_", as.Date(times[[i]]), sep = ""))
      media_day <- append(media_day, median(get(variav_name)))
      
}

#gráfico
graph_name <- paste("graphs/Rad_daily_", format(as.POSIXct(strptime(times[[1]], "%Y-%m-%d_%H:%M:%S")), "%Y-%m-%d"), ".png", sep = "")
png(graph_name, width = 5950, height = 4500, units = "px", res = 500)

x = seq(1, length(times), by= 1)
plot(x = x, y = media_day, xlab = paste("Dia Juliano"), ylab = paste("Radiação [W/m^2]"), main = paste("Radiação Solar diária na Ilha da Madeira"), type = "o", col = "blue", lwd = 2)

dev.off()

#equacao rotacao de matriz
matrix_rotate <- function(x)
      t(apply(x, 2, rev))


#ciclo gerar graficos e kmz
for (i in 1:length(times)) {
      
      ##filled contour grafs
      variav_name <- paste(paste("rad_", as.Date(times[[i]]), sep = ""))
      
      name_png = paste("Images/", "Rad_", as.Date(times[[i]]), ".png", sep = "")
      png(name_png, width = 5950, height = 4500, units = "px", res = 500)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
      
      filled.contour(long, lat, get(variav_name), asp = 1, color = rgb.palette.rad, levels = seq(0, 30000, 1000), # nlevels = 400, #axes = F #(12), nlev=13,
                     plot.title = title(main = as.expression(paste("Radiação Solar diária na Ilha da Madeira a", as.Date(times[[i]]))), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                     plot.axes = {axis(1); axis(2); contour(long, lat, hgt, add=TRUE, lwd=0.5, labcex=0.7, levels=c(2, seq(0, 500, 50), seq(500, 1900, 100)), drawlabels=  T, col = "grey30"); grid()},
                     key.title = title(main =  as.expression(paste("[W/m^2]"))))
      
      #plot(getMap(resolution = "high"), add = T)
      #contour(long, lat, hgt, add=TRUE, lwd=1, labcex=1, levels=0.99, drawlabels=FALSE, col="grey30")
      
      dev.off()
      
      ##kmz
      test <-  raster(matrix_rotate(matrix_rotate(matrix_rotate(get(variav_name)))), 
                      xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))   # + plot.axes={ works??????
      #proj4string(test) <- CRS("+proj=longlat +datum=WGS84") #proj
      
      setwd("kmz")
      system(paste("mkdir", paste("Rad_", as.Date(times[[i]]), sep = "")))
      setwd(paste("Rad_", as.Date(times[[i]]), sep = ""))
      
      #KML(test, file = paste("Rad_", as.Date(times[[i]]), ".kmz", sep = ""), colour = rgb.palette.rad)
      plotKML(obj=test, folder.name="RAD", file.name=paste("Rad_", as.Date(times[[i]]), ".kmz", sep = ""), colour_scale = rgb.palette.rad(400), open.kml = FALSE)
      
      setwd("../../")
      
}

#GIFs
gif_name <- paste("GIFs/", "Rad_", as.Date(times[[i]]), ".gif", sep="")

system(paste("convert -verbose -resize 30% -delay 80 -loop 0", paste("Images/", "*", sep=""), gif_name))


t <- Sys.time() - t

cat("Programado por Ricardo Faria \n
    Finalizado em", t, "mnts")

