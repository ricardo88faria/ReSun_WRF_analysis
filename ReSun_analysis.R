#!/usr/bin/env Rscript

#packages:
library(xlsx)
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
#library(parallel)


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

source("config.txt")
source("matrix_rotation.R")


#create folders
system("mkdir kml Images GIFs graphs GoogleMaps widgets")

#cores dos graficos
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")

#open .nc
fileNames <- Sys.glob("input/resun/*.nc")
nc <- nc_open(fileNames)
names(nc$var)               #variav names

#sist. de coordenadas, projecao e coordenadas (N-S, E-O)
proj <- CRS('+proj=longlat +datum=WGS84')
land <- readShapeSpatial("input/map/PRT_adm2.shp", proj4string = proj)


lat_min <- min(ncvar_get(nc)[,,2])
lat_max <- max(ncvar_get(nc)[,,2])
lat <- unique(as.vector(ncvar_get(nc)[,,2]))

long_min <- min(ncvar_get(nc)[,,1])
long_max <- max(ncvar_get(nc)[,,1])
long <- unique(as.vector(ncvar_get(nc)[,,1]))

hgt <- ncvar_get(nc)[,,3]

ncols <- length(long)
nrows <- length(lat)

nc_close(nc)

seq_time <- seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "month")
seq_months <- format(seq_time, format="%b")

variavs <- c("IGPH",
             "IDIR",
             "IDIF",
             "DIFGPH",
             "Kt")

#abrir ficheiro correcao
if (corr == 1) {
      
      file_csv <- Sys.glob("input/*.csv")
      corr_values <- read.csv(file_csv, row.names = 1)
      
}

#aplicar correcao variaveis fixas, z, lat, long
fix_var <- intersect(corr_vars, c("z", "lat", "lon"))
fix_var_tmp <- c("hgt", "lat", "long")
corr_fix <- c()

if (corr == 1) {
      for (i in 1:length(fix_var)) {
            
            #corr_IGPH[[i]] <- ncvar_get(nc)[,,8]
            
            m <- corr_values[c("m_D03"),c(corr_vars[i])]
            x <- corr_values[c("x_D03"),c(corr_vars[i])]
            
            corr_fix[[fix_var[i]]] <- x + m*get(fix_var_tmp[i])
            
            if (corr_vars[i] == "lat") {
                  
                  corr_fix[[fix_var[i]]] <- matrix(data = corr_fix$lat, nrow = nrow(hgt), ncol = ncol(hgt), byrow = F)
                  
            } else if (corr_vars[i] == "lon") {
                  
                  corr_fix[[fix_var[i]]] <- matrix(data = corr_fix$lon, nrow = nrow(hgt), ncol = ncol(hgt), byrow = T)
            }
      }
}

corr_fix_fix <- Reduce("+", corr_fix)

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
fix_var <- intersect(corr_vars, c("Kt", "cleS", "iS", "cloS"))

# ciclo abrir ficheiros e aplicar correcao variavel kt - varia em funcao do tempo
for (i in 1:length(fileNames)){ 
      temp_nc <- nc$filename[i]
      temp_nc <- nc_open(temp_nc)
      
      # retirar valores das variaveis
      if (corr == 1) {
            
            IGPH[[i]] <- ncvar_get(nc)[,,8] 
            Kt[[i]] <-ncvar_get(nc)[,,4]
            
            for (j in 1:length(fix_var)) {
                  
                  
                  m <- corr_values[c("m_D03"),c(fix_var[j])]
                  x <- corr_values[c("x_D03"),c(fix_var[j])]
                  
                  corr_fix[[paste0(fix_var[j],i)]] <- x + m*Kt[[i]]
                  
                  IGPH[[i]] <- IGPH[[i]] + corr_fix[[paste0(fix_var[j],i)]] + corr_fix_fix
                  
                  
            }
            
            IDIR[[i]] <- ncvar_get(nc)[,,7]
            IDIF[[i]] <- ncvar_get(nc)[,,6]  
            DIFGPH[[i]] <- ncvar_get(nc)[,,5]  
            
      } else {
            
            IGPH[[i]] <- ncvar_get(nc)[,,8] 
            IDIR[[i]] <- ncvar_get(nc)[,,7]
            IDIF[[i]] <- ncvar_get(nc)[,,6]  
            DIFGPH[[i]] <- ncvar_get(nc)[,,5]  
            Kt[[i]] <- ncvar_get(nc)[,,4]  
            
      }
      nc_close(temp_nc)
      
      #retirar valor maximo de radiacao para grafico
      max_IGPH[[i]] <- max(IGPH[[i]])
      max_IDIR[[i]] <- max(IDIR[[i]])
      max_IDIF[[i]] <- max(IDIF[[i]])
      max_DIFGPH[[i]] <- max(DIFGPH[[i]], na.rm = T)
      max_Kt[[i]] <- max(Kt[[i]], na.rm = T)
      
}

# criar raster files cortados pelo contorno da topografia
raster_IGPH <- c()
raster_IDIR <- c()
raster_IDIF <- c()
raster_DIFGPH <- c()
raster_Kt <- c()

#beginCluster( detectCores() -1)
rasterOptions(timer = T, progress = "text")
system.time(      
      for (i in 1:length(fileNames)){      
            
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
            
            
      })
#endCluster()
rm(temp)

IGPH <- array(unlist(IGPH), dim=c(nrows, ncols, length(fileNames)))
IDIR <- array(unlist(IDIR), dim=c(nrows, ncols, length(fileNames)))
IDIF <- array(unlist(IDIF), dim=c(nrows, ncols, length(fileNames)))
DIFGPH <- array(unlist(DIFGPH), dim=c(nrows, ncols, length(fileNames)))
Kt <- array(unlist(Kt), dim=c(nrows, ncols, length(fileNames)))

#IGPH <- matrix(IGPH)
#IDIR <- matrix(IDIR)
#IDIF <- matrix(IDIF)
#DIFGPH <- matrix(DIFGPH)
#Kt <- matrix(Kt)


pnts = data.frame(Name = 'Madeira',lat = lat[140] ,lon = long[300])
#pnts = rbind(pnts,data.frame(Name = 'Tel Aviv ',lat = lat_min, lon = long_max))
coordinates(pnts) <- ~lon + lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")

t.start = format(as.Date('2010_01_01', "%Y_%m_%d "), "%Y-%m-%d %H:%M:%S")
Times = seq(as.POSIXlt("2015-01-01"), as.POSIXlt("2015-02-28"), "months")


# passar os rasters para RasterBrickTimeSeries class
raster_IGPH <- brick(raster_IGPH)
names(raster_IGPH) <- paste0("IGPH", seq_months[1:length(fileNames)])
raster_IGPH <- new("RasterBrickTimeSeries", variable = "IGPH", 
                   sampled = pnts, rasters = raster_IGPH, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_IDIR <- brick(raster_IDIR)
names(raster_IDIR) <- paste0("IDIR", seq_months[1:length(fileNames)])
raster_IDIR <- new("RasterBrickTimeSeries", variable = "IDIR", 
                   sampled = pnts, rasters = raster_IDIR, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_IDIF <- brick(raster_IDIF)
names(raster_IDIF) <- paste0("IDIF", seq_months[1:length(fileNames)])
raster_IDIF <- new("RasterBrickTimeSeries", variable = "IDIF", 
                   sampled = pnts, rasters = raster_IDIF, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_DIFGPH <- brick(raster_DIFGPH)
names(raster_DIFGPH) <- paste0("DIFGPH", seq_months[1:length(fileNames)])
raster_DIFGPH <- new("RasterBrickTimeSeries", variable = "DIFGPH", 
                     sampled = pnts, rasters = raster_DIFGPH, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_Kt <- brick(raster_Kt)
names(raster_Kt) <- paste0("Kt", seq_months[1:length(fileNames)])
raster_Kt <- new("RasterBrickTimeSeries", variable = "Kt", 
                 sampled = pnts, rasters = raster_Kt, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

#raster_IGPH <- rts(stack(raster_IGPH), seq_time[1:length(fileNames)])
#raster_IDIR <- rts(stack(raster_IDIR), seq_time[1:length(fileNames)])
#raster_IDIF <- rts(stack(raster_IDIF), seq_time[1:length(fileNames)])
#raster_DIFGPH <- rts(stack(raster_DIFGPH), seq_time[1:length(fileNames)])
#raster_Kt <- rts(stack(raster_Kt), seq_time[1:length(fileNames)])

#ciclo gerar mapas e kmz
for (j in 1:length(variavs)) {
      
      for (i in 1:length(fileNames)) {
            ##filled contour grafs
            
            if (corr == 1) {
                  variav_name <- paste0(variavs[j], "_corr")
            } else if (corr == 1) {
                  variav_name <- paste0(variavs[j])
            }
            max_axis <- ceiling(as.numeric(max(get(paste0("max_", variavs[j])))))
            
            name_png = paste0("Images/", variavs[j], "_", seq_months[i], ".png")
            png(name_png, width = ncols*14, height = nrows*14, units = "px", res = 500)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
            
            filled.contour(long, lat, t(get(variav_name)[,,i]), asp = 1, color = rgb.palette.rad, levels = seq(0, max_axis, max_axis/10), # nlevels = 400, #axes = F #(12), nlev=13,
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
      
      variav_name <- paste0("raster_", variavs[j])
      
      setwd("kml")
      #system(paste("mkdir", seq_months[i]))
      #setwd(paste0(seq_months[i]))
      
      if (corr == 1) {
            system(paste("mkdir", paste0(variavs[j], "_corr")))
            setwd(paste0(variavs[j], "_corr"))
      } else if (corr == 1) {
            system(paste("mkdir", variavs[j]))
            setwd(paste0(variavs[j]))
      }
      #KML(test, file = paste("Rad_", as.Date(times[i]), ".kmz", sep = ""), colour = rgb.palette.rad)
      plotKML(obj = get(variav_name), folder.name = variavs[j], file.name = paste0(variavs[j], ".kml"),
              iframe.width = ncols*14, iframe.height = nrows*14, colour_scale = rgb.palette.rad(400), open.kml = F)
      
      setwd("../../")
      
}


t <- (Sys.time() - t)

cat("Programado por Ricardo Faria \n
    Finalizado em :")

print(t)
