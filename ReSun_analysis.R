#!/usr/bin/env Rscript

#packages:
#library(xlsx)
#library(plyr)
library(reshape2)
library(lubridate)
library(timeDate)
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
library(RColorBrewer)


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
system("mkdir -p output/kml output/images output/nc output/graphs")

#cores dos graficos
rgb.palette.rad <- colorRampPalette(c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), space = "rgb")
rgb.palette.kt <- colorRampPalette(c(brewer.pal(9, "Blues")), space = "rgb")
rgb.palette.DIFGPH <- colorRampPalette(c(brewer.pal(9, "YlOrRd")), space = "rgb")

#open .nc
fileNames <- Sys.glob("input/resun/Results_*")
#fileNames_tot <- Sys.glob("input/resun/Results_All*")
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

#extrair valores das coordenadas das estacoes
coord_file <- list.files(path = "input/coord", pattern="*.csv", full.names=TRUE)
locs <- read.csv(coord_file)
locs <- locs[,c("loc", "lat", "lon")]

lat_index <- c()
long_index <- c()
for (l in 1:nrow(locs)) { 
      
      lat_index_temp <- 0
      lat <- as.vector(lat)
      lat_index_temp <- which.max(lat[lat <= locs[l, 2]])
      lat_index <- append(lat_index, lat_index_temp)
      
      long_index_temp <- 0
      long <- as.vector(long)
      long_index_temp <- which.max(long[long <= locs[l, 3]])
      long_index <- c(long_index, long_index_temp)
      
}

hgt <- ncvar_get(nc)[,,3]

ncols <- length(long)
nrows <- length(lat)

nc_close(nc)

seq_time <- seq(as.Date("2000/1/1"), as.Date("2000/12/31"), "month")
seq_months <- format(seq_time, format="%b")

if (corr == 1) {
      variavs <- c("IGPH",
                   "IGPH_corr",
                   "IDIR",
                   "IDIF",
                   "DIFGPH",
                   "Kt")
} else if (corr == 0) {
      variavs <- c("IGPH",
                   "IDIR",
                   "IDIF",
                   "DIFGPH",
                   "Kt")
}

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
            
            # dominio D02
            m <- corr_values[c("m_D02"), c(corr_vars[i])]
            x <- corr_values[c("x_D02"), c(corr_vars[i])]
            
            corr_fix[[paste0(fix_var[i], "_D02")]] <- x + m*get(fix_var_tmp[i])
            
            # dominio D03
            m <- corr_values[c("m_D03"), c(corr_vars[i])]
            x <- corr_values[c("x_D03"), c(corr_vars[i])]
            
            corr_fix[[paste0(fix_var[i], "_D03")]] <- x + m*get(fix_var_tmp[i])
            
            if (corr_vars[i] == "lat") {
                  
                  corr_fix[[paste0(fix_var[i], "_D02")]] <- matrix(data = corr_fix$lat, nrow = nrow(hgt), ncol = ncol(hgt), byrow = F)
                  corr_fix[[paste0(fix_var[i], "_D03")]] <- matrix(data = corr_fix$lat, nrow = nrow(hgt), ncol = ncol(hgt), byrow = F)
                  
            } else if (corr_vars[i] == "lon") {
                  
                  corr_fix[[paste0(fix_var[i], "_D02")]] <- matrix(data = corr_fix$lon, nrow = nrow(hgt), ncol = ncol(hgt), byrow = T)
                  corr_fix[[paste0(fix_var[i], "_D03")]] <- matrix(data = corr_fix$lon, nrow = nrow(hgt), ncol = ncol(hgt), byrow = T)
                  
            }
      }
}

corr_fix_fix <- Reduce("+", corr_fix)

message("reading ReSun netcdf files, if coor=1 applying  correction")

max_IGPH <- character(0)
max_IGPH_corr <- character(0)
max_IDIR <- character(0)
max_IDIF <- character(0)
max_DIFGPH <- character(0)
max_Kt <- character(0)
IGPH <- c()
IGPH_corr <- c()
IDIR <- c()
IDIF <- c()
DIFGPH <- c()
Kt <- c()
fix_var <- intersect(corr_vars, c("Kt", "cleS", "iS", "cloS"))
d <- c()
d_tot <- c()
months_name <- c("tot", seq_months)
# ciclo abrir ficheiros e aplicar correcao variavel kt - varia em funcao do tempo
# retirar valores das variaveis
for (i in 1:length(fileNames)){ 
      temp_nc <- nc$filename[i]
      temp_nc <- nc_open(temp_nc)
      
      # retirar valores das variaveis
      IGPH[[i]] <- ncvar_get(temp_nc)[,,8] 
      # crop ao contour hgt/land
      #IGPH[[1]][hgt <= 0.1] <- NA
      
      Kt[[i]] <-ncvar_get(temp_nc)[,,4]
      IDIR[[i]] <- ncvar_get(temp_nc)[,,7]
      IDIF[[i]] <- ncvar_get(temp_nc)[,,6]  
      DIFGPH[[i]] <- ncvar_get(temp_nc)[,,5]  
      
      if (i == 1 & corr == 1 & length(fix_var) > 0) {
            
            for (j in 1:length(fix_var)) {
                  
                  # i >= 1 & i <= 13 equivalente ao dominio D02
                  if (i == 1) {
                        
                        m <- corr_values[c("m_D02"),c(fix_var[j])]
                        x <- corr_values[c("x_D02"),c(fix_var[j])]
                        
                        corr_fix[[paste0(fix_var[j], "_D02")]] <- x + m*Kt[[i]]
                        
                        IGPH_corr[[1]] <- IGPH[[1]] * (1 - (corr_fix[[paste0(fix_var[j], "_D02")]] + corr_fix_fix)/100)
                        #IGPH_corr[[i]] <- IGPH[[i]] + corr_fix[[paste0(fix_var[j],i)]] + corr_fix_fix
                        
                  } 
                  # i > 13 & i <= 26 equivalente ao dominio D03
                  else if (i == 14 ) {
                        
                        m <- corr_values[c("m_D03"),c(fix_var[j])]
                        x <- corr_values[c("x_D03"),c(fix_var[j])]
                        
                        corr_fix[[paste0(fix_var[j], "_D03")]] <- x + m*Kt[[i]]
                        
                        IGPH_corr[[2]] <- IGPH[[2]] * (1 - (corr_fix[[paste0(fix_var[j], "_D03")]] + corr_fix_fix)/100)
                        #IGPH_corr[[i]] <- IGPH[[i]] + corr_fix[[paste0(fix_var[j],i)]] + corr_fix_fix
                        
                  }
                  
            }
            
      } else if (i == 1 & corr == 1 & length(fix_var) == 0) {
            
            IGPH_corr[[i]] <- IGPH[[i]] * (1 - (corr_fix_fix)/100)
            
      } else if (i == 14 & corr == 1 & length(fix_var) == 0) {
            
            IGPH_corr[[2]] <- IGPH[[i]] * (1 - (corr_fix_fix)/100)
            
      }
      
      nc_close(temp_nc)
      
      #retirar valor maximo de radiacao para grafico
      max_IGPH[[i]] <- max(IGPH[[i]])
      
      if (i == 1 & corr == 1) {
            max_IGPH_corr[[i]] <- max(IGPH_corr[[i]])
      }
      
      max_IDIR[[i]] <- max(IDIR[[i]])
      max_IDIF[[i]] <- max(IDIF[[i]])
      max_DIFGPH[[i]] <- max(DIFGPH[[i]], na.rm = T)
      max_Kt[[i]] <- max(Kt[[i]], na.rm = T)
      
}

#retirar valores das cooredenadas das estacoes de entrada
for (i in 1:(length(fileNames)/2)){ 
      for (l in 1:length(lat_index)) {
            
            if (i == 1 & corr == 1) { 
                  
                  #d_tot <- rbind(d_tot ,c(Location = levels(locs$loc)[l], value = IGPH_corr[[i]][lat_index[l], long_index[l]], Month = "TMY_corr"))
                  
                  d_tot <- rbind(d_tot ,c(Location = levels(locs$loc)[l], value = (IGPH_corr[[1]][lat_index[l], long_index[l]] + IGPH_corr[[2]][lat_index[l], long_index[l]])/2, Month = "TMY_corr"))
                  
            }
            
      }
      for (l in 1:length(lat_index)) {
            
            if (i == 1) {    
                  
                  #d <- rbind(d ,c(Location = levels(locs$loc)[l], value = IGPH_corr[[i]][lat_index[l], long_index[l]], Month = "tot_corr"))
                  #d_tot <- rbind(d_tot ,c(Location = levels(locs$loc)[l], value = IGPH[[1]][lat_index[l], long_index[l]], Month = months_name[1]))
                  
                  d_tot <- rbind(d_tot ,c(Location = levels(locs$loc)[l], value = (IGPH[[1]][lat_index[l],long_index[l]] + IGPH[[1 + 13]][lat_index[l], long_index[l]])/2, Month = months_name[1]))
                  
            } else if (i >= 2 & i <= length(fileNames)){ #if (i >= 2 & i <= length(fileNames))
                  
                  #d <- rbind(d ,c(Location = levels(locs$loc)[l], value = IGPH[[i]][lat_index[l], long_index[l]], Month = months_name[i]))
                  
                  d <- rbind(d ,c(Location = levels(locs$loc)[l], value = (IGPH[[i]][lat_index[l], long_index[l]] + IGPH[[i + length(fileNames)/2]][lat_index[l], long_index[l]])/2, Month = months_name[i]))
                  
            }
            
      }
}

#separar dominio D02 e D03 e faxer media dos 2 contando com os fatores de correcao
for (i in 1:(length(fileNames)/2)) {
      
      if (i == 1 & corr == 1) {
            
            IGPH_corr[[1]] <- (IGPH_corr[[1]] + IGPH_corr[[2]])/2
            
      }
      
      IGPH[[i]] <- (IGPH[[i]] + IGPH[[i + length(fileNames)/2]])/2
      Kt[[i]] <- (Kt[[i]] + Kt[[i + length(fileNames)/2]])/2
      IDIR[[i]] <- (IDIR[[i]] + IDIR[[i + length(fileNames)/2]])/2
      IDIF[[i]] <- (IDIF[[i]] + IDIF[[i + length(fileNames)/2]])/2
      DIFGPH[[i]] <- (DIFGPH[[i]] + DIFGPH[[i + length(fileNames)/2]])/2
      
}
# remover resto das matrizes nao necessarias para libertar espaco
for (i in 1:length(fileNames)/2) {
      
      if (i == 1 & corr == 1) {
            
            IGPH_corr[[2]] <- NULL
            
      }
      
      IGPH[[i + length(fileNames)/2]] <- NULL
      Kt[[i + length(fileNames)/2]] <- NULL
      IDIR[[i + length(fileNames)/2]] <- NULL
      IDIF[[i + length(fileNames)/2]] <- NULL
      DIFGPH[[i + length(fileNames)/2]] <- NULL
      
}

# save d table for revisado results comparation
write.csv(rbind(d, d_tot), file = paste0("output/IGPH_", local, "_stat_data.csv"), row.names = T)
#write.xlsx(d, file = "output/IGPH_coords.csv")

d <- data.frame(d)
d_tot <- data.frame(d_tot)
d$value <- as.numeric(as.character(d$value))
d_tot$value <- as.numeric(as.character(d_tot$value))
d$Month <- factor(d$Month, levels = d$Month)
d_tot$Month <- factor(d_tot$Month, levels = d_tot$Month)

#d_d <- d[order(d[,1]),] 
#d_d <- melt(d_d, id = c("value", "Month"))

# qplot(x = d$Month, y = d$value)
# qplot(x = d$Month, y = d$value, color = d$Location)

g_poli <- ggplot(data = d, aes(x = Month ,y = value, color = Location, group = Location)) + 
      geom_point(size = 2) + 
      geom_line(alpha = .3, size=1) + 
      scale_color_brewer(palette="Paired") + 
      theme_bw() +
      ggtitle("Radiação mensal nas EMAS [W/m^2]")
ggsave(plot = g_poli, paste0("output/graphs/stations_", local, "_tot_year_pont-lin.png"), width = 7*7, height = 5*7, units = "cm", scale = .6, dpi = 400)

g_hist <- ggplot(data = d, aes(x = Month ,y = value, fill = Location)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_brewer(palette="Paired") + 
      theme_bw() +
      ggtitle("Radiação mensal nas EMAS [W/m^2]")
ggsave(plot = g_hist, paste0("output/graphs/stations_", local, "_tot_year_hist.png"), width = 7*7, height = 5*7, units = "cm", scale = .6, dpi = 400)

g_hist_tot <- ggplot(data = d_tot, aes(x = Month ,y = value, fill = Location)) + 
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette="Paired") + 
      theme_bw() +
      ggtitle("Radiação total e corrigida anual nas EMAS [W/m^2]")
ggsave(plot = g_hist_tot, paste0("output/graphs/stations_", local, "_tot_year_hist_tot.png"), width = 7*7, height = 5*7, units = "cm", scale = .6, dpi = 400)

g_polar <- ggplot(data = d, aes(x = Month ,y = value, fill = Location)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      coord_polar() +
      scale_fill_brewer(palette="Paired") + 
      theme_bw() +
      ggtitle("Radiação mensal nas EMAS [W/m^2]")
ggsave(plot = g_polar, paste0("output/graphs/stations_", local, "_tot_year_polar-hist.png"), width = 6*7, height = 5*7, units = "cm", scale = .6, dpi = 400)

# criar raster files cortados pelo contorno da topografia
message("cropping & creating raster files")
raster_IGPH <- c()
raster_IGPH_corr <- c()
raster_IDIR <- c()
raster_IDIF <- c()
raster_DIFGPH <- c()
raster_Kt <- c()

#beginCluster( detectCores() -1)
rasterOptions(timer = T, progress = "text")

if (corr == 1) {
      temp <- raster(mat_rot(mat_rot(mat_rot(t(IGPH_corr[[1]])))), 
                     xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
      if (crop == 1) {
            temp <- crop(temp, extent(land))
            temp <- mask(temp, land)
      }
      raster_IGPH_corr <- c(raster_IGPH_corr, temp)
}

system.time(
      for (i in 1:length(fileNames)){      
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IGPH[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IGPH <- c(raster_IGPH, temp)
            print(paste("mes", i, "- 20%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIR[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IDIR <- c(raster_IDIR, temp)
            print(paste("mes", i, "- 40%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIF[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IDIF <- c(raster_IDIF, temp)
            print(paste("mes", i, "- 60%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(DIFGPH[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_DIFGPH <- c(raster_DIFGPH, temp)
            print(paste("mes", i, "- 80%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(Kt[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_Kt <- c(raster_Kt, temp)
            print(paste("mes", i, "- 100%"))
            
      }
)
#endCluster()
rm(temp)

#test rasterbrick
#brickraster_kt <- brick(raster_Kt)


IGPH <- array(unlist(IGPH), dim=c(nrows, ncols, length(fileNames)))
if (corr == 1) {
      IGPH_corr <- array(unlist(IGPH_corr), dim=c(nrows, ncols, length(fileNames)))
}
IDIR <- array(unlist(IDIR), dim=c(nrows, ncols, length(fileNames)))
IDIF <- array(unlist(IDIF), dim=c(nrows, ncols, length(fileNames)))
DIFGPH <- array(unlist(DIFGPH), dim=c(nrows, ncols, length(fileNames)))
Kt <- array(unlist(Kt), dim=c(nrows, ncols, length(fileNames)))

#IGPH <- matrix(IGPH)
#IDIR <- matrix(IDIR)
#IDIF <- matrix(IDIF)
#DIFGPH <- matrix(DIFGPH)
#Kt <- matrix(Kt)

pnts = data.frame(Name = 'Madeira', lat = lat[round(length(lat)/2)] ,lon = long[round(length(long)/2)])
#pnts = rbind(pnts,data.frame(Name = 'Tel Aviv ',lat = lat_min, lon = long_max))
coordinates(pnts) <- ~lon + lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")

#sequencia de tempo inicial e final
#t.start = format(as.Date('2010_01_01', "%Y_%m_%d "), "%Y-%m-%d %H:%M:%S")
Times = seq(as.POSIXlt("2015-01-01"), as.POSIXlt("2015-02-28"), "months")
#Times = seq(as.POSIXlt("2015-01-30"), as.POSIXlt("2015-12-30"), by = "month")
#seq(as.Date("2010-02-01"), length=12, by="1 month") -1

tS_i = as.POSIXct(timeSequence(from = "2015-01-01", to = "2015-12-31", by = "month"))
tS_f = as.POSIXct(timeLastDayInMonth(tS_i))

# passar os rasters para RasterBrickTimeSeries class
raster_IGPH <- brick(raster_IGPH)
names(raster_IGPH) <- paste0("IGPH_", months_name[1:length(fileNames)])
#raster_IGPH_test <- new("RasterBrickTimeSeries", variable = "IGPH", 
#                   sampled = pnts, rasters = raster_IGPH, TimeSpan.begin = tS_i, TimeSpan.end = tS_f)

if (corr == 1) {
      raster_IGPH_corr <- brick(raster_IGPH_corr)
      names(raster_IGPH_corr) <- paste0("IGPH_", months_name[1])
      #      raster_IGPH_corr <- new("RasterBrickTimeSeries", variable = "IGPH_corr", 
      #                              sampled = pnts, rasters = raster_IGPH_corr, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])
}

raster_IDIR <- brick(raster_IDIR)
names(raster_IDIR) <- paste0("IDIR_", months_name[1:length(fileNames)])
#raster_IDIR <- new("RasterBrickTimeSeries", variable = "IDIR", 
#                   sampled = pnts, rasters = raster_IDIR, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_IDIF <- brick(raster_IDIF)
names(raster_IDIF) <- paste0("IDIF_", months_name[1:length(fileNames)])
#raster_IDIF <- new("RasterBrickTimeSeries", variable = "IDIF", 
#                   sampled = pnts, rasters = raster_IDIF, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_DIFGPH <- brick(raster_DIFGPH)
names(raster_DIFGPH) <- paste0("DIFGPH_", months_name[1:length(fileNames)])
#raster_DIFGPH <- new("RasterBrickTimeSeries", variable = "DIFGPH", 
#                     sampled = pnts, rasters = raster_DIFGPH, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

raster_Kt <- brick(raster_Kt)
names(raster_Kt) <- paste0("Kt_", months_name[1:length(fileNames)])
#raster_Kt <- new("RasterBrickTimeSeries", variable = "Kt", 
#                 sampled = pnts, rasters = raster_Kt, TimeSpan.begin = Times[1:length(fileNames)], TimeSpan.end = Times[1:length(fileNames)])

#raster_IGPH <- rts(stack(raster_IGPH), seq_time[1:length(fileNames)])
#raster_IDIR <- rts(stack(raster_IDIR), seq_time[1:length(fileNames)])
#raster_IDIF <- rts(stack(raster_IDIF), seq_time[1:length(fileNames)])
#raster_DIFGPH <- rts(stack(raster_DIFGPH), seq_time[1:length(fileNames)])
#raster_Kt <- rts(stack(raster_Kt), seq_time[1:length(fileNames)])

save(locs, d, d_tot, raster_IGPH, rgb.palette.rad, file = "output/data.RData")

#ciclo gerar mapas e kmz
for (j in 1:length(variavs)) {
      
      if (variavs[j] == "Kt") {
            color <- rgb.palette.kt
      } else if (variavs[j] == "DIFGPH") {
            color <- rgb.palette.DIFGPH
      } else {
            color <- rgb.palette.rad
      }
      
      if (png_out == 1) {
            message("creating png files - ", variavs[j])
            
            pb <- txtProgressBar(min = 0, max = length(fileNames), style = 3, title = paste0(variavs[j]))
            if (j != 2) {
                  for (i in 1:length(fileNames)) {
                        ##filled contour grafs
                        
                        variav_name <- paste0(variavs[j])
                        
                        #print(paste0("image variavel:", variav_name, " - mes:", i))
                        
                        max_axis <- ceiling(as.numeric(max(get(paste0("max_", variavs[j])))))
                        
                        name_png = paste0("output/images/", variavs[j], "_", local, "_", months_name[i], ".png")
                        png(name_png, width = ncols*7, height = nrows*7, units = "px", res = 250)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
                        
                        filled.contour(long, lat, t(get(variav_name)[,,i]),  color = color, levels = seq(0, max_axis, max_axis/15), # nlevels = 400, #axes = F #(12), nlev=13,
                                       plot.title = title(main = as.expression(paste("Média de", variavs[j], "na Ilha da Madeira em", months_name[i])), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                                       plot.axes = {axis(1); axis(2); 
                                             contour(long, lat, t(hgt), add = T, col = terrain.colors(21), lwd=0.4, labcex=0.5, levels = c(.1, seq(min(hgt), max(hgt), length.out = 21)));
                                             grid(lty = 5, lwd = 0.5)},
                                       key.title = title(main = as.expression(paste("[W/m^2]"))))
                        
                        #plot(getMap(resolution = "high"), add = T)
                        #contour(long, lat, hgt, add=TRUE, lwd=1, labcex=1, levels=0.99, drawlabels=FALSE, col="grey30")
                        
                        dev.off()
                        
                        setTxtProgressBar(pb, i)
                  }
            } else if (j == 2) {
                  
                  variav_name <- paste0(variavs[j])
                  
                  #print(paste0("image variavel:", variav_name, " - mes:", i))
                  
                  max_axis <- ceiling(as.numeric(max(get(paste0("max_", variavs[j])))))
                  
                  name_png = paste0("output/images/", variavs[j], "_", local, "_", months_name[i], ".png")
                  png(name_png, width = ncols*7, height = nrows*7, units = "px", res = 250)  #width = 7000 (width = 14000, height = 9000, units = "px", res = 1000)
                  
                  filled.contour(long, lat, t(get(variav_name)[,,i]),  color = color, levels = seq(0, max_axis, max_axis/15), # nlevels = 400, #axes = F #(12), nlev=13,
                                 plot.title = title(main = as.expression(paste("Média de", variavs[j], "na Ilha da Madeira em", months_name[i])), xlab = 'Longitude [°]', ylab = 'Latitude [°]'),
                                 plot.axes = {axis(1); axis(2); 
                                       contour(long, lat, t(hgt), add = T, col = terrain.colors(21), lwd=0.4, labcex=0.5, levels = c(.1, seq(min(hgt), max(hgt), length.out = 21)));
                                       grid(lty = 5, lwd = 0.5)},
                                 key.title = title(main = as.expression(paste("[W/m^2]"))))
                  
                  #plot(getMap(resolution = "high"), add = T)
                  #contour(long, lat, hgt, add=TRUE, lwd=1, labcex=1, levels=0.99, drawlabels=FALSE, col="grey30")
                  
                  dev.off()
                  
            }
            close(pb)
            
      }
      
      if (kml_out == 1) {
            message("creating kml files")
            
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
            
            print(paste0("kml variavel:", variav_name))
            
            setwd("output/kml/")
            
            # gravar sequencia dos meses
            if (j != 2) {
                  
                  KML(get0(variav_name), file = paste0(variavs[j], "_", local, ".kml"), time = c(tS_i, tS_i[length(tS_i)]), overwrite = T, #[length(tS_f)]
                      col = color(20), blur = 1) 
                  
            }else if (j == 2) {
                  
                  KML(get0(variav_name), file = paste0(variavs[j], "_", local, ".kml"), overwrite = T, #[length(tS_f)]
                      col = color(20), blur = 1) 
                  
            }
            # gravar total
            #KML(get0(variav_name)[1], file = paste0(variavs[j], "_tot.kml"), overwrite = T, #[length(tS_f)]
            #    col = color(20), blur = 1)
            #kml_layer.Raster(get0(variav_name), subfolder.name = paste(class(get0(variav_name))), TimeSpan.begin = tS_i, TimeSpan.end = tS_f, colour = color(20))
            #kml_layer.RasterBrick(get0(variav_name), plot.legend = TRUE, subfolder.name = paste(class(get0(variav_name))), dtime = tS_i, colour = color(20))
            
            #system(paste("mkdir", variavs[j]))
            #setwd(paste0(variavs[j]))
            
            #KML(test, file = paste("Rad_", as.Date(times[i]), ".kmz", sep = ""), colour = rgb.palette.rad)
            #plotKML(obj = get(variav_name), folder.name = variavs[j], file.name = paste0(variavs[j], ".kml"),
            #        iframe.width = ncols*14, iframe.height = nrows*14, colour_scale = rgb.palette.rad(400), open.kml = F) 
            
            #setwd("../../../")
            setwd("../../")
      }
}
#raster kml
#KML(raster_IGPH, file='meuse_b2-0.kml', time =(seq(as.Date("2010-02-01"), length=2, by="1 month") -1), col = rgb.palette.rad(400), blur = 2)

if (nc_out == 1) {
      message("creating netcdf files")
      
      IGPH_tot = list()
      IGPH_tot_corr = list()
      #transpose variavs matrixes
      for (i in 2:length(fileNames)) {
            
            IGPH[,,i] <- t(IGPH[,,i])
            
            IGPH_tot[[i-1]] <- t(IGPH[,,1])
            if (corr == 1) {
                  IGPH_tot_corr[[i-1]] <- t(IGPH_corr[,,1])
            }
            
            IDIR[,,i] <- t(IDIR[,,i])
            IDIF[,,i] <- t(IDIF[,,i])
            DIFGPH[,,i] <- t(DIFGPH[,,i])
            Kt[,,i] <- t(Kt[,,i])
      }
      IGPH_tot <- array(unlist(IGPH_tot), dim=c(nrows, ncols, length(fileNames)-1))
      IGPH_tot_corr <- array(unlist(IGPH_tot_corr), dim=c(nrows, ncols, length(fileNames)-1))
      
      
      #define dimensions
      nc_londim <- ncdim_def(name = "lon", units = "degrees_east", vals = long) 
      nc_latdim <- ncdim_def(name = "lat", units = "degrees_north", vals = lat) 
      nc_monthdim <- ncdim_def(name = "month", units = "month_year", vals = as.numeric(format(tS_i, "%m")))
      
      #define variables
      nc_hgt <- ncvar_def(name = "hgt", units = "m", dim = list(nc_londim, nc_latdim), prec = "single")
      
      nc_IGPH <- ncvar_def(name = "IGPH", units = "w/m^2", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      nc_IGPH_tot <- ncvar_def(name = "IGPH_tot", units = "w/m^2", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      
      if (corr == 1) {
            
            nc_IGPH_tot_corr <- ncvar_def(name = "IGPH_tot_corr", units = "w/m^2", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
            
      }
      
      nc_IDIR <- ncvar_def(name = "IDIR", units = "w/m^2", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      nc_IDIF <- ncvar_def(name = "IDIF", units = "w/m^2", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      nc_DIFGPH <- ncvar_def(name = "DIFGPH", units = "%", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      nc_Kt <- ncvar_def(name = "Kt", units = "", dim = list(nc_londim, nc_latdim, nc_monthdim), prec = "single")
      
      #create netCDF file and put arrays
      nc_name <- paste("output/nc/ReSun_", local, ".nc", sep = "")
      if (corr == 1) {
            
            ncout <- nc_create(filename = nc_name, vars = list(nc_hgt, nc_IGPH_tot, nc_IGPH_tot_corr, nc_IGPH, nc_IDIR, nc_IDIF, nc_DIFGPH, nc_Kt), force_v4 = T, verbose = F)
            
      } else {
            
            ncout <- nc_create(filename = nc_name, vars = list(nc_hgt, nc_IGPH_tot, nc_IGPH, nc_IDIR, nc_IDIF, nc_DIFGPH, nc_Kt), force_v4 = T, verbose = F)
            
      }
      
      #put variables
      ncvar_put(ncout, nc_hgt, t(hgt))
      
      ncvar_put(ncout, nc_IGPH, IGPH[,,2:13])
      ncvar_put(ncout, nc_IGPH_tot, IGPH_tot)
      
      if (corr == 1) {
            
            ncvar_put(ncout, nc_IGPH_tot_corr, IGPH_tot_corr)
            
      }
      ncvar_put(ncout, nc_IDIR, IDIR[,,2:13])
      ncvar_put(ncout, nc_IDIF, IDIF[,,2:13])
      ncvar_put(ncout, nc_DIFGPH, DIFGPH[,,2:13])
      ncvar_put(ncout, nc_Kt, Kt[,,2:13])
      
      #put additional attributes into dimension and data variables
      ncatt_put(ncout, "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
      ncatt_put(ncout, "lat", "axis", "Y")
      ncatt_put(ncout, "month", "axis", "T")
      #ncatt_put(ncout, "hour", "axis", "T")
      
      #add global attributes
      #ncatt_put(ncout,0,"title","Shadow median per Julian day")
      name <- paste("Created by: Ricardo Faria", Sys.time(), "ReSun results", sep=", ")
      ncatt_put(ncout, 0, "title", name)
      
      nc_close(ncout)
      
}

t <- (Sys.time() - t)

cat("Programado por Ricardo Faria \n
    Finalizado em :")

print(t)

#print report
#library(rmarkdown)

# render the default (first) format defined in the file
#render("static.Rmd")