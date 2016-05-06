suppressMessages({library(shiny)
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
  library(knitr)
  #library(reshape2)
  #library(sp)
  #library(ggmap)
})


#options(shiny.error=browser)

load("output/data.RData") # output/
load("output/data_coords.RData")
load("output/data_merge.RData")
source("matrix_rotation.R")
#source("about.R", local=T)


lat_res <- abs(round((lat[1] - lat[2])/2, digits = 5))
lon_res <- abs(round((long[1] - long[2])/2, digits = 5))

#decades <- seq_time
seq_time <- seq(00, 12, by =1)
lon_view <- -16.7
lat_view <- 32.7375

#hgt to contour
#Contour <- rasterToContour(raster(hgt, xmn = long[1], xmx = long[length(long)], ymn = lat[1], ymx = lat[length(lat)], CRS('+proj=longlat +datum=WGS84')),maxpixels=100000,nlevels=10)       #toGeoJSON(as.vector(hgt), )
#Contour_Leaflet <- toGeoJSON(Contour)

#locs <- locs
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

# extract values from raster to data frame to use in popup monthly
vals<-extract(x ,1:ncell(x))
coord<-xyFromCell(x ,1:ncell(x))
combine<-cbind(coord,vals)
colnames(combine) <- c("lon", "lat", months_name)
combine <- data.frame(combine)
