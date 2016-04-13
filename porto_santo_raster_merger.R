#!/usr/bin/env Rscript

#packages:
library(timeDate)
library(raster)
library(ncdf4)
library(maptools)


#limpeza ambiente e objetos:
rm(list=ls())
cat("\014")

#####################################
cat("Programado por Ricardo Faria \n
    ")
#####################################

t <- Sys.time()

load("output/data.RData")
source("config.txt")
source("matrix_rotation.R")

proj <- CRS("+proj=longlat +datum=WGS84 +towgs84=-160.410,-21.066,-99.282,2.437,-17.250,-7.446,0.168 +wktext +no_defs ") #CRS('+proj=longlat +datum=WGS84')
land <- readShapeSpatial("input/map/PRT_adm2.shp", proj4string = proj)

fileNames <- Sys.glob("input/resun/merge/*")

nc <- nc_open(fileNames[1])
hgt_ps <- ncvar_get(nc)[,,3]

lat_min <- min(ncvar_get(nc)[,,2])
lat_max <- max(ncvar_get(nc)[,,2])
lat_ps <- unique(as.vector(ncvar_get(nc)[,,2]))

long_min <- min(ncvar_get(nc)[,,1])
long_max <- max(ncvar_get(nc)[,,1])
long_ps <- unique(as.vector(ncvar_get(nc)[,,1]))

ncols <- length(long_ps)
nrows <- length(lat_ps)

r1 <- raster(xmx = long_max, xmn = long_min, ymx = lat_max, ymn = lat_min, ncols = ncols, nrows = nrows)

nc_close(nc)

temp_nc <- c()
IGPH_merg <- c()
#IGPH_corr_merg <- c()
IDIR_merg <- c()
IDIF_merg <- c()
DIFGPH_merg <- c()
Kt_merg <- c()
for (i in 1:length(fileNames)) {
      
      temp_nc <- nc_open(fileNames[i])
      
      IGPH_merg[[i]] <- ncvar_get(temp_nc)[,,8] 
      # crop ao contour hgt/land
      #IGPH[[1]][hgt <= 0.1] <- NA
      
      # correçao
      if (i == 1) {
            corr_fix <- x_D02 + m_D02*hgt_ps
            IGPH_merg[[1]] <- IGPH_merg[[1]] * (1 - (corr_fix)/100)
      }
      
      Kt_merg[[i]] <-ncvar_get(temp_nc)[,,4]
      IDIR_merg[[i]] <- ncvar_get(temp_nc)[,,7]
      IDIF_merg[[i]] <- ncvar_get(temp_nc)[,,6]  
      DIFGPH_merg[[i]] <- ncvar_get(temp_nc)[,,5]  
}

nc_close(temp_nc)


# criar raster files cortados pelo contorno da topografia
message("cropping & creating raster files - takes time")

raster_IGPH_merg <- c()
#raster_IGPH_corr_merg <- c()
raster_IDIR_merg <- c()
raster_IDIF_merg <- c()
raster_DIFGPH_merg <- c()
raster_Kt_merg <- c()

rasterOptions(timer = T, progress = "text")

system.time(
      for (i in 1:length(fileNames)){      
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IGPH_merg[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IGPH_merg <- c(raster_IGPH_merg, temp)
            print(paste("mes", i, "- 20%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIR_merg[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IDIR_merg <- c(raster_IDIR_merg, temp)
            print(paste("mes", i, "- 40%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(IDIF_merg[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_IDIF_merg <- c(raster_IDIF_merg, temp)
            print(paste("mes", i, "- 60%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(DIFGPH_merg[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_DIFGPH_merg <- c(raster_DIFGPH_merg, temp)
            print(paste("mes", i, "- 80%"))
            
            temp <- raster(mat_rot(mat_rot(mat_rot(t(Kt_merg[[i]])))), 
                           xmn = long_min, xmx = long_max, ymn = lat_min, ymx = lat_max, CRS("+proj=longlat +datum=WGS84"))
            if (crop == 1) {
                  temp <- crop(temp, extent(land))
                  temp <- mask(temp, land)
            }
            raster_Kt_merg <- c(raster_Kt_merg, temp)
            print(paste("mes", i, "- 100%"))
            
      }
)
#endCluster()
rm(temp)


IGPH_merg <- array(unlist(IGPH_merg), dim=c(nrows, ncols, (length(fileNames))))
IDIR_merg <- array(unlist(IDIR_merg), dim=c(nrows, ncols, (length(fileNames))))
IDIF_merg <- array(unlist(IDIF_merg), dim=c(nrows, ncols, (length(fileNames))))
DIFGPH_merg <- array(unlist(DIFGPH_merg), dim=c(nrows, ncols, (length(fileNames))))
Kt_merg <- array(unlist(Kt_merg), dim=c(nrows, ncols, (length(fileNames))))


pnts = data.frame(Name = 'Madeira', lat = lat[round(length(lat)/2)] ,lon = long[round(length(long)/2)])
#pnts = rbind(pnts,data.frame(Name = 'Tel Aviv ',lat = lat_min, lon = long_max))
coordinates(pnts) <- ~lon + lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")

#sequencia de tempo inicial e final
Times = seq(as.POSIXlt("2015-01-01"), as.POSIXlt("2015-02-28"), "months")


tS_i = as.POSIXct(timeSequence(from = "2015-01-01", to = "2015-12-31", by = "month"))
tS_f = as.POSIXct(timeLastDayInMonth(tS_i))


# passar os rasters para RasterBrickTimeSeries class
raster_IGPH_merg <- brick(raster_IGPH_merg)
names(raster_IGPH_merg) <- paste0("IGPH_", months_name[1:(length(fileNames))])

raster_IDIR_merg <- brick(raster_IDIR_merg)
names(raster_IDIR_merg) <- paste0("IDIR_", months_name[1:(length(fileNames))])

raster_IDIF_merg <- brick(raster_IDIF_merg)
names(raster_IDIF_merg) <- paste0("IDIF_", months_name[1:(length(fileNames))])

raster_DIFGPH_merg <- brick(raster_DIFGPH_merg)
names(raster_DIFGPH_merg) <- paste0("DIFGPH_", months_name[1:(length(fileNames))])

raster_Kt_merg <- brick(raster_Kt_merg)
names(raster_Kt_merg) <- paste0("Kt_", months_name[1:(length(fileNames))])

save(raster_IGPH_merg, long_ps, lat_ps, IGPH_merg, hgt_ps, file = "output/data_merge.RData")

# raster merge
#temp1 <- raster_IGPH$IGPH_tot #raster_IGPH[[1]]
#temp1_mat <- matrix(temp1[], temp1@ncols, temp1@nrows)
#res(temp1) <- c(.1, .1) round(res(temp1), 5)

#temp2 <- raster_IGPH_merg[[1]]
#temp2_mat <- matrix(temp2[], temp2@ncols, temp2@nrows)
#res(temp2) <- res(temp1)

#res(temp2) <- c(xres(temp1), yres(temp1))
#resample(temp2, temp1, method="ngb")


#rm <- merge(temp1, temp2, tolerance=0.2)

#rm_mat <- matrix(rm[], rm@ncols, rm@nrows)


#res(raster_IGPH_merg[[1]]) <- c(xres(raster_IGPH[[1]]), yres(raster_IGPH[[1]]))
#rm <- merge(raster_IGPH[[1]], raster_IGPH_merg[[1]], tolerance=0.2)


# r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30)
# r1[] <- 1:ncell(r1)
# r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
# res(r2) <- c(xres(r1), yres(r1))
# r2[] <- 1:ncell(r2)
# rm <- merge(r1, r2)

# coordenadas
# r1@extent@xmin

# matriz
# r1[]
# r1@data@values

# passar para matriz
# m1 <- matrix(r1[], r1@nrows, r1@ncols)
