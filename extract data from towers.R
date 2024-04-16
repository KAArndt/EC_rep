rm(list = ls())

library(data.table)
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(seegSDM)
library(plyr)

setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

base = rast("./data/summean_v5_base.tif")

#load in the stack created in the other file
clim = rast('./data/input data/climate_resample.tif')
sat  = rast('./data/input data/sat_data.tif')
soil = rast('./data/input data/soils.tif')

sat = resample(x = sat,y = base)
clim = resample(x = clim,y = base)
soil = resample(x = soil,y = base)

names(sat) = c('gdd','fdd','ndvisum','ndwimin','band7')

plot(sat)
r = c(clim,sat,soil)
names(r)
r = subset(x = r,subset = c(1:4,12:17,20:30))

#r[r$ndwimin > 0.1] = NA

#load in sites
towers = fread(file = './data/towers_sites_11_23.csv')
ext    = fread(file = './data/extension_sites.csv')

#sub to interested sites
ext = subset(ext,ext$remove == 'no')
towers = subset(towers,towers$Ignore != 'yes')

#reduce the existing sites and add class names for later sparsing
towers = towers[,c('Site_Name','Country','LON','LAT','Activity','CH4','Annual_cover','Supported')]
towers.and.ext    = rbind.fill(towers,ext)

#set just the coordinates for the extract
xy.tower = towers.and.ext[,c(3,4)]

towerdata = extract(x = r,y = xy.tower,small=T,buffer = 10^20,na.rm = T)

r = raster(r$MeanTemp)

orig = Sys.time()
sampled = apply(X = xy.tower, MARGIN = 1, FUN = function(xy.tower) r@data@values[which.min(replace(distanceFromPoints(r, xy.tower), is.na(r), NA))])
Sys.time() - orig






### Method 3 (nearestLand) #####################################################
for (i in 1:dim(r)[3]) {
 
  rdat = extract(x = r[[i]],y = xy.tower,df = T,cells = T,xy = T)
  NAs  = rdat[is.na(rdat[,2]),]
  
  if (nrow(NAs) > 0) {
    # find coordinates of nearest non-NA cell
    co = nearestLand(NAs[,c(4,5)], raster(r[[i]]), max_distance = 200000)
    
    # extract values of nearest non-NA cell with coordinates co
    NAVals = raster::extract(r[[i]], co, method='simple')
    
    # Add data to raster
    r[[i]][NAs[,'cell']] <- NAVals
  }
  progress(i,dim(r)[3])
}

towerdata = extract(r,y = xy.tower,df = T,cells = T,xy = T,method = 'bilinear')
?extract

tower.data = extract(x = r,y = xy.tower,df = T,cells = T,xy = T)
tower.data$sitename = towers.and.ext$Site_Name
tower.data$country = towers.and.ext$Country

tower.data$ch4 = towers.and.ext$CH4
tower.data$annual = towers.and.ext$Annual_cover
tower.data$active = towers.and.ext$Activity
tower.data$support = towers.and.ext$Supported

tower.data = tower.data[complete.cases(tower.data$MeanTemp),]  
summary(tower.data)

#add the class back in
write.csv(x = tower.data,file = './data/extracted_tower_data.csv',row.names = F)
