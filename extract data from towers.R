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

#load in sites
towers = fread(file = './data/towers_sites_ABZ.csv')
ext    = fread(file = './data/extension_sites.csv')

#sub to interested sites
ext    = subset(ext,ext$use == 'yes')
towers = subset(towers,towers$use == 'yes')

#reduce the existing sites and add class names for later sparsing
towers = towers[,c('Site_Name','Country','LON','LAT','Activity','CH4','Annual_cover')]
towers.and.ext    = rbind.fill(towers,ext)
names(towers.and.ext)[1] = 'site'

#set just the coordinates for the extract
xy.tower = towers.and.ext[,c(3,4)]

#climate #########################################################################
#load in the stack created in the other files
clim = rast('./data/input data/climate.tif')

#extract data
climdat = extract(x = clim,y = xy.tower,cells=T,xy=T)
nas = climdat[is.na(climdat$MeanTemp),] #extract where nas
climr = stack(clim) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = climr,max_distance = 1000))

#place in original dataframe
climdat[nas$ID,] = extract(x = clim,y = na.cor,cells=T,xy=T)
climdat$site = towers.and.ext$Site_Name

#soil grids #########################################################################
#load in the stack created in the other files
soil = rast('./data/input data/soils.tif')
sg = subset(x = soil,subset = 1:6)

#extract data
soildat = extract(x = sg,y = xy.tower,cells=T,xy=T)
nas = soildat[is.na(soildat$bd_100_agg),] #extract where nas
soilr = stack(sg) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = soilr,max_distance = 1000))
na.cor

#place in original dataframe
soildat[nas$ID,] = extract(x = sg,y = na.cor,cells=T,xy=T)
summary(soildat)
soildat$site = towers.and.ext$Site_Name


#permafrost #########################################################################
#load in the stack created in the other files
soil = rast('./data/input data/soils.tif')
perm = subset(x = soil,subset = 7)

#extract data
permdat = extract(x = perm,y = xy.tower,cells=T,xy=T)
summary(permdat$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)
nas = permdat[is.na(permdat$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH),] #extract where nas
permr = stack(perm) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = permr,max_distance = 1000))

#place in original dataframe
permdat[nas$ID,] = extract(x = perm,y = na.cor,cells=T,xy=T)
summary(permdat)
permdat$site = towers.and.ext$Site_Name

#modis #########################################################################
#load in the stack created in the other files
modis = rast('./data/input data/modis.tif')
names(modis) = c('ndvimax','ndvisum','evimax')

#extract data
modisdat = extract(x = modis,y = xy.tower,xy=T,cell=T)
summary(modisdat)
nas = modisdat[is.na(modisdat$ndvimax),] #extract where nas
modisr = stack(modis) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = modisr,max_distance = 1000))

#place in original dataframe
modisdat[nas$ID,] = extract(x = modis,y = na.cor,cells=T,xy=T)
summary(modisdat)
modisdat$site = towers.and.ext$site

#combine all
modisdat[,c('cell','ID','x','y')] = list(NULL)
climdat[,c('cell','ID','x','y')] = list(NULL)
permdat[,c('cell','ID','x','y')] = list(NULL)
soildat[,c('cell','ID','x','y')] = list(NULL)

modisclim = merge(modisdat,climdat,by = 'site')
permsoil = merge(permdat,soildat,by = 'site')
alldata = merge(modisclim,permsoil,by = 'site')

towerdata = merge(towers.and.ext,alldata,by = 'site')


#add the class back in
write.csv(x = towerdata,file = './data/extracted_tower_data_new.csv',row.names = F)
