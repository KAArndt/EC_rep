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
library(sf)
library(cowplot)

#gh_install_packages("SEEG-Oxford/seegSDM")

#load in sites
towers = fread(file = './data/ARGO_EC_Tower_edited.csv')
ext    = fread(file = './data/extension_sites.csv')

#sub to interested sites
ext    = subset(ext,ext$remove == 'no')
towers = subset(towers,towers$Terrestrial == TRUE)
towers$active  = ifelse(towers$End_CO2 == 2024,'active','inactive')
towers$methane = ifelse(towers$GHG == 'CO2,CH4' | 
                         towers$GHG == 'CO2,CH4,N2O',
                       'methane','nonmethane')

#reduce the existing sites and add class names
towers = towers[,c('Site_Name','Country','Longitude','Latitude','active','methane','Season_Activity')]
towers.and.ext    = rbind.fill(towers,ext)
names(towers.and.ext)[1] = 'site'
towers.and.ext = towers.and.ext[!duplicated(towers.and.ext$site),]

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
climdat$site = towers.and.ext$site

#soil grids #########################################################################
#load in the stack created in the other files
soil = rast('./data/input data/soils.tif')

#extract data
soildat = extract(x = soil,y = xy.tower,cells=T,xy=T)
nas = soildat[is.na(soildat$bd_100_agg),] #extract where nas
soilr = stack(soil) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = soilr,max_distance = 1000))

#place in original dataframe
soildat[nas$ID,] = extract(x = soil,y = na.cor,cells=T,xy=T)
summary(soildat)
soildat$site = towers.and.ext$site

#permafrost #########################################################################
#load in the stack created in the other files
pp = rast('./data/input data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')
perm = project(x = pp,y = clim)

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
permdat$site = towers.and.ext$site

#modis #########################################################################
#load in the stack created in the other files
modis = rast('./data/input data/modis.tif')

#extract data
modisdat = extract(x = modis,y = xy.tower,xy=T,cell=T)
summary(modisdat)
nas = modisdat[is.na(modisdat$ndvimax),] #extract where nas
modisr = stack(modis) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = modisr,max_distance = 2000))

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

#Add variables for projected coordinates
td = vect(geom = c("Longitude","Latitude"),x = towerdata,crs = crs(clim))
td = project(x = td,y = crs(pp))
crd = data.frame(crds(td))

towerdata$x = crd$x
towerdata$y = crd$y

towerdata = towerdata[complete.cases(towerdata$mirsaug),]

#add the class back in
write.csv(x = towerdata,file = './data/extracted_tower_data.csv',row.names = F)
