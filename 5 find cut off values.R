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

??gh_install_packages
gh_install_packages("SEEG-Oxford/seegSDM")
devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towers.csv')

#set just the coordinates for the extract
xy.tower = tower.data[,c(32,33)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')
clust = clust$km30
plot(clust)

#extract data
clustdat = extract(x = clust,y = xy.tower,cells=T,xy=T)
nas = clustdat[is.na(clustdat$km30),] #extract where nas
clustr = stack(clust) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c('x','y')],raster = clustr,max_distance = 1000))

#place in original dataframe
clustdat[nas$ID,] = extract(x = clust,y = na.cor,cells=T,xy=T)
clustdat$site = towers.and.ext$site

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
td = vect(geom = c("LON","LAT"),x = towerdata,crs = crs(clim))
td = project(x = td,y = crs(pp))
crd = data.frame(crds(td))

towerdata$x = crd$x
towerdata$y = crd$y

towerdata = towerdata[complete.cases(towerdata$mirsaug),]

#add the class back in
write.csv(x = towerdata,file = './data/extracted_tower_data_new.csv',row.names = F)
