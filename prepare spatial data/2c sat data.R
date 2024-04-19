rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
#library(sf)

#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
                 eco$WWF_MHTNAM == 'Tundra' |
                 eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

############################################################
#Data from APPEARS
############################################################
#evi indexes aren't useful
#burn data isn't useful
#nontree and nonveg is garbage above 60

#evi
files = list.files(path = './data/input data/modis/evi/',full.names = T)
evi = lapply(X = files,FUN = rast)

evis = evi[[1]]
for (i in 2:length(evi)) {
  evis = c(evis,evi[[i]])
}
#plot(evis)

#ndvi
files = list.files(path = './data/input data/modis/ndvi/',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvis = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvis = c(ndvis,ndvi[[i]])
}
#plot(ndvis)

#nir
files = list.files(path = './data/input data/modis/nir/',full.names = T)
nir = lapply(X = files,FUN = rast)

nirs = nir[[1]]
for (i in 2:length(nir)) {
  nirs = c(nirs,nir[[i]])
}
#plot(nirs)

#mir
files = list.files(path = './data/input data/modis/mir/',full.names = T)
mir = lapply(X = files,FUN = rast)

mirs = mir[[1]]
for (i in 2:length(mir)) {
  mirs = c(mirs,mir[[i]])
}

#calculate NDWI using the bands above
 ndwis1 = (nirs[[1]] - mirs[[1]])/(nirs[[1]] + mirs[[1]])
 ndwis2 = (nirs[[2]] - mirs[[2]])/(nirs[[2]] + mirs[[2]])
 ndwis3 = (nirs[[3]] - mirs[[3]])/(nirs[[3]] + mirs[[3]])
 ndwis4 = (nirs[[4]] - mirs[[4]])/(nirs[[4]] + mirs[[4]])
 ndwis5 = (nirs[[5]] - mirs[[5]])/(nirs[[5]] + mirs[[5]])
 ndwis6 = (nirs[[6]] - mirs[[6]])/(nirs[[6]] + mirs[[6]])
 ndwis7 = (nirs[[7]] - mirs[[7]])/(nirs[[7]] + mirs[[7]])
 ndwis8 = (nirs[[8]] - mirs[[8]])/(nirs[[8]] + mirs[[8]])
 ndwis9 = (nirs[[9]] - mirs[[9]])/(nirs[[9]] + mirs[[9]])
 ndwis10 = (nirs[[10]] - mirs[[10]])/(nirs[[10]] + mirs[[10]])
 ndwis11 = (nirs[[11]] - mirs[[11]])/(nirs[[11]] + mirs[[11]])
 ndwis12 = (nirs[[12]] - mirs[[12]])/(nirs[[12]] + mirs[[12]])

 ndwis = c(ndwis1,ndwis2,ndwis3,ndwis4,
           ndwis5,ndwis6,ndwis7,ndwis8,
           ndwis9,ndwis10,ndwis11,ndwis12)
#break down and calculate what we want
ndvimax = app(x = ndvis,fun = max,na.rm=T)
#plot(ndvimax)

ndvisum = app(x = ndvis,fun = sum,na.rm=T)
#plot(ndvisum)

evimax = app(x = evis,fun = max,na.rm=T)
#plot(evimax)

ndwimin = app(x = ndwis,fun = min,na.rm=T)
ndwimin[ndwimin > 1] = NA
ndwimin[ndwimin < -1] = NA
#plot(ndwimin)

mirsaug = mirs[[8]]
hist(mirsaug)
mirsaug[mirsaug > 0.5] = NA
plot(mirsaug)

modis = c(ndvimax,ndvisum,evimax,ndwimin,mirsaug)
names(modis) = c('ndvimax','ndvisum','evimax','ndwimin','mirsaug')
#resize to the area and size of the base image
modis = crop(x = modis,y = eco)
#bd = mask(x = bd,mask = eco)

writeRaster(x = modis,filename = './data/input data/modis.tif',overwrite=T)
