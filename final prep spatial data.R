rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(plyr)
library(data.table)
library(kit)
library(ggthemes)
library(sf)

#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

#load in the different data files
clim  = rast('./data/input data/climate.tif')
modis = rast('./data/input data/modis.tif')
soil  = rast('./data/input data/soils.tif')

#merge them all together
r = c(clim,modis,soil)

#mask out areas outside of the arctic boreal region
r = mask(x = r,mask = eco)
r2 = r

#mask out where any layer is NA, using one layer from each dataset
r2[is.na(r2$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)] = NA #permafrost
r2[is.na(r2$OCSTHA_M_100cm_1km_ll)] = NA #soil grids
r2[is.na(r2$MeanTemp)] = NA #worldclim
r2[is.na(r2$ndwimin)] = NA #MODIS

summary(r2)

plot(r2$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)

writeRaster(x = r2,filename = './data/input data/spatial.tif',overwrite = T)
