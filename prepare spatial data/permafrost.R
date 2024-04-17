rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

#library(raster)
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
#Newer permafrost file from 
############################################################

alt = rast('./data/input data/NIEER_permafrost_dataset_released/NIEER_ALT.tif')

#resize to the area and size of the base image
alt = crop(x = alt,y = eco)
#alt = mask(x = alt,mask = eco)

magt = rast('./data/input data/NIEER_permafrost_dataset_released/NIEER_MAGT.tif')

#resize to the area and size of the base image
magt = crop(x = magt,y = eco)
#magt = mask(x = magt,mask = eco)


pprob = rast('./data/input data/NIEER_permafrost_dataset_released/NIEER_Probability.tif')

#resize to the area and size of the base image
pprob = crop(x = pprob,y = eco)
#pprob = mask(x = pprob,mask = eco)

perm = c(alt,magt,pprob)

writeRaster(x = perm,'./data/input data/permafrost.tif',overwrite=T)

#soil temps and permafrost probability ##########################
pp = rast('./data/input data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')

pp = project(x = pp,y = soc,method = 'near')
#pp = crop(x = pp,y = eco)
pp = mask(x = pp,mask = eco)

plot(pp)

#combine all
soils = c(bd,ph,soc,sand,silt,clay,pp)

plot(soils)

writeRaster(x = soils,filename = './data/input data/soils.tif',overwrite=T)
