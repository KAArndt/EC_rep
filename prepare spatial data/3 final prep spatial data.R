rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

#library(raster)
#library(svMisc)
#library(MASS)
#library(ggplot2)
#library(ggspatial)
#library(plotrix)
library(terra)
#library(plyr)
library(data.table)
#library(kit)
#library(ggthemes)
#library(sf)

#m = rast('./data/predictions_1km_raster_agg_NEE_gC_m2_2001_2020_avg.tif')
pp = rast('./data/input data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')


#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))
eco = project(x = eco,y = pp)
plot(eco)

pp = crop(x = pp,y = eco)
pp = mask(x = pp,mask = eco)

#load in the differeneconomics#load in the different data files
clim  = rast('./data/input data/climate.tif')
clim = subset(clim,subset = -c(2,3,5,6,8:11,15,16,18,19)) #subset down to layers we want to save space
modis = rast('./data/input data/modis.tif')
soil  = rast('./data/input data/soils.tif')

#merge them all together
#r = c(clim,modis,soil)

#test with single layers, may be faster and simpler
#climate
clim2 = project(x = clim$MeanTemp,                y = pp)
clim3 = project(x = clim$Precip,                  y = pp)
clim4 = project(x = clim$PrecipitationSeasonality,y = pp)
clim5 = project(x = clim$MeanDiurnalRange,        y = pp)
clim6 = project(x = clim$Isothermality,           y = pp)
clim7 = project(x = clim$TempSeasonality,         y = pp)
clim8 = project(x = clim$TempAnnualRange,         y = pp)

#modis
modis2 = project(x = modis$ndvimax,y = pp)
modis3 = project(x = modis$ndvisum,y = pp)
modis4 = project(x = modis$evimax, y = pp)
modis5 = project(x = modis$ndwimin,y = pp)
modis6 = project(x = modis$mirsaug,y = pp)

#soil grids
soil2 = project(x = soil$bd_100_agg,           y = pp)
soil3 = project(x = soil$ph0_100,              y = pp)
soil4 = project(x = soil$OCSTHA_M_100cm_1km_ll,y = pp)
soil5 = project(x = soil$soc0_100,             y = pp)
soil6 = project(x = soil$sand_100_agg,         y = pp)
soil7 = project(x = soil$silt_100_agg,         y = pp)
soil8 = project(x = soil$clay_100_agg,         y = pp)

final.clim  = c(clim2,clim3,clim4,clim5,clim6,clim7,clim8)
final.modis = c(modis2,modis3,modis4,modis5,modis6)
final.soil  = c(soil2,soil3,soil4,soil5,soil6,soil7,soil8)

#crop and mask the stacks
eco2 = subset(eco,eco$WWF_MHTNAM != 'Rock and Ice')

final.clim2 = crop(x = final.clim,y = eco2)
final.clim2 = mask(x = final.clim2,mask = eco2)

final.modis2 = crop(x = final.modis,y = eco2)
final.modis2 = mask(x = final.modis2,mask = eco2)

final.soil2 = crop(x = final.soil,y = eco2)
final.soil2 = mask(x = final.soil2,mask = eco2)

final = c(final.clim2,final.modis2,final.soil2,pp)

#mask out where any layer is NA, using one layer from each data set
final[is.na(final$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)] = NA #permafrost
final[is.na(final$OCSTHA_M_100cm_1km_ll)] = NA #soil grids
final[is.na(final$MeanTemp)] = NA #worldclim
final[is.na(final$ndwimin)] = NA #MODIS
final[is.na(final$mirsaug)] = NA #MODIS

writeRaster(x = final,filename = './data/input data/spatial_repro.tif',overwrite = T)
