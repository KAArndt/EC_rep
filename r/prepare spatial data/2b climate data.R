rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)

#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

#climate files
c.files = list.files('./data/input data/climate/wc2.1_30s_bio/',full.names = T)

clim = stack(c.files[1])
for (i in 2:length(c.files)) {
  clim = stack(clim,c.files[i])  
  progress(i,length(c.files))
}

names(clim) = c('MeanTemp','WarmestQuarter','ColdestQuarter',
                'Precip','WettestMonth','DriestMonth','PrecipitationSeasonality',
                'WettestQuarter','DriestQuarter','PrecipWarmestQuarter','PrecipColdestQuarter',
                'MeanDiurnalRange','Isothermality','TempSeasonality',
                'MaxTempWarmestMonth','MinTempColdestMonth','TempAnnualRange',
                'MeanTempWettestQuarter','MeanTempDriestQuarter')

clim = rast(clim)
clim2 = crop(x = clim,y = eco)
#clim2 = mask(x = clim2,mask = eco)

#subset to the files used in the rep analysis
#clim = dropLayer(x = clim,i = c(5,6,10,11,13,15:18))

writeRaster(x = clim2,filename = './data/input data/climate.tif',overwrite=T)


# add elevation data
# elev = rast('./data/input data/climate/wc2.1_30s_elev/wc2.1_30s_elev.tif')
# slope = terrain(x = elev,v = 'slope')
# slope = project(x = slope,y = cl)
# 
# elev = stack(elev)
# plot(elev,ylim=c(45,80),zlim = c(0,1000))
# 
# slope = stack(slope)
# plot(slope,ylim=c(45,80),zlim=c(0,15))
