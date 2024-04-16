rm(list = ls())

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)

#base Extrapolation index image from martijn, use as domain mask
base = rast('./base_3_towersnew.tif')
base

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

clim2 = crop(x = clim,y = extent(c(-180,180,45,80)))

#subset to the files used in the rep analysis
#clim = dropLayer(x = clim,i = c(5,6,10,11,13,15:18))

clim2 = rast(clim2)

cl = resample(x = clim2,y = base)

writeRaster(x = cl,filename = './data/input data/climate/climate_resample.tif',overwrite=T)

# add elevation data
elev = rast('./data/input data/climate/wc2.1_30s_elev/wc2.1_30s_elev.tif')
slope = terrain(x = elev,v = 'slope')
slope = project(x = slope,y = cl)

elev = stack(elev)
plot(elev,ylim=c(45,80),zlim = c(0,1000))

slope = stack(slope)
plot(slope,ylim=c(45,80),zlim=c(0,15))
