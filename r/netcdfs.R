rm(list = ls())

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
library(ncdf4)

#base Extrapolation index image from martijn, use as domain mask
base = rast('./summean_v5_base.tif')

#first load in all the data
#NDVI ###############################
ndvi = nc_open(filename = './data/input data/ei inputs/NDVIRg.2020.nc')
ndvi
lon <- ncvar_get(ndvi, "lon")
lat <- ncvar_get(ndvi, "lat", verbose = F)
t <- ncvar_get(ndvi, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')
ndvi.array <- ncvar_get(ndvi, "NDVIRg") # store the data in a 3-dimensional array

ndvi = list()
for (i in 1:46) {
  ndviar = ndvi.array[,,i]
  ndvi[[i]] = raster(t(ndviar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  ndvi[[i]] = crop(x = ndvi[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

#NDWI ################################
ndwi = nc_open(filename = './data/input data/ei inputs/NDWI.MCD43B4.006.v4_201905.2020.nc')

lon <- ncvar_get(ndwi, "lon")
lat <- ncvar_get(ndwi, "lat", verbose = F)
t <- ncvar_get(ndwi, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')
ndwi.array <- ncvar_get(ndwi, "NDWI") # store the data in a 3-dimensional array

#how to use stats on the whole year of indices
ndwi = list()
for (i in 1:46) {
  ndwiar = ndwi.array[,,i]
  ndwi[[i]] = raster(t(ndwiar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  ndwi[[i]] = crop(x = ndwi[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

#LST day ################################
lstd = nc_open(filename = './data/input data/ei inputs/LST_Day.006.v4_201905.2020.nc')

lon <- ncvar_get(lstd, "lon")
lat <- ncvar_get(lstd, "lat", verbose = F)
t <- ncvar_get(lstd, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')
lstd.array <- ncvar_get(lstd, "LST_Day") # store the data in a 3-dimensional array

#how to use stats on the whole year of indices
lstd = list()
for (i in 1:46) {
  lstdar = lstd.array[,,i]
  lstd[[i]] = raster(t(lstdar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  lstd[[i]] = crop(x = lstd[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

#LST night ################################
lstn = nc_open(filename = './data/input data/ei inputs/LST_Night.006.v4_201905.2020.nc')

lon <- ncvar_get(lstn, "lon")
lat <- ncvar_get(lstn, "lat", verbose = F)
t <- ncvar_get(lstn, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')
lstn.array <- ncvar_get(lstn, "LST_Night") # store the data in a 3-dimensional array

#how to use stats on the whole year of indices
lstn = list()
for (i in 1:46) {
  lstnar = lstn.array[,,i]
  lstn[[i]] = raster(t(lstnar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  lstn[[i]] = crop(x = lstn[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

#stack NDVI and LSTN together to calculate NDVI of snow free time
stak = list()
for (i in 1:46) {stak[[i]] = stack(ndvi[[i]],lstn[[i]])}

#for (i in 1:46) {names(stak[[i]]) = c('ndvi','ndwi','lstn')}

for (i in 1:46) {stak[[i]]$layer.2 = stak[[i]]$layer.2-273.15}

#remove all places where the LST is below freezing
for (i in 1:46) {stak[[i]][stak[[i]]$layer.2 < 0] = NA}

#calculate NDVI for snow free days
ndvis = stak[[1]]$layer.1
names(ndvis) = 'ndvi_1'
for (i in 2:46) {ndvis = stack(ndvis,stak[[i]]$layer.1)
                 names(ndvis)[i] = paste('ndvi',i,sep = '_')}

#calculate the sum of NDVI
ndvisum = calc(x = ndvis,fun = sum,na.rm = T)
ndvisum[ndvisum$layer == 0] = NA
plot(ndvisum,ylim = c(45,80))

#calculate minimum NDWI
ndwis = ndwi[[1]]$layer
names(ndwis) = 'ndwi_1'
for (i in 2:46) {ndwis = stack(ndwis,ndwi[[i]]$layer)
names(ndwis)[i] = paste('ndwi',i,sep = '_')}

ndwimin = calc(x = ndwis,fun = min)

hist(ndwimin)
plot(ndwimin,zlim = c(-0.3,0),xlim = c(-100,-80),ylim = c(70,80))
plot(ndwimin,xlim = c(-80,-60),ylim = c(68,73))
plot(ndwimin,xlim = c(-80,-60),ylim = c(68,73),zlim = c(-0.6,0.15))

#band7 ################################ great for highlighting dry areas
band7 = nc_open(filename = './data/input data/ei inputs/AMP_Band7.nc')

lon <- ncvar_get(band7, "lon")
lat <- ncvar_get(band7, "lat", verbose = F)

band7.array <- ncvar_get(band7, "AMP_Band7") # store the data in a 3-dimensional array

band7 = raster(t(band7.array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(band7)

band7 = crop(band7,extent(ndvisum))

#landcover ################################
lc = nc_open(filename = './data/input data/ei inputs/MCD12Q1plusC4_fraction.GLOBAL01KM.2001001.LC.10KM.nc')

lon <- ncvar_get(lc, "lon")
lat <- ncvar_get(lc, "lat", verbose = F)

lc.array <- ncvar_get(lc, "MCD12Q1plusC4_fraction") # store the data in a 3-dimensional array
lc = raster(t(lc.array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

lcr = raster('./data/input data/ei inputs/MCD12Q1plusC4_fraction.GLOBAL01KM.2001001.LC.10KM.nc',ncdf = T)

lc = crop(lc,extent(ndvisum))

#calculate growing degree days from the land surface temperature
gdds = lstd

for (i in 1:length(gdds)) {
  gdds[[i]]$layer = gdds[[i]]$layer - 273.15
  gdds[[i]][gdds[[i]]$layer < 0] = 0
}

gddsall = gdds[[1]]
for (i in 2:46) {
  gddsall = stack(gddsall,gdds[[i]])
}

gddsum = calc(x = gddsall,fun = sum)
plot(gddsum,ylim=c(45,80),zlim = c(0,600))

#calculate growing degree days from the land surface temperature
fdds = lstd

for (i in 1:length(fdds)) {
  fdds[[i]]$layer = fdds[[i]]$layer - 273.15
  fdds[[i]][fdds[[i]]$layer > 0] = 0
}

fddsall = fdds[[1]]
for (i in 2:46) {
  fddsall = stack(fddsall,fdds[[i]])
}

fddsum = calc(x = fddsall,fun = sum)
plot(fddsum,ylim=c(45,80))

#merge important data layers
cdfs = stack(gddsum,fddsum,ndvisum,ndwimin,band7)

names(cdfs) = c('gdd','fdd','ndvisum','ndwimin','band7')

plot(cdfs,ylim = c(48,80))

writeRaster(x = cdfs,filename = './data/input data/sat_data.tif',overwrite=T)





############################################################
#THings not used!!!!!!!!
#######################################################


#GPP ################################################################
gpp = nc_open('C:/Users/karndt/Desktop/fluxcom/RS/ALL/monthly/GPP.RS_METEO.FP-ALL.MLM-ALL.METEO-ALL.720_360.monthly.2010.nc')

lon <- ncvar_get(gpp, "lon")
lat <- ncvar_get(gpp, "lat", verbose = F)
t <- ncvar_get(gpp, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')

gpp.array <- ncvar_get(gpp, "GPP") # store the data in a 3-dimensional array

#how to use stats on the whole year of indices
gpp = list()
for (i in 1:12) {
  gppar = gpp.array[,,i]
  gpp[[i]] = raster(t(gppar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
}

gppall = gpp[[1]]
for (i in 2:12) {
  gppall = stack(gppall,gpp[[i]])
}

gppmax = calc(x = gppall,fun = max,na.rm=T)
plot(gppmax)

gppmean = calc(x = gppall,fun = mean,na.rm=T)
plot(gppmean)

gppsum = calc(x = gppall,fun = sum)
plot(gppsum)

#evi ################################
evi = nc_open(filename = 'C:/site.selection/data/input data/ei inputs/AMP_EVI.nc')

lon <- ncvar_get(evi, "lon")
lat <- ncvar_get(evi, "lat", verbose = F)

evi.array <- ncvar_get(evi, "AMP_EVI") # store the data in a 3-dimensional array

evi = raster(t(evi.array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(evi)

#MAX LST ################################
#BROAD YEARLY STATISTIC
maxlst = nc_open(filename = 'C:/site.selection/data/input data/ei inputs/MAX_LST_Day.nc')

lon <- ncvar_get(maxlst, "lon")
lat <- ncvar_get(maxlst, "lat", verbose = F)

maxlst.array <- ncvar_get(maxlst, "MAX_LST_Day") # store the data in a 3-dimensional array

maxlst = raster(t(maxlst.array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(maxlst-273.15,zlim=c(-30,2))

#LAI day ################################ DOESNT HAVE NORTHERN ISLANDS COVERED
lai = nc_open(filename = './data/input data/ei inputs/MSC_Lai.2001.nc')
lai
lon <- ncvar_get(lai, "lon")
lat <- ncvar_get(lai, "lat", verbose = F)
t <- ncvar_get(lai, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')

lai.array <- ncvar_get(lai, "MSC_Lai") # store the data in a 3-dimensional array

#how to use stats on the whole year of indices
lai = list()
for (i in 1:46) {
  laiar = lai.array[,,i]
  lai[[i]] = raster(t(laiar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
}

laiall = lai[[1]]
for (i in 2:46) {
  laiall = stack(laiall,lai[[i]])
}

ggplot()+
  layer_spatial(laiall[[30]])+
  scale_fill_viridis_c()+
  scale_y_continuous(limits = c(25,73))

plot(laiall[[26]],ylim=c(45,80))

laimean = calc(x = laiall,fun = mean,na.rm=T)
plot(laimean,ylim=c(45,80))

laimax = calc(x = laiall,fun = max,na.rm=T)
plot(laimax,ylim=c(45,80))


ggplot()+
  layer_spatial(laimax)+
  scale_fill_viridis_c(na.value = 'white')+
  scale_y_continuous(limits = c(45,80))

laimin = calc(x = laiall,fun = min,na.rm=T)
plot(laimin,ylim=c(45,80))

laisum = calc(x = laiall,fun = sum)
plot(laisum,ylim=c(45,80))
