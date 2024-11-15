rm(list = ls())
gc()


library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
library(ncdf4)

#base image from rep analysis
base = rast('./output/base_2km.tif')

#first load in all the data
#NDVI ###############################
nee = nc_open(filename = './data/input data/NEE.RS.FP-NONE.MLM-ALL.METEO-NONE.4320_2160.monthly.2015.nc')
nee
lon = ncvar_get(nee, "longitude",verbose = F)
lat = ncvar_get(nee, "latitude", verbose = F)
t   = ncvar_get(nee, "time")
t   = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '1582-10-15 00:00')

nee.array = ncvar_get(nee, "NEE") # store the data in a 3-dimensional array
nee.sd    = ncvar_get(nee, "NEE_mad") # store the data in a 3-dimensional array

nee = list()
for (i in 1:length(t)) {
  neear = nee.array[,,i]
  nee[[i]] = raster(t(neear),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  nee[[i]] = crop(x = nee[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

nee.mad = list()
for (i in 1:length(t)) {
  neear = nee.sd[,,i]
  nee.mad[[i]] = raster(t(neear),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  nee.mad[[i]] = crop(x = nee.mad[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

nees = list()
for (i in 1:length(t)) {
  nees[[i]] = nee.mad[[i]]$layer/abs(nee[[i]]$layer)
}

nee.stack = nee[[1]]
for (i in 2:length(t)) {
  nee.stack = stack(nee.stack,nee[[i]])
}

nee.mad.stack = nees[[1]]
for (i in 2:length(t)) {
  nee.mad.stack = stack(nee.mad.stack,nees[[i]])
}

plot(nee.mad.stack)
plot(nee.mad.stack,zlim=c(0,20))
plot(nee.stack)

nee.mad = rast(nee.mad.stack)

nee.mad.mean = terra::mean(x = nee.mad)

plot(nee.mad.mean,range = c(0,10))

nee.mad.mean = project(x = nee.mad.mean,y = base)

plot(nee.mad.mean)
r = c(nee.mad.mean,base)

r = aggregate(x = r,FUN='mean',na.rm=T,fact=10)

df = as.data.frame(r)
df = df[complete.cases(df$mean),]
df = df[complete.cases(df$base.dist),]

ggplot(data = df)+
  geom_point(aes(mean,base.dist))+
  scale_x_continuous(limits = c(0,100))

