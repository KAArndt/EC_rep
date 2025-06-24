library(dplyr)
library(terra)
library(ncdf4)
library(raster)
library(svMisc)
library(ggplot2)

# WADM METHANE FLUX #########################################
#open netcdf files
wadm = nc_open(filename = './spatial_data/fch4_10km_emi_wad2m.nc')

#get components of netcdf
lon = ncvar_get(wadm, "lon")
lat = ncvar_get(wadm, "lat", verbose = F)
t = ncvar_get(wadm, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '2016-01-01 00:00:00')
wadm.array = ncvar_get(wadm, "fch4") # store the data in a 3-dimensional array

t16 = which(t <= as.POSIXct('2016-12-31'))
t17 = which(t >= as.POSIXct('2017-1-1') & t <= as.POSIXct('2017-12-31'))
t18 = which(t >= as.POSIXct('2018-1-1') & t <= as.POSIXct('2018-12-31'))
t19 = which(t >= as.POSIXct('2019-1-1') & t <= as.POSIXct('2019-12-31'))
t20 = which(t >= as.POSIXct('2020-1-1') & t <= as.POSIXct('2020-12-31'))
t21 = which(t >= as.POSIXct('2021-1-1') & t <= as.POSIXct('2021-12-31'))
t22 = which(t >= as.POSIXct('2022-1-1'))


#rasterize all the data
wadm16 = list()
for (i in min(t16):max(t16)) {
  wadm.ar = wadm.array[,,i]
  wadm16[[i]] = raster(t(wadm.ar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  progress(value = i,max.value = length(t16))
  #  wadm[[i]] = crop(x = wadm[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

r16 = list()
for (i in 1:length(wadm16)) {
  r16[[i]] = rast(wadm16[[i]])
}

r16s = rast(r16)
r16sum = sum(r16s,na.rm=T)

r16sum$unit = r16sum$sum/10^9*60*60*24*16

plot(r16sum$unit)




(10^9)*60*60*24*16/10000000









#load in permafrost data
pp = stack('./pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')
#project to the same extent and size of the methane maps
pp = projectRaster(from = pp,to = wadm[[1]],method = 'ngb')

#mask out areas outside of the permafrost region
for (i in 1:length(wadm)) {
  wadm[[i]][pp$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH == 0] = NA
  progress(i,length(wadm))
}

#calculate sum of each raster
fluxes = vector(length = length(t))
for (i in 1:length(wadm)) {
  fluxes[i] = cellStats(x = wadm[[i]],stat = sum,na.rm=T)
  progress(i,length(wadm))
}

rm(wadm)
rm(wadm.array)
rm(wadm.ar)
gc()

# Wwadm# WADM METHANE FLUX ERROR #########################################
#open netcdf files
err = nc_open(filename = './fch4_10km_emi_uncertainty_wad2m.nc')

#get components of netcdf
lon = ncvar_get(err, "lon")
lat = ncvar_get(err, "lat", verbose = F)
t = ncvar_get(err, "time")
t = as.POSIXct(x = t*24*3600,tz = 'UTC',origin = '2016-01-01 00:00:00')
err.array = ncvar_get(err, "fch4") # store the data in a 3-dimensional array

#rasterize all the data
err = list()
for (i in 1:length(t)) {
  err.ar = err.array[,,i]
  err[[i]] = raster(t(err.ar),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                    crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  progress(value = i,max.value = length(t))
  #  err[[i]] = crop(x = err[[i]],y = extent(c(-179.9583,179.9583,45,85)))
}

#mask out areas outside of the permafrost region
for (i in 1:length(err)) {
  err[[i]][pp$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH == 0] = NA
  err[[i]][err[[i]]$layer > 2e36] = NA
  progress(i,length(err))
}

#calculate sum of each raster
errs = vector(length = length(t))
for (i in 1:length(err)) {
  errs[i] = cellStats(x = err[[i]],stat = sum,na.rm=T)
  progress(i,length(err))
}

#put all the sums into a constant dataframe with the dates
fch4 = data.frame(fluxes/10^12,errs/10^12,t)
names(fch4) = c('flux.tg.m2.d','err.tg.m2.d','date')


#check it out
library(ggplot2)
ggplot(data = fch4)+
  geom_line(aes(date,flux.tg.m2.d))+
  geom_ribbon(aes(x = date,y = flux.tg.m2.d,
                  ymax = flux.tg.m2.d + err.tg.m2.d,
                  ymin = flux.tg.m2.d - err.tg.m2.d))

#summary stats by year
fch4$year = format(fch4$date,'%Y')

ag = fch4 %>%
  group_by(year) %>%
  summarise(fch4 = sum(flux.tg.m2.d,na.rm=T),
            err  = sum(err.tg.m2.d,na.rm = T))

mean  = sum(ag$fch4/(ag$err^2))/sum(1/ag$err^2)*.75
error = sqrt(sum(1/(ag$err^2)))*.75

12/16
ggplot(data = ag)+
  geom_pointrange(aes(x = year,y = fch4,
                      ymax = fch4 + err,ymin = fch4 - err))

