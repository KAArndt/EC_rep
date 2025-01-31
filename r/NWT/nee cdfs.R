
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
library(ncdf4)

setwd('C:/Users/karndt.WHRC/Documents/GitHub/EC_rep/spatial_data/nee')

#first load in all the data
#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2002 ###############################
nee2002 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2002.nc')
nee2002
east  = ncvar_get(nee2002, "easting")
north = ncvar_get(nee2002, "northing")

time  = ncvar_get(nee2002, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2002, "NEE") # store the data in a 3-dimensional array

nee2002 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2002[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2002 = sum(nee2002[[1]],nee2002[[2]],nee2002[[3]],
                   nee2002[[4]],nee2002[[5]],nee2002[[6]],
                   nee2002[[7]],nee2002[[8]],nee2002[[9]],
                   nee2002[[10]],nee2002[[11]],nee2002[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])

#2001 ###############################
nee2001 = nc_open(filename = './CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")

time  = ncvar_get(nee2001, "time")
time  = as.POSIXct(x = time*24*3600,tz = 'UTC',origin = '2001-01-01 00:00')
nee.array   = ncvar_get(nee2001, "NEE") # store the data in a 3-dimensional array

nee2001 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2001[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2001 = sum(nee2001[[1]],nee2001[[2]],nee2001[[3]],
                   nee2001[[4]],nee2001[[5]],nee2001[[6]],
                   nee2001[[7]],nee2001[[8]],nee2001[[9]],
                   nee2001[[10]],nee2001[[11]],nee2001[[12]])
