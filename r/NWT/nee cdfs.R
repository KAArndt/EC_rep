
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
library(ncdf4)

#setwd('C:/Users/karndt.WHRC/Documents/GitHub/EC_rep/')

#first load in all the data
#2001 ###############################
nee2001 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2001.nc')

east  = ncvar_get(nee2001, "easting")
north = ncvar_get(nee2001, "northing")
time  = ncvar_get(nee2001, "time")
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

writeRaster(x = nee.sum.2001,file = './spatial_data/nee/nee2001sum.tif')
rm(list = ls())

#2002 ###############################

nee2002 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2002.nc')

east  = ncvar_get(nee2002, "easting")
north = ncvar_get(nee2002, "northing")
time  = ncvar_get(nee2002, "time")
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

writeRaster(x = nee.sum.2002,file = './spatial_data/nee/nee2002sum.tif')
rm(list = ls())

#2003 ###############################
nee2003 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2003.nc')

east  = ncvar_get(nee2003, "easting")
north = ncvar_get(nee2003, "northing")
time  = ncvar_get(nee2003, "time")
nee.array   = ncvar_get(nee2003, "NEE") # store the data in a 3-dimensional array

nee2003 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2003[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2003 = sum(nee2003[[1]],nee2003[[2]],nee2003[[3]],
                   nee2003[[4]],nee2003[[5]],nee2003[[6]],
                   nee2003[[7]],nee2003[[8]],nee2003[[9]],
                   nee2003[[10]],nee2003[[11]],nee2003[[12]])

writeRaster(x = nee.sum.2003,file = './spatial_data/nee/nee2003sum.tif')
rm(list = ls())

#2004 ###############################
nee2004 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2004.nc')

east  = ncvar_get(nee2004, "easting")
north = ncvar_get(nee2004, "northing")
time  = ncvar_get(nee2004, "time")
nee.array   = ncvar_get(nee2004, "NEE") # store the data in a 3-dimensional array

nee2004 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2004[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2004 = sum(nee2004[[1]],nee2004[[2]],nee2004[[3]],
                   nee2004[[4]],nee2004[[5]],nee2004[[6]],
                   nee2004[[7]],nee2004[[8]],nee2004[[9]],
                   nee2004[[10]],nee2004[[11]],nee2004[[12]])

writeRaster(x = nee.sum.2004,file = './spatial_data/nee/nee2004sum.tif')
rm(list = ls())

#2005 ###############################
nee2005 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2005.nc')

east  = ncvar_get(nee2005, "easting")
north = ncvar_get(nee2005, "northing")
time  = ncvar_get(nee2005, "time")
nee.array   = ncvar_get(nee2005, "NEE") # store the data in a 3-dimensional array

nee2005 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2005[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2005 = sum(nee2005[[1]],nee2005[[2]],nee2005[[3]],
                   nee2005[[4]],nee2005[[5]],nee2005[[6]],
                   nee2005[[7]],nee2005[[8]],nee2005[[9]],
                   nee2005[[10]],nee2005[[11]],nee2005[[12]])

writeRaster(x = nee.sum.2005,file = './spatial_data/nee/nee2005sum.tif')
rm(list = ls())

#2006 ###############################
nee2006 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2006.nc')

east  = ncvar_get(nee2006, "easting")
north = ncvar_get(nee2006, "northing")
time  = ncvar_get(nee2006, "time")
nee.array   = ncvar_get(nee2006, "NEE") # store the data in a 3-dimensional array

nee2006 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2006[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2006 = sum(nee2006[[1]],nee2006[[2]],nee2006[[3]],
                   nee2006[[4]],nee2006[[5]],nee2006[[6]],
                   nee2006[[7]],nee2006[[8]],nee2006[[9]],
                   nee2006[[10]],nee2006[[11]],nee2006[[12]])

writeRaster(x = nee.sum.2006,file = './spatial_data/nee/nee2006sum.tif')
rm(list = ls())

#2007 ###############################
nee2007 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2007.nc')

east  = ncvar_get(nee2007, "easting")
north = ncvar_get(nee2007, "northing")
time  = ncvar_get(nee2007, "time")
nee.array   = ncvar_get(nee2007, "NEE") # store the data in a 3-dimensional array

nee2007 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2007[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2007 = sum(nee2007[[1]],nee2007[[2]],nee2007[[3]],
                   nee2007[[4]],nee2007[[5]],nee2007[[6]],
                   nee2007[[7]],nee2007[[8]],nee2007[[9]],
                   nee2007[[10]],nee2007[[11]],nee2007[[12]])

writeRaster(x = nee.sum.2007,file = './spatial_data/nee/nee2007sum.tif')
rm(list = ls())

#2008 ###############################
nee2008 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2008.nc')

east  = ncvar_get(nee2008, "easting")
north = ncvar_get(nee2008, "northing")
time  = ncvar_get(nee2008, "time")
nee.array   = ncvar_get(nee2008, "NEE") # store the data in a 3-dimensional array

nee2008 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2008[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2008 = sum(nee2008[[1]],nee2008[[2]],nee2008[[3]],
                   nee2008[[4]],nee2008[[5]],nee2008[[6]],
                   nee2008[[7]],nee2008[[8]],nee2008[[9]],
                   nee2008[[10]],nee2008[[11]],nee2008[[12]])

writeRaster(x = nee.sum.2008,file = './spatial_data/nee/nee2008sum.tif')
rm(list = ls())

#2009 ###############################
nee2009 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2009.nc')

east  = ncvar_get(nee2009, "easting")
north = ncvar_get(nee2009, "northing")
time  = ncvar_get(nee2009, "time")
nee.array   = ncvar_get(nee2009, "NEE") # store the data in a 3-dimensional array

nee2009 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2009[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2009 = sum(nee2009[[1]],nee2009[[2]],nee2009[[3]],
                   nee2009[[4]],nee2009[[5]],nee2009[[6]],
                   nee2009[[7]],nee2009[[8]],nee2009[[9]],
                   nee2009[[10]],nee2009[[11]],nee2009[[12]])

writeRaster(x = nee.sum.2009,file = './spatial_data/nee/nee2009sum.tif')
rm(list = ls())

#2010 ###############################
nee2010 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2010.nc')

east  = ncvar_get(nee2010, "easting")
north = ncvar_get(nee2010, "northing")
time  = ncvar_get(nee2010, "time")
nee.array   = ncvar_get(nee2010, "NEE") # store the data in a 3-dimensional array

nee2010 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2010[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2010 = sum(nee2010[[1]],nee2010[[2]],nee2010[[3]],
                   nee2010[[4]],nee2010[[5]],nee2010[[6]],
                   nee2010[[7]],nee2010[[8]],nee2010[[9]],
                   nee2010[[10]],nee2010[[11]],nee2010[[12]])

writeRaster(x = nee.sum.2010,file = './spatial_data/nee/nee2010sum.tif')
rm(list = ls())

#2011 ###############################
nee2011 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2011.nc')

east  = ncvar_get(nee2011, "easting")
north = ncvar_get(nee2011, "northing")
time  = ncvar_get(nee2011, "time")
nee.array   = ncvar_get(nee2011, "NEE") # store the data in a 3-dimensional array

nee2011 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2011[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2011 = sum(nee2011[[1]],nee2011[[2]],nee2011[[3]],
                   nee2011[[4]],nee2011[[5]],nee2011[[6]],
                   nee2011[[7]],nee2011[[8]],nee2011[[9]],
                   nee2011[[10]],nee2011[[11]],nee2011[[12]])

writeRaster(x = nee.sum.2011,file = './spatial_data/nee/nee2011sum.tif')
rm(list = ls())

#2012 ###############################
nee2012 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2012.nc')

east  = ncvar_get(nee2012, "easting")
north = ncvar_get(nee2012, "northing")
time  = ncvar_get(nee2012, "time")
nee.array   = ncvar_get(nee2012, "NEE") # store the data in a 3-dimensional array

nee2012 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2012[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2012 = sum(nee2012[[1]],nee2012[[2]],nee2012[[3]],
                   nee2012[[4]],nee2012[[5]],nee2012[[6]],
                   nee2012[[7]],nee2012[[8]],nee2012[[9]],
                   nee2012[[10]],nee2012[[11]],nee2012[[12]])

writeRaster(x = nee.sum.2012,file = './spatial_data/nee/nee2012sum.tif')
rm(list = ls())

#2013 ###############################
nee2013 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2013.nc')

east  = ncvar_get(nee2013, "easting")
north = ncvar_get(nee2013, "northing")
time  = ncvar_get(nee2013, "time")
nee.array   = ncvar_get(nee2013, "NEE") # store the data in a 3-dimensional array

nee2013 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2013[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2013 = sum(nee2013[[1]],nee2013[[2]],nee2013[[3]],
                   nee2013[[4]],nee2013[[5]],nee2013[[6]],
                   nee2013[[7]],nee2013[[8]],nee2013[[9]],
                   nee2013[[10]],nee2013[[11]],nee2013[[12]])

writeRaster(x = nee.sum.2013,file = './spatial_data/nee/nee2013sum.tif')
rm(list = ls())

#2014 ###############################
nee2014 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2014.nc')

east  = ncvar_get(nee2014, "easting")
north = ncvar_get(nee2014, "northing")
time  = ncvar_get(nee2014, "time")
nee.array   = ncvar_get(nee2014, "NEE") # store the data in a 3-dimensional array

nee2014 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2014[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2014 = sum(nee2014[[1]],nee2014[[2]],nee2014[[3]],
                   nee2014[[4]],nee2014[[5]],nee2014[[6]],
                   nee2014[[7]],nee2014[[8]],nee2014[[9]],
                   nee2014[[10]],nee2014[[11]],nee2014[[12]])

writeRaster(x = nee.sum.2014,file = './spatial_data/nee/nee2014sum.tif')
rm(list = ls())

#2015 ###############################
nee2015 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2015.nc')

east  = ncvar_get(nee2015, "easting")
north = ncvar_get(nee2015, "northing")
time  = ncvar_get(nee2015, "time")
nee.array   = ncvar_get(nee2015, "NEE") # store the data in a 3-dimensional array

nee2015 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2015[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2015 = sum(nee2015[[1]],nee2015[[2]],nee2015[[3]],
                   nee2015[[4]],nee2015[[5]],nee2015[[6]],
                   nee2015[[7]],nee2015[[8]],nee2015[[9]],
                   nee2015[[10]],nee2015[[11]],nee2015[[12]])

writeRaster(x = nee.sum.2015,file = './spatial_data/nee/nee2015sum.tif')
rm(list = ls())

#2016 ###############################
nee2016 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2016.nc')

east  = ncvar_get(nee2016, "easting")
north = ncvar_get(nee2016, "northing")
time  = ncvar_get(nee2016, "time")
nee.array   = ncvar_get(nee2016, "NEE") # store the data in a 3-dimensional array

nee2016 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2016[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2016 = sum(nee2016[[1]],nee2016[[2]],nee2016[[3]],
                   nee2016[[4]],nee2016[[5]],nee2016[[6]],
                   nee2016[[7]],nee2016[[8]],nee2016[[9]],
                   nee2016[[10]],nee2016[[11]],nee2016[[12]])

writeRaster(x = nee.sum.2016,file = './spatial_data/nee/nee2016sum.tif')
rm(list = ls())

#2017 ###############################
nee2017 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2017.nc')

east  = ncvar_get(nee2017, "easting")
north = ncvar_get(nee2017, "northing")

time  = ncvar_get(nee2017, "time")
nee.array   = ncvar_get(nee2017, "NEE") # store the data in a 3-dimensional array

nee2017 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2017[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2017 = sum(nee2017[[1]],nee2017[[2]],nee2017[[3]],
                   nee2017[[4]],nee2017[[5]],nee2017[[6]],
                   nee2017[[7]],nee2017[[8]],nee2017[[9]],
                   nee2017[[10]],nee2017[[11]],nee2017[[12]])

writeRaster(x = nee.sum.2017,file = './spatial_data/nee/nee2017sum.tif')
rm(list = ls())

#2018 ###############################
nee2018 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2018.nc')

east  = ncvar_get(nee2018, "easting")
north = ncvar_get(nee2018, "northing")

time  = ncvar_get(nee2018, "time")
nee.array   = ncvar_get(nee2018, "NEE") # store the data in a 3-dimensional array

nee2018 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2018[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2018 = sum(nee2018[[1]],nee2018[[2]],nee2018[[3]],
                   nee2018[[4]],nee2018[[5]],nee2018[[6]],
                   nee2018[[7]],nee2018[[8]],nee2018[[9]],
                   nee2018[[10]],nee2018[[11]],nee2018[[12]])

writeRaster(x = nee.sum.2018,file = './spatial_data/nee/nee2018sum.tif')
rm(list = ls())

#2019 ###############################
nee2019 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2019.nc')

east  = ncvar_get(nee2019, "easting")
north = ncvar_get(nee2019, "northing")

time  = ncvar_get(nee2019, "time")
nee.array   = ncvar_get(nee2019, "NEE") # store the data in a 3-dimensional array

nee2019 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2019[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2019 = sum(nee2019[[1]],nee2019[[2]],nee2019[[3]],
                   nee2019[[4]],nee2019[[5]],nee2019[[6]],
                   nee2019[[7]],nee2019[[8]],nee2019[[9]],
                   nee2019[[10]],nee2019[[11]],nee2019[[12]])

writeRaster(x = nee.sum.2019,file = './spatial_data/nee/nee2019sum.tif')
rm(list = ls())

#2020 ###############################
nee2020 = nc_open(filename = './spatial_data/nee/CO2Fluxes_Arctic_Boreal_NEE_2020.nc')

east  = ncvar_get(nee2020, "easting")
north = ncvar_get(nee2020, "northing")
time  = ncvar_get(nee2020, "time")
nee.array   = ncvar_get(nee2020, "NEE") # store the data in a 3-dimensional array

nee2020 = list()
for (i in 1:length(time)) {
  neear = nee.array[,,i]
  nee2020[[i]] = raster(t(neear),xmn=min(east), xmx=max(east), ymn=min(north), ymx=max(north), crs=CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
}

#calculate the mean for the year
nee.sum.2020 = sum(nee2020[[1]],nee2020[[2]],nee2020[[3]],
                   nee2020[[4]],nee2020[[5]],nee2020[[6]],
                   nee2020[[7]],nee2020[[8]],nee2020[[9]],
                   nee2020[[10]],nee2020[[11]],nee2020[[12]])

writeRaster(x = nee.sum.2020,file = './spatial_data/nee/nee2020sum.tif')
rm(list = ls())