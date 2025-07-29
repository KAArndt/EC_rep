
#library(raster)
#library(MASS)
#library(ggplot2)
#library(ggspatial)
library(terra)
library(svMisc)
#library(sf)

############################################################
#Data from APPEARS
############################################################
#evi indexes aren't useful
#burn data isn't useful
#nontree and nonveg is garbage above 60


#evi max ###############################################################################
#2015
files = list.files(path = 'D:/spatial_data/modis/evi/2015',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2015 = evi[[1]]
for (i in 2:length(evi)) {
  evi2015 = c(evi2015,evi[[i]])
}

#2016
files = list.files(path = 'D:/spatial_data/modis/evi/2016',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2016 = evi[[1]]
for (i in 2:length(evi)) {
  evi2016 = c(evi2016,evi[[i]])
}


#2017
files = list.files(path = 'D:/spatial_data/modis/evi/2017',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2017 = evi[[1]]
for (i in 2:length(evi)) {
  evi2017 = c(evi2017,evi[[i]])
}

#2018
files = list.files(path = 'D:/spatial_data/modis/evi/2018',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2018 = evi[[1]]
for (i in 2:length(evi)) {
  evi2018 = c(evi2018,evi[[i]])
}

#2019
files = list.files(path = 'D:/spatial_data/modis/evi/2019',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2019 = evi[[1]]
for (i in 2:length(evi)) {
  evi2019 = c(evi2019,evi[[i]])
}

#2020
files = list.files(path = 'D:/spatial_data/modis/evi/2020',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2020 = evi[[1]]
for (i in 2:length(evi)) {
  evi2020 = c(evi2020,evi[[i]])
}

#2021
files = list.files(path = 'D:/spatial_data/modis/evi/2021',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2021 = evi[[1]]
for (i in 2:length(evi)) {
  evi2021 = c(evi2021,evi[[i]])
}

#2022
files = list.files(path = 'D:/spatial_data/modis/evi/2022',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2022 = evi[[1]]
for (i in 2:length(evi)) {
  evi2022 = c(evi2022,evi[[i]])
}

#2023
files = list.files(path = 'D:/spatial_data/modis/evi/2023',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2023 = evi[[1]]
for (i in 2:length(evi)) {
  evi2023 = c(evi2023,evi[[i]])
}

#2024
files = list.files(path = 'D:/spatial_data/modis/evi/2024',full.names = T)
evi = lapply(X = files,FUN = rast)

evi2024 = evi[[1]]
for (i in 2:length(evi)) {
  evi2024 = c(evi2024,evi[[i]])
}

#calculate annual maxes
evimax2015 = app(x = evi2015,fun = max,na.rm=T)
evimax2016 = app(x = evi2016,fun = max,na.rm=T)
evimax2017 = app(x = evi2017,fun = max,na.rm=T)
evimax2018 = app(x = evi2018,fun = max,na.rm=T)
evimax2019 = app(x = evi2019,fun = max,na.rm=T)
evimax2020 = app(x = evi2020,fun = max,na.rm=T)
evimax2021 = app(x = evi2021,fun = max,na.rm=T)
evimax2022 = app(x = evi2022,fun = max,na.rm=T)
evimax2023 = app(x = evi2023,fun = max,na.rm=T)
evimax2024 = app(x = evi2024,fun = max,na.rm=T)

maxstack = c(evimax2015,
             evimax2016,
             evimax2017,
             evimax2018,
             evimax2019,
             evimax2020,
             evimax2021,
             evimax2022,
             evimax2023,
             evimax2024)
plot(maxstack)

evimax_mean = app(x = maxstack,fun = mean,na.rm=T)

plot(evi2020)
writeRaster(x = evimax_mean,filename = './spatial_data/evimax_10yrmean.tif')
