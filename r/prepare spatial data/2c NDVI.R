
library(terra)
library(svMisc)

############################################################
#Data from APPEARS (MODIS)
############################################################

#ndvi ###############################################################################
#2015
files = list.files(path = 'D:/spatial_data/modis/ndvi/2015',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2015 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2015 = c(ndvi2015,ndvi[[i]])
}

#2016
files = list.files(path = 'D:/spatial_data/modis/ndvi/2016',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2016 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2016 = c(ndvi2016,ndvi[[i]])
}

#2017
files = list.files(path = 'D:/spatial_data/modis/ndvi/2017',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2017 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2017 = c(ndvi2017,ndvi[[i]])
}

#2018
files = list.files(path = 'D:/spatial_data/modis/ndvi/2018',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2018 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2018 = c(ndvi2018,ndvi[[i]])
}

#2019
files = list.files(path = 'D:/spatial_data/modis/ndvi/2019',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2019 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2019 = c(ndvi2019,ndvi[[i]])
}

#2020
files = list.files(path = 'D:/spatial_data/modis/ndvi/2020',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2020 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2020 = c(ndvi2020,ndvi[[i]])
}

#2021
files = list.files(path = 'D:/spatial_data/modis/ndvi/2021',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2021 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2021 = c(ndvi2021,ndvi[[i]])
}

#2022
files = list.files(path = 'D:/spatial_data/modis/ndvi/2022',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2022 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2022 = c(ndvi2022,ndvi[[i]])
}

#2023
files = list.files(path = 'D:/spatial_data/modis/ndvi/2023',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2023 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2023 = c(ndvi2023,ndvi[[i]])
}

#2024
files = list.files(path = 'D:/spatial_data/modis/ndvi/2024',full.names = T)
ndvi = lapply(X = files,FUN = rast)

ndvi2024 = ndvi[[1]]
for (i in 2:length(ndvi)) {
  ndvi2024 = c(ndvi2024,ndvi[[i]])
}

#calculate annual maxes
ndvimax2015 = app(x = ndvi2015,fun = max,na.rm=T)
ndvimax2016 = app(x = ndvi2016,fun = max,na.rm=T)
ndvimax2017 = app(x = ndvi2017,fun = max,na.rm=T)
ndvimax2018 = app(x = ndvi2018,fun = max,na.rm=T)
ndvimax2019 = app(x = ndvi2019,fun = max,na.rm=T)
ndvimax2020 = app(x = ndvi2020,fun = max,na.rm=T)
ndvimax2021 = app(x = ndvi2021,fun = max,na.rm=T)
ndvimax2022 = app(x = ndvi2022,fun = max,na.rm=T)
ndvimax2023 = app(x = ndvi2023,fun = max,na.rm=T)
ndvimax2024 = app(x = ndvi2024,fun = max,na.rm=T)

maxstack = c(ndvimax2015,
             ndvimax2016,
             ndvimax2017,
             ndvimax2018,
             ndvimax2019,
             ndvimax2020,
             ndvimax2021,
             ndvimax2022,
             ndvimax2023,
             ndvimax2024)

ndvimax_mean = app(x = maxstack,fun = mean,na.rm=T)

plot(ndvimax_mean)

#calculate annual sums
ndvisum2015 = app(x = ndvi2015,fun = sum,na.rm=T)
ndvisum2016 = app(x = ndvi2016,fun = sum,na.rm=T)
ndvisum2017 = app(x = ndvi2017,fun = sum,na.rm=T)
ndvisum2018 = app(x = ndvi2018,fun = sum,na.rm=T)
ndvisum2019 = app(x = ndvi2019,fun = sum,na.rm=T)
ndvisum2020 = app(x = ndvi2020,fun = sum,na.rm=T)
ndvisum2021 = app(x = ndvi2021,fun = sum,na.rm=T)
ndvisum2022 = app(x = ndvi2022,fun = sum,na.rm=T)
ndvisum2023 = app(x = ndvi2023,fun = sum,na.rm=T)
ndvisum2024 = app(x = ndvi2024,fun = sum,na.rm=T)

sumstack = c(ndvisum2015,
             ndvisum2016,
             ndvisum2017,
             ndvisum2018,
             ndvisum2019,
             ndvisum2020,
             ndvisum2021,
             ndvisum2022,
             ndvisum2023,
             ndvisum2024)

ndvisum_mean = app(x = sumstack,fun = mean,na.rm=T)
plot(ndvisum_mean)

#save NDVIs
writeRaster(x = ndvimax_mean,filename = './spatial_data/ndvimax_10yrmean.tif')
writeRaster(x = ndvisum_mean,filename = './spatial_data/ndvisum_10yrmean.tif')
