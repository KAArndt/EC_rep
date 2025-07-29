
library(terra)
library(svMisc)

############################################################
#Data from APPEARS (MODIS)
############################################################

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

#nir ##################################################################################################
#2015
files = list.files(path = 'D:/spatial_data/modis/nir/2015',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2015 = nir[[1]]
for (i in 2:length(nir)) {
  nir2015 = c(nir2015,nir[[i]])
}

#2016
files = list.files(path = 'D:/spatial_data/modis/nir/2016',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2016 = nir[[1]]
for (i in 2:length(nir)) {
  nir2016 = c(nir2016,nir[[i]])
}

#2017
files = list.files(path = 'D:/spatial_data/modis/nir/2017',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2017 = nir[[1]]
for (i in 2:length(nir)) {
  nir2017 = c(nir2017,nir[[i]])
}

#2018
files = list.files(path = 'D:/spatial_data/modis/nir/2018',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2018 = nir[[1]]
for (i in 2:length(nir)) {
  nir2018 = c(nir2018,nir[[i]])
}

#2019
files = list.files(path = 'D:/spatial_data/modis/nir/2019',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2019 = nir[[1]]
for (i in 2:length(nir)) {
  nir2019 = c(nir2019,nir[[i]])
}

#2020
files = list.files(path = 'D:/spatial_data/modis/nir/2020',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2020 = nir[[1]]
for (i in 2:length(nir)) {
  nir2020 = c(nir2020,nir[[i]])
}

#2021
files = list.files(path = 'D:/spatial_data/modis/nir/2021',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2021 = nir[[1]]
for (i in 2:length(nir)) {
  nir2021 = c(nir2021,nir[[i]])
}

#2022
files = list.files(path = 'D:/spatial_data/modis/nir/2022',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2022 = nir[[1]]
for (i in 2:length(nir)) {
  nir2022 = c(nir2022,nir[[i]])
}

#2023
files = list.files(path = 'D:/spatial_data/modis/nir/2023',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2023 = nir[[1]]
for (i in 2:length(nir)) {
  nir2023 = c(nir2023,nir[[i]])
}

#2024
files = list.files(path = 'D:/spatial_data/modis/nir/2024',full.names = T)
nir = lapply(X = files,FUN = rast)

nir2024 = nir[[1]]
for (i in 2:length(nir)) {
  nir2024 = c(nir2024,nir[[i]])
}

#mir #################################################################################################
#2015
files = list.files(path = 'D:/spatial_data/modis/mir/2015',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2015 = mir[[1]]
for (i in 2:length(mir)) {
  mir2015 = c(mir2015,mir[[i]])
}

#2016
files = list.files(path = 'D:/spatial_data/modis/mir/2016',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2016 = mir[[1]]
for (i in 2:length(mir)) {
  mir2016 = c(mir2016,mir[[i]])
}

#2017
files = list.files(path = 'D:/spatial_data/modis/mir/2017',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2017 = mir[[1]]
for (i in 2:length(mir)) {
  mir2017 = c(mir2017,mir[[i]])
}

#2018
files = list.files(path = 'D:/spatial_data/modis/mir/2018',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2018 = mir[[1]]
for (i in 2:length(mir)) {
  mir2018 = c(mir2018,mir[[i]])
}

#2019
files = list.files(path = 'D:/spatial_data/modis/mir/2019',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2019 = mir[[1]]
for (i in 2:length(mir)) {
  mir2019 = c(mir2019,mir[[i]])
}

#2020
files = list.files(path = 'D:/spatial_data/modis/mir/2020',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2020 = mir[[1]]
for (i in 2:length(mir)) {
  mir2020 = c(mir2020,mir[[i]])
}

#2021
files = list.files(path = 'D:/spatial_data/modis/mir/2021',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2021 = mir[[1]]
for (i in 2:length(mir)) {
  mir2021 = c(mir2021,mir[[i]])
}

#2022
files = list.files(path = 'D:/spatial_data/modis/mir/2022',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2022 = mir[[1]]
for (i in 2:length(mir)) {
  mir2022 = c(mir2022,mir[[i]])
}

#2023
files = list.files(path = 'D:/spatial_data/modis/mir/2023',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2023 = mir[[1]]
for (i in 2:length(mir)) {
  mir2023 = c(mir2023,mir[[i]])
}

#2024
files = list.files(path = 'D:/spatial_data/modis/mir/2024',full.names = T)
mir = lapply(X = files,FUN = rast)

mir2024 = mir[[1]]
for (i in 2:length(mir)) {
  mir2024 = c(mir2024,mir[[i]])
}

#calculate NDWI for every image ###########################################################################
ndwi2015 = (nir2015[[1]] - mir2015[[1]])/(nir2015[[1]] + mir2015[[1]])
for (i in 2:12) {
  ndwi_step = (nir2015[[i]] - mir2015[[i]])/(nir2015[[i]] + mir2015[[i]])
  ndwi2015 = c(ndwi2015,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2016 = (nir2016[[1]] - mir2016[[1]])/(nir2016[[1]] + mir2016[[1]])
for (i in 2:12) {
  ndwi_step = (nir2016[[i]] - mir2016[[i]])/(nir2016[[i]] + mir2016[[i]])
  ndwi2016 = c(ndwi2016,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2017 = (nir2017[[1]] - mir2017[[1]])/(nir2017[[1]] + mir2017[[1]])
for (i in 2:12) {
  ndwi_step = (nir2017[[i]] - mir2017[[i]])/(nir2017[[i]] + mir2017[[i]])
  ndwi2017 = c(ndwi2017,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2018 = (nir2018[[1]] - mir2018[[1]])/(nir2018[[1]] + mir2018[[1]])
for (i in 2:12) {
  ndwi_step = (nir2018[[i]] - mir2018[[i]])/(nir2018[[i]] + mir2018[[i]])
  ndwi2018 = c(ndwi2018,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2019 = (nir2019[[1]] - mir2019[[1]])/(nir2019[[1]] + mir2019[[1]])
for (i in 2:12) {
  ndwi_step = (nir2019[[i]] - mir2019[[i]])/(nir2019[[i]] + mir2019[[i]])
  ndwi2019 = c(ndwi2019,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2020 = (nir2020[[1]] - mir2020[[1]])/(nir2020[[1]] + mir2020[[1]])
for (i in 2:12) {
  ndwi_step = (nir2020[[i]] - mir2020[[i]])/(nir2020[[i]] + mir2020[[i]])
  ndwi2020 = c(ndwi2020,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2021 = (nir2021[[1]] - mir2021[[1]])/(nir2021[[1]] + mir2021[[1]])
for (i in 2:12) {
  ndwi_step = (nir2021[[i]] - mir2021[[i]])/(nir2021[[i]] + mir2021[[i]])
  ndwi2021 = c(ndwi2021,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2022 = (nir2022[[1]] - mir2022[[1]])/(nir2022[[1]] + mir2022[[1]])
for (i in 2:12) {
  ndwi_step = (nir2022[[i]] - mir2022[[i]])/(nir2022[[i]] + mir2022[[i]])
  ndwi2022 = c(ndwi2022,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2023 = (nir2023[[1]] - mir2023[[1]])/(nir2023[[1]] + mir2023[[1]])
for (i in 2:12) {
  ndwi_step = (nir2023[[i]] - mir2023[[i]])/(nir2023[[i]] + mir2023[[i]])
  ndwi2023 = c(ndwi2023,ndwi_step)
  progress(value = i,max.value = 12)
}

ndwi2024 = (nir2024[[1]] - mir2024[[1]])/(nir2024[[1]] + mir2024[[1]])
for (i in 2:12) {
  ndwi_step = (nir2024[[i]] - mir2024[[i]])/(nir2024[[i]] + mir2024[[i]])
  ndwi2024 = c(ndwi2024,ndwi_step)
  progress(value = i,max.value = 12)
}

#calculate the minimum each year
ndwi2015_min = app(x = ndwi2015,fun = min,na.rm=T)
ndwi2016_min = app(x = ndwi2016,fun = min,na.rm=T)
ndwi2017_min = app(x = ndwi2017,fun = min,na.rm=T)
ndwi2018_min = app(x = ndwi2018,fun = min,na.rm=T)
ndwi2019_min = app(x = ndwi2019,fun = min,na.rm=T)
ndwi2020_min = app(x = ndwi2020,fun = min,na.rm=T)
ndwi2021_min = app(x = ndwi2021,fun = min,na.rm=T)
ndwi2022_min = app(x = ndwi2022,fun = min,na.rm=T)
ndwi2023_min = app(x = ndwi2023,fun = min,na.rm=T)
ndwi2024_min = app(x = ndwi2024,fun = min,na.rm=T)

#remove values far outside of range
ndwi2015_min[ndwi2015_min > 1] = NA
ndwi2015_min[ndwi2015_min < -1] = NA

ndwi2016_min[ndwi2016_min > 1] = NA
ndwi2016_min[ndwi2016_min < -1] = NA

ndwi2017_min[ndwi2017_min > 1] = NA
ndwi2017_min[ndwi2017_min < -1] = NA

ndwi2018_min[ndwi2018_min > 1] = NA
ndwi2018_min[ndwi2018_min < -1] = NA

ndwi2019_min[ndwi2019_min > 1] = NA
ndwi2019_min[ndwi2019_min < -1] = NA

ndwi2020_min[ndwi2020_min > 1] = NA
ndwi2020_min[ndwi2020_min < -1] = NA

ndwi2021_min[ndwi2021_min > 1] = NA
ndwi2021_min[ndwi2021_min < -1] = NA

ndwi2022_min[ndwi2022_min > 1] = NA
ndwi2022_min[ndwi2022_min < -1] = NA

ndwi2023_min[ndwi2023_min > 1] = NA
ndwi2023_min[ndwi2023_min < -1] = NA

ndwi2024_min[ndwi2024_min > 1] = NA
ndwi2024_min[ndwi2024_min < -1] = NA

#calculate the average minimum over the years
ndwi

#subset to august for MIR all years
mirsaug2015 = mir2015[[8]]
mirsaug2016 = mir2016[[8]]
mirsaug2017 = mir2017[[8]]
mirsaug2018 = mir2018[[8]]
mirsaug2019 = mir2019[[8]]
mirsaug2020 = mir2020[[8]]
mirsaug2021 = mir2021[[8]]
mirsaug2022 = mir2022[[8]]
mirsaug2023 = mir2023[[8]]
mirsaug2024 = mir2024[[8]]








hist(mirsaug)
mirsaug[mirsaug > 0.5] = NA
plot(mirsaug)

modis = c(ndvimax,ndvisum,evimax,ndwimin,mirsaug)
names(modis) = c('ndvimax','ndvisum','evimax','ndwimin','mirsaug')

writeRaster(x = modis,filename = './data/input data/modis.tif',overwrite=T)
