
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
# 2015 #################################################################################
ndwi_2015_01 = (nir2015[[1]] - mir2015[[1]])/(nir2015[[1]] + mir2015[[1]])
ndwi_2015_02 = (nir2015[[2]] - mir2015[[2]])/(nir2015[[2]] + mir2015[[2]])
ndwi_2015_03 = (nir2015[[3]] - mir2015[[3]])/(nir2015[[3]] + mir2015[[3]])
ndwi_2015_04 = (nir2015[[4]] - mir2015[[4]])/(nir2015[[4]] + mir2015[[4]])
ndwi_2015_05 = (nir2015[[5]] - mir2015[[5]])/(nir2015[[5]] + mir2015[[5]])
ndwi_2015_06 = (nir2015[[6]] - mir2015[[6]])/(nir2015[[6]] + mir2015[[6]])
ndwi_2015_07 = (nir2015[[7]] - mir2015[[7]])/(nir2015[[7]] + mir2015[[7]])
ndwi_2015_08 = (nir2015[[8]] - mir2015[[8]])/(nir2015[[8]] + mir2015[[8]])
ndwi_2015_09 = (nir2015[[9]] - mir2015[[9]])/(nir2015[[9]] + mir2015[[9]])
ndwi_2015_10 = (nir2015[[10]] - mir2015[[10]])/(nir2015[[10]] + mir2015[[10]])
ndwi_2015_11 = (nir2015[[11]] - mir2015[[11]])/(nir2015[[11]] + mir2015[[11]])
ndwi_2015_12 = (nir2015[[12]] - mir2015[[12]])/(nir2015[[12]] + mir2015[[12]])

#stack together in one file
ndwi_2015 = c(ndwi_2015_01,ndwi_2015_02,ndwi_2015_03,
              ndwi_2015_04,ndwi_2015_05,ndwi_2015_06,
              ndwi_2015_07,ndwi_2015_08,ndwi_2015_09,
              ndwi_2015_10,ndwi_2015_11,ndwi_2015_12)

#save
writeRaster(x = ndwi_2015,filename = 'D:/spatial_data/modis/ndwi/ndwi_2015.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2015_01','ndwi_2015_02','ndwi_2015_03','ndwi_2015_04',
     'ndwi_2015_05','ndwi_2015_06','ndwi_2015_07','ndwi_2015_08',
     'ndwi_2015_09','ndwi_2015_10','ndwi_2015_11','ndwi_2015_12','mir2015','nir2015','mir','nir')


# 2016 ################################################################################
ndwi_2016_01 = (nir2016[[1]] - mir2016[[1]])/(nir2016[[1]] + mir2016[[1]])
ndwi_2016_02 = (nir2016[[2]] - mir2016[[2]])/(nir2016[[2]] + mir2016[[2]])
ndwi_2016_03 = (nir2016[[3]] - mir2016[[3]])/(nir2016[[3]] + mir2016[[3]])
ndwi_2016_04 = (nir2016[[4]] - mir2016[[4]])/(nir2016[[4]] + mir2016[[4]])
ndwi_2016_05 = (nir2016[[5]] - mir2016[[5]])/(nir2016[[5]] + mir2016[[5]])
ndwi_2016_06 = (nir2016[[6]] - mir2016[[6]])/(nir2016[[6]] + mir2016[[6]])
ndwi_2016_07 = (nir2016[[7]] - mir2016[[7]])/(nir2016[[7]] + mir2016[[7]])
ndwi_2016_08 = (nir2016[[8]] - mir2016[[8]])/(nir2016[[8]] + mir2016[[8]])
ndwi_2016_09 = (nir2016[[9]] - mir2016[[9]])/(nir2016[[9]] + mir2016[[9]])
ndwi_2016_10 = (nir2016[[10]] - mir2016[[10]])/(nir2016[[10]] + mir2016[[10]])
ndwi_2016_11 = (nir2016[[11]] - mir2016[[11]])/(nir2016[[11]] + mir2016[[11]])
ndwi_2016_12 = (nir2016[[12]] - mir2016[[12]])/(nir2016[[12]] + mir2016[[12]])

#stack together in one file
ndwi_2016 = c(ndwi_2016_01,ndwi_2016_02,ndwi_2016_03,
              ndwi_2016_04,ndwi_2016_05,ndwi_2016_06,
              ndwi_2016_07,ndwi_2016_08,ndwi_2016_09,
              ndwi_2016_10,ndwi_2016_11,ndwi_2016_12)

#save
writeRaster(x = ndwi_2016,filename = 'D:/spatial_data/modis/ndwi/ndwi_2016.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2016_01','ndwi_2016_02','ndwi_2016_03','ndwi_2016_04',
     'ndwi_2016_05','ndwi_2016_06','ndwi_2016_07','ndwi_2016_08',
     'ndwi_2016_09','ndwi_2016_10','ndwi_2016_11','ndwi_2016_12','mir2016','nir2016')


# 2017 ################################################################################
ndwi_2017_01 = (nir2017[[1]] - mir2017[[1]])/(nir2017[[1]] + mir2017[[1]])
ndwi_2017_02 = (nir2017[[2]] - mir2017[[2]])/(nir2017[[2]] + mir2017[[2]])
ndwi_2017_03 = (nir2017[[3]] - mir2017[[3]])/(nir2017[[3]] + mir2017[[3]])
ndwi_2017_04 = (nir2017[[4]] - mir2017[[4]])/(nir2017[[4]] + mir2017[[4]])
ndwi_2017_05 = (nir2017[[5]] - mir2017[[5]])/(nir2017[[5]] + mir2017[[5]])
ndwi_2017_06 = (nir2017[[6]] - mir2017[[6]])/(nir2017[[6]] + mir2017[[6]])
ndwi_2017_07 = (nir2017[[7]] - mir2017[[7]])/(nir2017[[7]] + mir2017[[7]])
ndwi_2017_08 = (nir2017[[8]] - mir2017[[8]])/(nir2017[[8]] + mir2017[[8]])
ndwi_2017_09 = (nir2017[[9]] - mir2017[[9]])/(nir2017[[9]] + mir2017[[9]])
ndwi_2017_10 = (nir2017[[10]] - mir2017[[10]])/(nir2017[[10]] + mir2017[[10]])
ndwi_2017_11 = (nir2017[[11]] - mir2017[[11]])/(nir2017[[11]] + mir2017[[11]])
ndwi_2017_12 = (nir2017[[12]] - mir2017[[12]])/(nir2017[[12]] + mir2017[[12]])

#stack together in one file
ndwi_2017 = c(ndwi_2017_01,ndwi_2017_02,ndwi_2017_03,
              ndwi_2017_04,ndwi_2017_05,ndwi_2017_06,
              ndwi_2017_07,ndwi_2017_08,ndwi_2017_09,
              ndwi_2017_10,ndwi_2017_11,ndwi_2017_12)

#save
writeRaster(x = ndwi_2017,filename = 'D:/spatial_data/modis/ndwi/ndwi_2017.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2017_01','ndwi_2017_02','ndwi_2017_03','ndwi_2017_04',
     'ndwi_2017_05','ndwi_2017_06','ndwi_2017_07','ndwi_2017_08',
     'ndwi_2017_09','ndwi_2017_10','ndwi_2017_11','ndwi_2017_12','mir2017','nir2017')


# 2018 ################################################################################
ndwi_2018_01 = (nir2018[[1]] - mir2018[[1]])/(nir2018[[1]] + mir2018[[1]])
ndwi_2018_02 = (nir2018[[2]] - mir2018[[2]])/(nir2018[[2]] + mir2018[[2]])
ndwi_2018_03 = (nir2018[[3]] - mir2018[[3]])/(nir2018[[3]] + mir2018[[3]])
ndwi_2018_04 = (nir2018[[4]] - mir2018[[4]])/(nir2018[[4]] + mir2018[[4]])
ndwi_2018_05 = (nir2018[[5]] - mir2018[[5]])/(nir2018[[5]] + mir2018[[5]])
ndwi_2018_06 = (nir2018[[6]] - mir2018[[6]])/(nir2018[[6]] + mir2018[[6]])
ndwi_2018_07 = (nir2018[[7]] - mir2018[[7]])/(nir2018[[7]] + mir2018[[7]])
ndwi_2018_08 = (nir2018[[8]] - mir2018[[8]])/(nir2018[[8]] + mir2018[[8]])
ndwi_2018_09 = (nir2018[[9]] - mir2018[[9]])/(nir2018[[9]] + mir2018[[9]])
ndwi_2018_10 = (nir2018[[10]] - mir2018[[10]])/(nir2018[[10]] + mir2018[[10]])
ndwi_2018_11 = (nir2018[[11]] - mir2018[[11]])/(nir2018[[11]] + mir2018[[11]])
ndwi_2018_12 = (nir2018[[12]] - mir2018[[12]])/(nir2018[[12]] + mir2018[[12]])

#stack together in one file
ndwi_2018 = c(ndwi_2018_01,ndwi_2018_02,ndwi_2018_03,
              ndwi_2018_04,ndwi_2018_05,ndwi_2018_06,
              ndwi_2018_07,ndwi_2018_08,ndwi_2018_09,
              ndwi_2018_10,ndwi_2018_11,ndwi_2018_12)

#save
writeRaster(x = ndwi_2018,filename = 'D:/spatial_data/modis/ndwi/ndwi_2018.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2018_01','ndwi_2018_02','ndwi_2018_03','ndwi_2018_04',
   'ndwi_2018_05','ndwi_2018_06','ndwi_2018_07','ndwi_2018_08',
   'ndwi_2018_09','ndwi_2018_10','ndwi_2018_11','ndwi_2018_12','mir2018','nir2018')


# 2019 ################################################################################
ndwi_2019_01 = (nir2019[[1]] - mir2019[[1]])/(nir2019[[1]] + mir2019[[1]])
ndwi_2019_02 = (nir2019[[2]] - mir2019[[2]])/(nir2019[[2]] + mir2019[[2]])
ndwi_2019_03 = (nir2019[[3]] - mir2019[[3]])/(nir2019[[3]] + mir2019[[3]])
ndwi_2019_04 = (nir2019[[4]] - mir2019[[4]])/(nir2019[[4]] + mir2019[[4]])
ndwi_2019_05 = (nir2019[[5]] - mir2019[[5]])/(nir2019[[5]] + mir2019[[5]])
ndwi_2019_06 = (nir2019[[6]] - mir2019[[6]])/(nir2019[[6]] + mir2019[[6]])
ndwi_2019_07 = (nir2019[[7]] - mir2019[[7]])/(nir2019[[7]] + mir2019[[7]])
ndwi_2019_08 = (nir2019[[8]] - mir2019[[8]])/(nir2019[[8]] + mir2019[[8]])
ndwi_2019_09 = (nir2019[[9]] - mir2019[[9]])/(nir2019[[9]] + mir2019[[9]])
ndwi_2019_10 = (nir2019[[10]] - mir2019[[10]])/(nir2019[[10]] + mir2019[[10]])
ndwi_2019_11 = (nir2019[[11]] - mir2019[[11]])/(nir2019[[11]] + mir2019[[11]])
ndwi_2019_12 = (nir2019[[12]] - mir2019[[12]])/(nir2019[[12]] + mir2019[[12]])

#stack together in one file
ndwi_2019 = c(ndwi_2019_01,ndwi_2019_02,ndwi_2019_03,
              ndwi_2019_04,ndwi_2019_05,ndwi_2019_06,
              ndwi_2019_07,ndwi_2019_08,ndwi_2019_09,
              ndwi_2019_10,ndwi_2019_11,ndwi_2019_12)

#save
writeRaster(x = ndwi_2019,filename = 'D:/spatial_data/modis/ndwi/ndwi_2019.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2019_01','ndwi_2019_02','ndwi_2019_03','ndwi_2019_04',
   'ndwi_2019_05','ndwi_2019_06','ndwi_2019_07','ndwi_2019_08',
   'ndwi_2019_09','ndwi_2019_10','ndwi_2019_11','ndwi_2019_12','nir2019','mir2019')


# 2020 ################################################################################
ndwi_2020_01 = (nir2020[[1]] - mir2020[[1]])/(nir2020[[1]] + mir2020[[1]])
ndwi_2020_02 = (nir2020[[2]] - mir2020[[2]])/(nir2020[[2]] + mir2020[[2]])
ndwi_2020_03 = (nir2020[[3]] - mir2020[[3]])/(nir2020[[3]] + mir2020[[3]])
ndwi_2020_04 = (nir2020[[4]] - mir2020[[4]])/(nir2020[[4]] + mir2020[[4]])
ndwi_2020_05 = (nir2020[[5]] - mir2020[[5]])/(nir2020[[5]] + mir2020[[5]])
ndwi_2020_06 = (nir2020[[6]] - mir2020[[6]])/(nir2020[[6]] + mir2020[[6]])
ndwi_2020_07 = (nir2020[[7]] - mir2020[[7]])/(nir2020[[7]] + mir2020[[7]])
ndwi_2020_08 = (nir2020[[8]] - mir2020[[8]])/(nir2020[[8]] + mir2020[[8]])
ndwi_2020_09 = (nir2020[[9]] - mir2020[[9]])/(nir2020[[9]] + mir2020[[9]])
ndwi_2020_10 = (nir2020[[10]] - mir2020[[10]])/(nir2020[[10]] + mir2020[[10]])
ndwi_2020_11 = (nir2020[[11]] - mir2020[[11]])/(nir2020[[11]] + mir2020[[11]])
ndwi_2020_12 = (nir2020[[12]] - mir2020[[12]])/(nir2020[[12]] + mir2020[[12]])

#stack together in one file
ndwi_2020 = c(ndwi_2020_01,ndwi_2020_02,ndwi_2020_03,
              ndwi_2020_04,ndwi_2020_05,ndwi_2020_06,
              ndwi_2020_07,ndwi_2020_08,ndwi_2020_09,
              ndwi_2020_10,ndwi_2020_11,ndwi_2020_12)

#save
writeRaster(x = ndwi_2020,filename = 'D:/spatial_data/modis/ndwi/ndwi_2020.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2020_01','ndwi_2020_02','ndwi_2020_03','ndwi_2020_04',
   'ndwi_2020_05','ndwi_2020_06','ndwi_2020_07','ndwi_2020_08',
   'ndwi_2020_09','ndwi_2020_10','ndwi_2020_11','ndwi_2020_12','nir2020','mir2020')


# 2021 ################################################################################
ndwi_2021_01 = (nir2021[[1]] - mir2021[[1]])/(nir2021[[1]] + mir2021[[1]])
ndwi_2021_02 = (nir2021[[2]] - mir2021[[2]])/(nir2021[[2]] + mir2021[[2]])
ndwi_2021_03 = (nir2021[[3]] - mir2021[[3]])/(nir2021[[3]] + mir2021[[3]])
ndwi_2021_04 = (nir2021[[4]] - mir2021[[4]])/(nir2021[[4]] + mir2021[[4]])
ndwi_2021_05 = (nir2021[[5]] - mir2021[[5]])/(nir2021[[5]] + mir2021[[5]])
ndwi_2021_06 = (nir2021[[6]] - mir2021[[6]])/(nir2021[[6]] + mir2021[[6]])
ndwi_2021_07 = (nir2021[[7]] - mir2021[[7]])/(nir2021[[7]] + mir2021[[7]])
ndwi_2021_08 = (nir2021[[8]] - mir2021[[8]])/(nir2021[[8]] + mir2021[[8]])
ndwi_2021_09 = (nir2021[[9]] - mir2021[[9]])/(nir2021[[9]] + mir2021[[9]])
ndwi_2021_10 = (nir2021[[10]] - mir2021[[10]])/(nir2021[[10]] + mir2021[[10]])
ndwi_2021_11 = (nir2021[[11]] - mir2021[[11]])/(nir2021[[11]] + mir2021[[11]])
ndwi_2021_12 = (nir2021[[12]] - mir2021[[12]])/(nir2021[[12]] + mir2021[[12]])

#stack together in one file
ndwi_2021 = c(ndwi_2021_01,ndwi_2021_02,ndwi_2021_03,
              ndwi_2021_04,ndwi_2021_05,ndwi_2021_06,
              ndwi_2021_07,ndwi_2021_08,ndwi_2021_09,
              ndwi_2021_10,ndwi_2021_11,ndwi_2021_12)

#save
writeRaster(x = ndwi_2021,filename = 'D:/spatial_data/modis/ndwi/ndwi_2021.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2021_01','ndwi_2021_02','ndwi_2021_03','ndwi_2021_04',
   'ndwi_2021_05','ndwi_2021_06','ndwi_2021_07','ndwi_2021_08',
   'ndwi_2021_09','ndwi_2021_10','ndwi_2021_11','ndwi_2021_12','mir2021','nir2021')


# 2022 ################################################################################
ndwi_2022_01 = (nir2022[[1]] - mir2022[[1]])/(nir2022[[1]] + mir2022[[1]])
ndwi_2022_02 = (nir2022[[2]] - mir2022[[2]])/(nir2022[[2]] + mir2022[[2]])
ndwi_2022_03 = (nir2022[[3]] - mir2022[[3]])/(nir2022[[3]] + mir2022[[3]])
ndwi_2022_04 = (nir2022[[4]] - mir2022[[4]])/(nir2022[[4]] + mir2022[[4]])
ndwi_2022_05 = (nir2022[[5]] - mir2022[[5]])/(nir2022[[5]] + mir2022[[5]])
ndwi_2022_06 = (nir2022[[6]] - mir2022[[6]])/(nir2022[[6]] + mir2022[[6]])
ndwi_2022_07 = (nir2022[[7]] - mir2022[[7]])/(nir2022[[7]] + mir2022[[7]])
ndwi_2022_08 = (nir2022[[8]] - mir2022[[8]])/(nir2022[[8]] + mir2022[[8]])
ndwi_2022_09 = (nir2022[[9]] - mir2022[[9]])/(nir2022[[9]] + mir2022[[9]])
ndwi_2022_10 = (nir2022[[10]] - mir2022[[10]])/(nir2022[[10]] + mir2022[[10]])
ndwi_2022_11 = (nir2022[[11]] - mir2022[[11]])/(nir2022[[11]] + mir2022[[11]])
ndwi_2022_12 = (nir2022[[12]] - mir2022[[12]])/(nir2022[[12]] + mir2022[[12]])

#stack together in one file
ndwi_2022 = c(ndwi_2022_01,ndwi_2022_02,ndwi_2022_03,
              ndwi_2022_04,ndwi_2022_05,ndwi_2022_06,
              ndwi_2022_07,ndwi_2022_08,ndwi_2022_09,
              ndwi_2022_10,ndwi_2022_11,ndwi_2022_12)

#save
writeRaster(x = ndwi_2022,filename = 'D:/spatial_data/modis/ndwi/ndwi_2022.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2022_01','ndwi_2022_02','ndwi_2022_03','ndwi_2022_04',
   'ndwi_2022_05','ndwi_2022_06','ndwi_2022_07','ndwi_2022_08',
   'ndwi_2022_09','ndwi_2022_10','ndwi_2022_11','ndwi_2022_12','mir2022','nir2022')


# 2023 ################################################################################
ndwi_2023_01 = (nir2023[[1]] - mir2023[[1]])/(nir2023[[1]] + mir2023[[1]])
ndwi_2023_02 = (nir2023[[2]] - mir2023[[2]])/(nir2023[[2]] + mir2023[[2]])
ndwi_2023_03 = (nir2023[[3]] - mir2023[[3]])/(nir2023[[3]] + mir2023[[3]])
ndwi_2023_04 = (nir2023[[4]] - mir2023[[4]])/(nir2023[[4]] + mir2023[[4]])
ndwi_2023_05 = (nir2023[[5]] - mir2023[[5]])/(nir2023[[5]] + mir2023[[5]])
ndwi_2023_06 = (nir2023[[6]] - mir2023[[6]])/(nir2023[[6]] + mir2023[[6]])
ndwi_2023_07 = (nir2023[[7]] - mir2023[[7]])/(nir2023[[7]] + mir2023[[7]])
ndwi_2023_08 = (nir2023[[8]] - mir2023[[8]])/(nir2023[[8]] + mir2023[[8]])
ndwi_2023_09 = (nir2023[[9]] - mir2023[[9]])/(nir2023[[9]] + mir2023[[9]])
ndwi_2023_10 = (nir2023[[10]] - mir2023[[10]])/(nir2023[[10]] + mir2023[[10]])
ndwi_2023_11 = (nir2023[[11]] - mir2023[[11]])/(nir2023[[11]] + mir2023[[11]])
ndwi_2023_12 = (nir2023[[12]] - mir2023[[12]])/(nir2023[[12]] + mir2023[[12]])

#stack together in one file
ndwi_2023 = c(ndwi_2023_01,ndwi_2023_02,ndwi_2023_03,
              ndwi_2023_04,ndwi_2023_05,ndwi_2023_06,
              ndwi_2023_07,ndwi_2023_08,ndwi_2023_09,
              ndwi_2023_10,ndwi_2023_11,ndwi_2023_12)

#save
writeRaster(x = ndwi_2023,filename = 'D:/spatial_data/modis/ndwi/ndwi_2023.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2023_01','ndwi_2023_02','ndwi_2023_03','ndwi_2023_04',
   'ndwi_2023_05','ndwi_2023_06','ndwi_2023_07','ndwi_2023_08',
   'ndwi_2023_09','ndwi_2023_10','ndwi_2023_11','ndwi_2023_12','mir2023','nir2023')


# 2024 ################################################################################
ndwi_2024_01 = (nir2024[[1]] - mir2024[[1]])/(nir2024[[1]] + mir2024[[1]])
ndwi_2024_02 = (nir2024[[2]] - mir2024[[2]])/(nir2024[[2]] + mir2024[[2]])
ndwi_2024_03 = (nir2024[[3]] - mir2024[[3]])/(nir2024[[3]] + mir2024[[3]])
ndwi_2024_04 = (nir2024[[4]] - mir2024[[4]])/(nir2024[[4]] + mir2024[[4]])
ndwi_2024_05 = (nir2024[[5]] - mir2024[[5]])/(nir2024[[5]] + mir2024[[5]])
ndwi_2024_06 = (nir2024[[6]] - mir2024[[6]])/(nir2024[[6]] + mir2024[[6]])
ndwi_2024_07 = (nir2024[[7]] - mir2024[[7]])/(nir2024[[7]] + mir2024[[7]])
ndwi_2024_08 = (nir2024[[8]] - mir2024[[8]])/(nir2024[[8]] + mir2024[[8]])
ndwi_2024_09 = (nir2024[[9]] - mir2024[[9]])/(nir2024[[9]] + mir2024[[9]])
ndwi_2024_10 = (nir2024[[10]] - mir2024[[10]])/(nir2024[[10]] + mir2024[[10]])
ndwi_2024_11 = (nir2024[[11]] - mir2024[[11]])/(nir2024[[11]] + mir2024[[11]])
ndwi_2024_12 = (nir2024[[12]] - mir2024[[12]])/(nir2024[[12]] + mir2024[[12]])

#stack together in one file
ndwi_2024 = c(ndwi_2024_01,ndwi_2024_02,ndwi_2024_03,
              ndwi_2024_04,ndwi_2024_05,ndwi_2024_06,
              ndwi_2024_07,ndwi_2024_08,ndwi_2024_09,
              ndwi_2024_10,ndwi_2024_11,ndwi_2024_12)

#save
writeRaster(x = ndwi_2024,filename = 'D:/spatial_data/modis/ndwi/ndwi_2024.tif')

#remove previous monthly calcs to save memory
rm('ndwi_2024_01','ndwi_2024_02','ndwi_2024_03','ndwi_2024_04',
   'ndwi_2024_05','ndwi_2024_06','ndwi_2024_07','ndwi_2024_08',
   'ndwi_2024_09','ndwi_2024_10','ndwi_2024_11','ndwi_2024_12','mir2024','nir2024')

#re-load in any saved ones
ndwi_2015 = rast('D:/spatial_data/modis/ndwi/ndwi_2015.tif')
ndwi_2016 = rast('D:/spatial_data/modis/ndwi/ndwi_2016.tif')
ndwi_2017 = rast('D:/spatial_data/modis/ndwi/ndwi_2017.tif')
ndwi_2018 = rast('D:/spatial_data/modis/ndwi/ndwi_2018.tif')
ndwi_2019 = rast('D:/spatial_data/modis/ndwi/ndwi_2019.tif')
ndwi_2020 = rast('D:/spatial_data/modis/ndwi/ndwi_2020.tif')
ndwi_2021 = rast('D:/spatial_data/modis/ndwi/ndwi_2021.tif')
ndwi_2022 = rast('D:/spatial_data/modis/ndwi/ndwi_2022.tif')
ndwi_2023 = rast('D:/spatial_data/modis/ndwi/ndwi_2023.tif')
ndwi_2024 = rast('D:/spatial_data/modis/ndwi/ndwi_2024.tif')

#calculate the minimum each year
ndwi2015_min = app(x = ndwi_2015,fun = min,na.rm=T)
ndwi2016_min = app(x = ndwi_2016,fun = min,na.rm=T)
ndwi2017_min = app(x = ndwi_2017,fun = min,na.rm=T)
ndwi2018_min = app(x = ndwi_2018,fun = min,na.rm=T)
ndwi2019_min = app(x = ndwi_2019,fun = min,na.rm=T)
ndwi2020_min = app(x = ndwi_2020,fun = min,na.rm=T)
ndwi2021_min = app(x = ndwi_2021,fun = min,na.rm=T)
ndwi2022_min = app(x = ndwi_2022,fun = min,na.rm=T)
ndwi2023_min = app(x = ndwi_2023,fun = min,na.rm=T)
ndwi2024_min = app(x = ndwi_2024,fun = min,na.rm=T)

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

#stack all together
ndwis = c(ndwi2015_min,ndwi2016_min,ndwi2017_min,ndwi2018_min,ndwi2019_min,
          ndwi2020_min,ndwi2021_min,ndwi2022_min,ndwi2023_min,ndwi2024_min)

#calculate the average minimum over the years
ndwi_min_mean = app(x = ndwis,fun = mean,na.rm=T)


writeRaster(x = ndwi_min_mean,filename = './spatial_data/ndwi_min_10yrmean.tif',overwrite=T)
