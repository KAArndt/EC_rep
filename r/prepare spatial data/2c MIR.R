
library(terra)
library(svMisc)

############################################################
#Data from APPEARS (MODIS)
############################################################

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


#compare a few months of data for each
mirs_jul = c(mir2015[[7]]
,mir2016[[7]]
,mir2017[[7]]
,mir2018[[7]]
,mir2019[[7]]
,mir2020[[7]]
,mir2021[[7]]
,mir2022[[7]]
,mir2023[[7]]
,mir2024[[7]])

mirs_aug = c(mir2015[[8]]
              ,mir2016[[8]]
              ,mir2017[[8]]
              ,mir2018[[8]]
              ,mir2019[[8]]
              ,mir2020[[8]]
              ,mir2021[[8]]
              ,mir2022[[8]]
              ,mir2023[[8]]
              ,mir2024[[8]])

mirs_sep = c(mir2015[[9]]
              ,mir2016[[9]]
              ,mir2017[[9]]
              ,mir2018[[9]]
              ,mir2019[[9]]
              ,mir2020[[9]]
              ,mir2021[[9]]
              ,mir2022[[9]]
              ,mir2023[[9]]
              ,mir2024[[9]])

#calcualte annual averages
mir_jul = app(x = mirs_jul,fun=mean,na.rm=T)
mir_aug = app(x = mirs_aug,fun=mean,na.rm=T)
mir_sep = app(x = mirs_sep,fun=mean,na.rm=T)


hist(mir_jul)
hist(mir_aug)
hist(mir_sep)

plot(mir_jul,range=c(0,0.4))
plot(mir_aug,range=c(0,0.4))
plot(mir_sep,range=c(0,0.4))

hist(mir_aug)
summary(mir_aug)
#mirsaug[mir_aug > 0.5] = NA
plot(mir_aug)

writeRaster(x = mir_aug,filename = './spatial_data/mir_aug_10yrmean.tif')
