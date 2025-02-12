
library(terra)
library(svMisc)
library(sf)

#load in all the nee files
files = list.files(path = './spatial_data/nee/',pattern = '*.tif',all.files = T,full.names = T)
rasters = lapply(files, rast)

#load in spatial shape data
states = readRDS(file = './spatial_data/states.rds') #load in states sf file
states = st_transform(x = states,crs = crs(rasters[[1]]))
can    = subset(states,states$admin == 'Canada')
nwt    = subset(states,states$name == 'Northwest Territories')

#cut to Canadian and NWT extents #######################################
nee.can = lapply(X = rasters,FUN = crop,y = ext(can))
nee.can = lapply(X = nee.can,FUN = mask,mask=can)

nee.nwt = lapply(X = rasters,FUN = crop,ext(nwt))
nee.nwt = lapply(X = nee.nwt,FUN = mask,mask=nwt)

#make data frames of all the images
df.can = lapply(nee.can,as.data.frame)
df.nwt = lapply(nee.nwt,as.data.frame)

#annual sums for canada
annual.sum.can = vector(length = length(df.can))
for (i in 1:length(df.can)) {
  annual.sum.can[i] = sum(df.can[[i]])*(999.8591^2)/(10^12)
}

#annual sums for nwt
annual.sum.nwt = vector(length = length(df.nwt))
for (i in 1:length(df.nwt)) {
  annual.sum.nwt[i] = sum(df.nwt[[i]])*(999.8591^2)/(10^12)
}

#calculate mean and sd of values
mean(annual.sum.can)
mean(annual.sum.nwt)

sd(annual.sum.can)
sd(annual.sum.nwt)

