#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
gc()

library(svMisc)
library(maps)
library(ggplot2)
library(ggspatial)
library(terra)
#library(kit)
library(sf)
library(viridis)
library(data.table)
library(readr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towersv2.csv')
tower.data = subset(tower.data,tower.data$remove == 'no' | is.na(tower.data$remove))
pca.towers = tower.data

#load back in euclidean distance matrix
euci = read_rds('./data/euci_2kmv2.rds')

#load in the other spatial data
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
df = as.data.frame(x = r,xy = T,na.rm = T)

#########################################################################################
# BASE ###################################################################
net = which(pca.towers$active == 'active' & pca.towers$Start_CO2 < 2022)
euci.net = euci[,c(net)]

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
for (i in 1:nrow(euci.net)) {
  base.dist[i] = min(euci.net[i,])
  }
Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(df$x,df$y,base.dist)

base = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
base.towers = tower.data[net,]
towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))

hist(base)
plot(base,range=c(0,4.5))
points(towers,col='red')

#save the base here
writeRaster(x = base,filename = './output/base_2kmv2_min.tif',overwrite = T)

#######################################################################################
##################     METHANE

net.methane = which(pca.towers$Start_CO2 < 2022 & pca.towers$active == 'active' & pca.towers$methane == 'methane')
pca.towers$site[net.methane]

euci.net.methane = euci[,c(net.methane)]

#calculate the base network, parallel processing is much slower here
methane.dist = numeric(length = nrow(euci.net.methane))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.methane)) {
    methane.dist[i] = min(euci.net.methane[i,])
  }
  Sys.time() - orig} #stop the clock

#make the base image
methanedf = data.frame(df$x,df$y,methane.dist)

methane = rast(x = methanedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
methane.towers = tower.data[net.methane,]
towers = vect(x = methane.towers,geom=c("x", "y"), crs=crs(r))

hist(methane)
plot(methane,range=c(0,3.5))
points(towers,col='red')

#save the base here
writeRaster(x = methane,filename = './output/methane_2kmv2_min.tif',overwrite = T)

##########################################################################################
################ Annual

net.annual = which(pca.towers$active == 'active' & pca.towers$Start_CO2 < 2022 & pca.towers$Season_Activity == 'All year')

euci.net.annual = euci[,c(net.annual)]

#calculate the base network, parallel processing is much slower here
annual.dist = numeric(length = nrow(euci.net.annual))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.annual)) {
    annual.dist[i] = min(euci.net.annual[i,])
  }
  Sys.time() - orig} #stop the clock

#make the base image
annualdf = data.frame(df$x,df$y,annual.dist)

annual = rast(x = annualdf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers dataannual
annual.towers = tower.data[net.annual,]
towers = vect(x = annual.towers,geom=c("x", "y"), crs=crs(r))

hist(annual)
plot(annual,range=c(0,3.5))
points(towers,col='red')

#save the annual here
writeRaster(x = annual,filename = './output/annual_2kmv2_min.tif',overwrite = T)

###################################################################################################
################ Annual Methane

net.annual.methane = which(pca.towers$active == 'active' & pca.towers$Start_CO2 < 2022 & pca.towers$Season_Activity == 'All year' & pca.towers$methane == 'methane')

euci.net.annual.methane = euci[,c(net.annual.methane)]

#calculate the base network, parallel processing is much slower here
annual.methane.dist = numeric(length = nrow(euci.net.annual.methane))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.annual.methane)) {
    annual.methane.dist[i] = min(euci.net.annual.methane[i,])
  }
  Sys.time() - orig} #stop the clock

#make the base image
annual.methane.df = data.frame(df$x,df$y,annual.methane.dist)

annual.methane = rast(x = annual.methane.df,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
annual.methane.towers = tower.data[net.annual.methane,]
towers = vect(x = annual.methane.towers,geom=c("x", "y"), crs=crs(r))

hist(annual.methane)
plot(annual.methane,range=c(0,3.5))
points(towers,col='red')

#save the base here
writeRaster(x = annual.methane,filename = './output/annual_methane_2kmv2_min.tif',overwrite = T)

