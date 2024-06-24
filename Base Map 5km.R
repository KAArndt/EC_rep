#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

#library(raster)
#library(svMisc)
library(ggplot2)
library(ggspatial)
library(terra)
library(data.table)
library(kit)
library(sf)
#library(foreach)
#library(doParallel)
#library(doSNOW)
library(viridis)
library(readr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#load back in euclidean distance matrix
euci = read_rds('./data/euci_5km.rds')
#euci = as.matrix(euci)

#################################################################
#### first go to base image #####################################
#################################################################
pca.towers1 = tower.data
pca.towers1[,c('site','Activity')]

#find columns which are active sites
net = which(pca.towers1$Activity == 'active')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
#rm(euci)
gc() #clear unused memory


#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
for (i in 1:nrow(euci.net)) {
  base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])}

#create base image
#load in the stack created in the imagery preparation stack
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 5,fun = 'mean',cores = 10,na.rm = T) #aggregate to 5 km
df = as.data.frame(x = r,xy = T,na.rm = T)

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

#extract just the towers used in the base map
base.towers = tower.data[net,]

#basic plot
plot(base,range = c(0,4))
points(base.towers$x,base.towers$y)

#write the raster
writeRaster(x = base,filename = './5km/base_5km.tif')
