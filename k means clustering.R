rm(list = ls())

#library(raster)
#library(svMisc)
#library(MASS)
#library(ggplot2)
#library(ggspatial)
library(terra)
#library(plyr)
#library(data.table)
#library(kit)
#library(ggthemes)
#library(plotrix)
library(foreach)
library(doParallel)
library(doSNOW)

#load in the pca image
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=12,na.rm=T)
df = as.data.frame(x = r,na.rm = T,xy = T)

#take just the coordinates from column 1 and 2 (will need for transforming back into a raster)
cor = df[,c(1,2)]

#remove the coordinates from the stack
df = df[,-c(1,2)]

#re-scale all variables between 0 and 1 to ensure units are not the deciding factor
#for (i in 1:length(df2)) {df2[,i] = plotrix::rescale(x = df2[,i],newrange = c(0,1))}

#run kmeans clustering in parallel, determine what is the best by running several size clusters
set.seed(100) #set seed so the kmeans always starts in the same spot, makes the results more similar if ran again
cents = c(2,3) #set how many clusters we want in the different iterations

#setup parallel back end to use many processors
  cores = detectCores() #detect the number of cores
  cl = makeCluster(2) #set the clusters to the number of kmeans to be run, or 1 less than total cores
  {orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the kmeans calculations in parallel
  km = foreach (i = cents,.verbose = T) %dopar% {
    kmeans(x = df,centers = i,iter.max = 500,nstart = 10,algorithm = 'Lloyd')}
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

km[[1]]$tot.withinss

#error calculations for deciding appropriate amount of clusters
error  = c(km[[1]]$tot.withinss,km[[2]]$tot.withinss)


plot(cents,error,type='b')


#rasterize the clusters
cor$km2 = km[[1]]$cluster #add to the coordinates
cor$km3 = km[[2]]$cluster #add to the coordinates
cor$km12 = km12$cluster #add to the coordinates
cor$km6 = km6$cluster #add to the coordinates


kms = rast(x = cor,type = 'xyz',crs = crs(r))


plot(kms)

#project to the rest of the images
kms = projectRaster(from = kms,to = r)

#save off so it can just be reloaded again
writeRaster(x = kms,filename = 'C:/site.selection/euc dist/kms.tif',overwrite = T)

kms = stack(x = 'C:/site.selection/euc dist/kms.tif') 

#add to the datastack
r2 = stack(r,kms)


#just for plotting of k means ##############################################################
#steridean projection
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

#project to the steridean projection
r2 = projectRaster(from = kms,crs = proj)

library(cowplot)
library(viridis)

ggplot()+theme_map()+
  layer_spatial(r2$km6)+
  scale_fill_viridis_c(na.value = NA,'cluster')+
  scale_x_continuous(limits = c(-4580235,4580235))+
  scale_y_continuous(limits = c(-4580235,4580235))

