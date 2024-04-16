rm(list = ls())

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
library(plyr)
library(data.table)
library(kit)
library(ggthemes)
library(plotrix)

base = rast("./summean_v5_base.tif")

#load in the stack created in the other file
clim = rast('./data/input data/climate_resample.tif')
sat  = rast('./data/input data/sat_data.tif')
soil = rast('./data/input data/soils.tif')

sat = resample(x = sat,y = base)
clim = resample(x = clim,y = base)
soil = resample(x = soil,y = base)

names(sat) = c('gdd','fdd','ndvisum','ndwimin','band7')

r = c(clim,sat,soil)
r = subset(x = r,subset = c(1:4,12:17,20:30))

r[is.na(base)] = NA

#convert the raster tp a data frame
df = as.data.frame(r,xy = T)

#filter out NAs
df2 = df[complete.cases(df),]

#take just the coordinates from column 1 and 2 (will need for transforming back into a raster)
cor = df2[,c(1,2)]

#remove the coordinates from the stack
df2 = df2[,-c(1,2)]

#re-scale all variables between 0 and 1 to ensure units are not the deciding factor
for (i in 1:length(df2)) {df2[,i] = plotrix::rescale(x = df2[,i],newrange = c(0,1))}

#could also run a PCA first to condense data.
pca = prcomp(x = df2,center = T,scale. = T)
pcadf = data.frame(pca$x[,c(1:4)])

#run a kmeans, determine what is the best by running several size clusters
set.seed(100) #set seed so the kmeans always starts in the same spot, makes the results more similar if ran again
km48 = kmeans(x = df2,centers = 48,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km24 = kmeans(x = df2,centers = 24,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km12 = kmeans(x = df2,centers = 12,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km6  = kmeans(x = df2,centers = 6,iter.max = 500,nstart = 10,algorithm = 'Lloyd')

#run a kmeans, using PCA
set.seed(100) #set seed so the kmeans always starts in the same spot, makes the results more similar if ran again
km48 = kmeans(x = pcadf,centers = 48,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km24 = kmeans(x = pcadf,centers = 24,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km12 = kmeans(x = pcadf,centers = 12,iter.max = 500,nstart = 10,algorithm = 'Lloyd')
km6  = kmeans(x = pcadf,centers = 6,iter.max = 500,nstart = 10,algorithm = 'Lloyd')

error  = c(km6$tot.withinss,km12$tot.withinss,km24$tot.withinss,km48$tot.withinss)
clusters = c(6,12,24,48)

plot(clusters,error,type='b')


#rasterize the clusters
cor$km48 = km48$cluster #add to the coordinates
cor$km24 = km24$cluster #add to the coordinates
cor$km12 = km12$cluster #add to the coordinates
cor$km6 = km6$cluster #add to the coordinates

kms = rasterFromXYZ(xyz = cor,crs = crs(r))

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

