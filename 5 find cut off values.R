rm(list = ls())

library(data.table)
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(seegSDM)
library(plyr)

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towers.csv')

#set just the coordinates for the extract
xy.tower = tower.data[,c(32,33)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')
#reproject to lat lon so it works right

clust = clust$km20
plot(clust)

#extract data
clustdat = extract(x = clust,y = xy.tower,cells=T,xy=T)
nas = clustdat[is.na(clustdat$km20),] #extract where nas
clustr = stack(clust) #make a raster version
na.cor
#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c(4,5)],raster = clustr,max_distance = 100000000))

#place in original data frame
clustdat[nas$ID,] = extract(x = clust,y = na.cor,cells=T,xy=T)
clustdat$site = tower.data$site
tower.data$cluster = clustdat$km20

active = subset(tower.data,tower.data$Activity == 'active')
active$CH4 = ifelse(active$CH4=='','no',active$CH4)
active$status = paste(active$CH4,active$Annual_cover,sep = '_')

ggplot(data = active)+theme_bw()+
  geom_bar(aes(cluster,fill = status))+
  scale_y_continuous(expand = c(0,0),limits = c(0,30),'Number of Tower Sites')+
  scale_x_continuous(expand = c(0,0),"Cluster")

active$one = 1

dfs = active %>%
  group_by(cluster) %>%
  summarise(count = sum(one))

dfs

#load in base image
base = rast('./output/base_2km.tif')

all = c(base,clust)

all$base.filt1 = all$base.dist
all$base.filt4 = all$base.dist

all$base.filt1[all$km20 == 3 | all$km20 == 7] = NA
all$base.filt4[all$km20 == 3 | all$km20 == 7 | all$km20 == 12 | all$km20 == 16 | all$km20 == 19] = NA

plot(all$base.filt1)
plot(all$base.filt4)

hist(all$base.filt1)
hist(all$base.filt4)

summary(all$base.filt1)
summary(all$base.filt4)

#the final cut offs are 1.77 and 1.71 for ER1 and ER4

