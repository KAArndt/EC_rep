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
library(dplyr)

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towersv2.csv')
active = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#set just the coordinates for the extract
xy.tower = active[,c(56,57)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')

clust = clust$km40
plot(clust)
names(clust) = 'cluster'

#extract data
clustdat = extract(x = clust,y = xy.tower,cells=T,xy=T)
nas = clustdat[is.na(clustdat$cluster),] #extract where nas
clustr = stack(clust) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c(4,5)],raster = clustr,max_distance = 1000000))
summary(na.cor)

#place in original data frame
clustdat[nas$ID,] = extract(x = clust,y = na.cor,cells=T,xy=T)
clustdat$site = active$site
active$cluster = clustdat$cluster

active$status = paste(active$methane,active$Season_Activity,sep = '_')

ggplot(data = active)+theme_bw()+
  geom_bar(aes(cluster,fill = status))+
  scale_y_continuous(expand = c(0,0),limits = c(0,20),'Number of Tower Sites')+
  scale_x_continuous(expand = c(0,0),"Cluster")

active$one = 1

dfs = active %>%
  group_by(cluster) %>%
  summarise(count = sum(one))
dfs

#load in base image
#base = rast('./output/base_2kmv2.tif')
base = rast('./output/base_2kmv2_min.tif')
all = c(base,clust)
alldf = as.data.frame(x = all)

dfs4 = subset(dfs,dfs$count >= 4)

er1 = merge(dfs,alldf,by = 'cluster',all = T)
er1 = er1[complete.cases(er1$count),]

summary(er1$base.dist)[5]
hist(er1$base.dist)

er4 = merge(dfs4,alldf,by = 'cluster',all = T)
er4 = er4[complete.cases(er4$count),]

summary(er4$base.dist)[5]
hist(er4$base.dist)

#mean
#the final cut offs are 1.xx and 1.xx for ER1 and ER4 2 mean WOULD NEED TO RE-DO

#minimum
#the final cut offs are 1.54 and 1.43 for ER1 and ER4 minimum
