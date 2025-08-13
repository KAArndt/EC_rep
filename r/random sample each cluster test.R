
library(data.table)
library(raster)
library(svMisc)
library(ggplot2)
library(terra)
library(seegSDM)
library(dplyr)

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towers.base.csv')
active = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#set just the coordinates for the extract
xy.tower = active[,c(58,59)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')

clust = clust$km40
plot(clust)
names(clust) = 'cluster'


r = rast('./spatial_data/pca_2km.tif')

r = crop(x = r,y = ext(clust))
spat = c(clust,r)
plot(spat)

spat.df = as.data.frame(x = spat,xy=T)

library(foreach)
library(doParallel)
library(doSNOW)


samp1 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp1 = samp1[!duplicated(samp1$cluster),]
samp1 = samp1[complete.cases(samp1$cluster),]

#setup parallel back end to use many processors
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp1),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp1$PC1[j])^2 +
                         (spat.df$PC2[i]-samp1$PC2[j])^2 +
                         (spat.df$PC3[i]-samp1$PC3[j])^2 +
                         (spat.df$PC4[i]-samp1$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock


#calculate the base network, parallel processing is much slower here
library(kit)
base.dist = numeric(length = nrow(euci))
num = 2
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

hist(base)
summary(base)
plot(base,range=c(0,3))
points(samp1$x,samp1$y,col='red',pch=16)

####################################################################
samp2 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp2 = samp2[!duplicated(samp2$cluster),]

#setup parallel back end to use many processors
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp1),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp2$PC1[j])^2 +
                         (spat.df$PC2[i]-samp2$PC2[j])^2 +
                         (spat.df$PC3[i]-samp2$PC3[j])^2 +
                         (spat.df$PC4[i]-samp2$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock


#calculate the base network, parallel processing is much slower here
library(kit)
base.dist = numeric(length = nrow(euci))
num = 2
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
base2 = rast(x = basedf,type = 'xyz',crs = crs(r))

hist(base2)
summary(base2)
plot(base2,range=c(0,3))
points(samp2$x,samp2$y,col='red',pch=16)



samp3 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp3 = samp3[!duplicated(samp3$cluster),]

#setup parallel back end to use many processors
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp1),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp3$PC1[j])^2 +
                         (spat.df$PC2[i]-samp3$PC2[j])^2 +
                         (spat.df$PC3[i]-samp3$PC3[j])^2 +
                         (spat.df$PC4[i]-samp3$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock


#calculate the base network, parallel processing is much slower here
library(kit)
base.dist = numeric(length = nrow(euci))
num = 2
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
base3 = rast(x = basedf,type = 'xyz',crs = crs(r))

hist(base3)
summary(base3)
plot(base3,range=c(0,3))
points(samp3$x,samp3$y,col='red',pch=16)

plot(clust,range=c(30,40))
points(samp3$x,samp3$y,col='red',pch=16)



samp4 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp4 = samp4[!duplicated(samp4$cluster),]

samp5 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp5 = samp5[!duplicated(samp5$cluster),]





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

#load in base image
base = rast('./output/base_network/base_2km.tif')
all = c(base,clust)
alldf = as.data.frame(x = all)

active$one = 1

dfs = active %>%
  group_by(cluster) %>%
  summarise(count = sum(one))


dfs1 = subset(dfs,dfs$count == 1)
er1 = merge(dfs1,alldf,by = 'cluster',all = T)
er1 = er1[complete.cases(er1$count),]

summary(er1$base.dist)
median(er1$base.dist,na.rm=T)
sd(er1$base.dist,na.rm=T)
1.53+0.53*2
summary(er1$base.dist)[5]
hist(er1$base.dist)

dfs4 = subset(dfs,dfs$count == 4)
er4 = merge(dfs4,alldf,by = 'cluster',all = T)
er4 = er4[complete.cases(er4$count),]

summary(er4$base.dist)[5]
hist(er4$base.dist)

#mean
#the final cut offs are 1.96 and 1.56 for ER1 and ER4 2 mean
