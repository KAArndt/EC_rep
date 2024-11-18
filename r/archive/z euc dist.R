#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
#setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(plyr)
library(data.table)
library(kit)
library(ggthemes)
library(sf)

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = rast('./pca.tif')

#r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
r

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#create data frame from PCAs
df = as.data.frame(x = r,xy = T,na.rm = T)
xy = df[,c(1,2)] #extract just the coordinates
df = df[,-c(1,2,7)] #extract just the top 4 PCs

#################################################################################
#calculate the euclidean distance for the whole data set 
#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(df)
pca.towers = data.table(tower.data[,c('pc1','pc2','pc3','pc4')])

#pre-populating the whole matrix makes computation time much faster *DO NOT USE A DATAFRAME
rm(r)
rm(df)
gc() #free unused memory

library(foreach)
library(doParallel)
library(doSNOW)

#initialize the Euclid
euclid = vector(length = nrow(pca.dt))

#setup parallel back-end to use many processors
{orig = Sys.time() #start the clock for timing the process
cores = detectCores()        #detect the number of cores
#cl = makeCluster(cores[1]-1) #assign X less than total cores to leave some processing for other tasks
cl = makeCluster(cores[1])    #use all clusters on VM
registerDoSNOW(cl) #register the cores

#run the ED calculations in parallel
euci = foreach (j = 1:nrow(pca.towers),.verbose = T,.combine = cbind) %dopar% {
   for (i in 1:nrow(pca.dt))  {
      euclid[i] = sqrt((pca.dt$PC1[i]-pca.towers$pc1[j])^2 +
                       (pca.dt$PC2[i]-pca.towers$pc2[j])^2 +
                       (pca.dt$PC3[i]-pca.towers$pc3[j])^2 +
                       (pca.dt$PC4[i]-pca.towers$pc4[j])^2)}
  euclid} #report out the loops above to be included
stopCluster(cl) #stop the clusters
Sys.time() - orig} #stop the clock

#save the euclidean distance
#colnames(euci) = tower.data$site
#fwrite(x = euci,file = './euci_new.csv',row.names = F)


######################################################################

#load back in
euci = fread('./euci_new.csv')
euci = as.matrix(euci)

#################################################################
#### first go to base image #####################################
#################################################################
pca.towers1 = tower.data
pca.towers1[,c('site','Activity')]

#find columns which are active sites
net = which(pca.towers1$Activity == 'active')
ext = which(pca.towers1$Activity != 'active' | is.na(pca.towers1$Activity))

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
wch  = matrix(nrow = nrow(df),ncol = num)
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#calculate the euc distance of the network and one extension site
orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
for (i in 1:nrow(df)) {
  dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
 # dist[i] = min(temp.euci[i,])
}
eucis[,j] = dist
progress(j,ncol(euci.ext))
}
Sys.time() - orig

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
  for (i in 1:nrow(df)) {
    base.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
  #  base.dist[i]    = min(euci.net[i,])
  }

#create base image
base = rasterFromXYZ(xyz = cbind(df$x,df$y,base.dist),crs = crs(r))

base.towers = SpatialPointsDataFrame(coords = pca.towers[net,c(1,2)],data = pca.towers[net,],proj4string = crs(base))
base.towers = spTransform(x = base.towers,CRSobj = proj)
base.towers.st = as.data.frame(base.towers)

ex.towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
ex.towers = spTransform(x = ex.towers,CRSobj = proj)
ex.towers.st = as.data.frame(ex.towers)

#create the base
base.ster = projectRaster(from = base,crs = proj)

#load in base map
#things needed for all the plots
library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

#background world map for plotting
map = map_data('world')
wm = SpatialPointsDataFrame(coords = map[,c(1,2)],data = map,proj4string = crs(r))
plot(wm)
wm = spTransform(x = wm,CRSobj = proj)
plot(wm)
sf_use_s2(FALSE)
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 45, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))


ex.towers.st = subset(ex.towers.st,ex.towers.st$sitename == "Quebec 2" | ex.towers.st$sitename == "CEF cluster")

png(filename = './output/feb/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ster)+
  geom_point(data = ex.towers.st,aes(coords.x1,coords.x2),col='black',fill='white',pch=25,size=2)+
  geom_point(data = base.towers.st,aes(coords.x1,coords.x2),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))
dev.off()

#save the base here
writeRaster(x = base,filename = './output/feb/base_all_towers.tif',overwrite = T)
base = rast('./output/aug/active/base_all_towers.tif')

#now create rasters out of the euclidean distances calculated for extension sites
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rasterFromXYZ(xyz = tempdf,crs = crs(r))
  progress(i,ncol(eucis))
}

#create a path of file names
path = paste('./output/feb/ext/',pca.towers$sitename[ext],'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}

#####################################################################################
#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/feb/difs/',pca.towers$sitename[ext],'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = cellStats(x = difs[[i]],stat = mean)
}

#add other parts of the dataframe back in
bars = data.frame(pca.towers$sitename[ext])
bars$means = means
bars$country = pca.towers$country[ext]

pca.towers$type = paste(pca.towers$active,pca.towers$ch4,pca.towers$annual,sep = '_')
bars$type = pca.towers$type[ext]
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

png(filename = './output/feb/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle('Top 50 Percentile Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()



write.csv(x = bars,file = './output/feb/meanreduction.csv',row.names = F)

#decide which sites to leave out of all future analyses.
topnums = which(bars$means < median(bars$means))
difs.top = difs[[topnums[1]]]


plot(difs.top)

#site level improvement plots
#reproject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#plot
path = paste('./output/feb/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 500)
  plots =   ggplot()+theme_bw()+ggtitle(pca.towers$sitename[ext][i])+
    geom_sf(data = countries,fill='gray',col='gray40')+
    layer_spatial(dif.ster[[i]])+
    geom_point(data = towers.st,aes(coords.x1[i],coords.x2[i]),col='black',fill='white',pch=25)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-.5,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
    theme(text = element_text(size = 8),
          legend.text = element_text(size = 8),
          title = element_text(size = 10),
          axis.title = element_text(size = 8),
          legend.key.width = unit(x = 0.1,units = 'in'))
  print(plots)
  dev.off()
  progress(i,length(dif.ster))
}


#Quebec #1
ggplot()+theme_bw()+ggtitle(pca.towers$sitename[ext][216])+
    geom_sf(data = countries,fill='gray',col='gray40')+
    layer_spatial(dif.ster[[216]])+
    geom_point(data = towers.st,aes(coords.x1[216],coords.x2[216]),col='black',fill='white',pch=25,cex=3)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-1,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
    theme(text = element_text(size = 18),
          legend.text = element_text(size = 18),
          title = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.key.width = unit(x = 0.25,units = 'in'))

ggplot()+theme_bw()+ggtitle('LG 4')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.ster[[92]])+
  geom_point(data = towers.st,aes(coords.x1[92],coords.x2[92]),col='black',fill='white',pch=25,cex=3)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(-1,0),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.key.width = unit(x = 0.25,units = 'in'))

cellStats(dif.ster[[216]],stat = 'sum')
cellStats(dif.ster[[92]],stat = 'sum')

cellStats(dif.ster[[216]],stat = 'mean')
cellStats(dif.ster[[92]],stat = 'mean')

5959.285/7616.686

#project the layers of interest to 
lg4 = projectRaster(dist.rasts[[92]],crs = proj)
qu2 = projectRaster(dist.rasts[[216]],crs = proj)

ggplot()+theme_bw()+ggtitle('Base')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ster)+
  geom_point(data = base.towers.st,aes(coords.x1,coords.x2),col='black',fill='green',pch=25)+
 # geom_point(data = towers.st,aes(coords.x1[92],coords.x2[92]),col='black',fill='white',pch=25)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(0,3),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))

ggplot()+theme_bw()+ggtitle('LG 4')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(lg4)+
  geom_point(data = base.towers.st,aes(coords.x1,coords.x2),col='black',fill='green',pch=25,cex=3)+
  geom_point(data = towers.st,aes(coords.x1[92],coords.x2[92]),col='black',fill='white',pch=25,cex=3)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(0,3),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.key.width = unit(x = 0.25,units = 'in'))

ggplot()+theme_bw()+ggtitle('Quebec 2')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(qu2)+
  geom_point(data = base.towers.st,aes(coords.x1,coords.x2),col='black',fill='green',pch=25,cex=3)+
  geom_point(data = towers.st,aes(coords.x1[216],coords.x2[216]),col='black',fill='white',pch=25,cex=3)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(0,3),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        title = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.key.width = unit(x = 0.25,units = 'in'))

####################################################################################
#AFTER ADDITION OF NEW SITES #########################################################
####################################################################################

pca.towers2 = pca.towers1
pca.towers2[,c('sitename','active')]

pca.towers2$active = ifelse(
  pca.towers2$sitename == 'Churchill Fen' |
    pca.towers2$sitename == 'Iqaluit' |
    pca.towers2$sitename == 'Scotty Creek Landscape' |
    pca.towers2$sitename == 'Resolute' |
    pca.towers2$sitename == 'Pond Inlet' |
    pca.towers2$sitename == 'Kangiqsualujjuaq' |
    pca.towers2$sitename == 'CEF cluster' ,
    'active',pca.towers2$active)
pca.towers2$sitename

#find columns which are active sites
net = which(pca.towers2$active == 'active')
ext = which(pca.towers2$active != 'active' | is.na(pca.towers2$active))

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
wch  = matrix(nrow = nrow(df),ncol = num)
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#calculate the euc distance of the network and one extension site
orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
  for (i in 1:nrow(df)) {
    dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
    
  }
  eucis[,j] = dist
  progress(j,ncol(euci.ext))
}
Sys.time() - orig

#calculate the base network
imp.dist = numeric(length = nrow(euci.net))
orig = Sys.time()
for (i in 1:nrow(df)) {
  imp.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
}
Sys.time() - orig

#create base image
imp = rasterFromXYZ(xyz = cbind(df$x,df$y,imp.dist),crs = crs(r))

towers = SpatialPointsDataFrame(coords = pca.towers[net,c(1,2)],data = pca.towers[net,],proj4string = crs(imp))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

imp.ster = projectRaster(from = imp,crs = proj)
pal = viridis(n = 8,direction = -1,option = 'A')

png(filename = './output/aug/active/imp.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('Added Sites')+
  geom_polygon(data = wmd,aes(x = x,y=y,group=group),fill = 'gray')+
  layer_spatial(imp.ster$imp.dist)+
  geom_point(data = towers.st,aes(coords.x1,coords.x2),col='black',fill='green',pch=25,size=2)+
  geom_point(data = added,aes(coords.x1,coords.x2),col='black',fill='cyan',pch=25,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
theme(text = element_text(size = 8),
      legend.text = element_text(size = 8),
      title = element_text(size = 10),
      axis.title = element_text(size = 8),
      legend.key.width = unit(x = 0.1,units = 'in'))
dev.off()


plot(imp.ster$imp.dist - base.ster$base_all_towers)

#save the imp here
writeRaster(x = imp,filename = './output/aug/active/imp_2_towers.tif',overwrite = T)

pal = viridis(n = 8,direction = 1,option = 'A')
added = subset(towers.st,towers.st$sitename == 'Churchill Fen' |
                 towers.st$sitename == 'Iqaluit' |
                 towers.st$sitename == 'Scotty Creek Landscape' |
                 towers.st$sitename == 'Resolute' |
                 towers.st$sitename == 'Pond Inlet' |
                 towers.st$sitename == 'Kangiqsualujjuaq' |
                  towers.st$sitename == 'CEF cluster')

png(filename = './output/aug/active/improve.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('Added Sites')+
  geom_polygon(data = wmd,aes(x = x,y=y,group=group),fill = 'gray')+
  layer_spatial(base.ster$base_all_towers - imp.ster$imp.dist)+
  geom_point(data = added,aes(coords.x1,coords.x2),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,1),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))
dev.off()



#now create rasters out of the euclidean distances calculated for extension sites
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rasterFromXYZ(xyz = tempdf,crs = crs(r))
  progress(i,ncol(eucis))
}

#create a path of file names
path = paste('./output/aug/ch4/ext/',pca.towers$sitename[ext],'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}

#####################################################################################
#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/aug/ch4/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
}

#reproject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#plot
path = paste('./output/aug/ch4/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 200)
  plots =   ggplot()+theme_bw()+ggtitle(pca.towers$sitename[ext][i])+
    geom_polygon(data = wm,aes(x = long,y=lat,group=group),
                 fill = 'gray80',lwd = 0.25)+
    layer_spatial(dif.ster[[i]])+
    geom_point(data = towers.st,aes(x.1[i],y.1[i]),col='black',fill='white',pch=25)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-.3,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
    theme(text = element_text(size = 8),
          legend.text = element_text(size = 8),
          title = element_text(size = 10),
          axis.title = element_text(size = 8),
          legend.key.width = unit(x = 0.1,units = 'in'))
  print(plots)
  dev.off()
  progress(i,length(dif.ster))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = cellStats(x = difs[[i]],stat = mean)
}

#add other parts of the dataframe back in
bars = data.frame(pca.towers$sitename[ext])
bars$means = means
bars$country = pca.towers$country[ext]

pca.towers$type = paste(pca.towers$active,pca.towers$ch4,pca.towers$annual,sep = '_')
bars$type = pca.towers$type[ext]
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

png(filename = './output/aug/ch4/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle('Top 50 Percentile Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=type),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

png(filename = './output/aug/ch4/barplot_reduction_country.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle('Top 50 Percentile Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

write.csv(x = bars,file = './output/aug/ch4/meanreduction.csv',row.names = F)

####################################################################################
#Winter NETWORK #########################################################
####################################################################################
#the winter network
net = which(pca.towers$annual == 'yes' & pca.towers$active == 'active')# | pca.towers$sitename == 'Resolute' | pca.towers$sitename == 'Kangiqsualujjuaq')
ext = which(pca.towers$annual != 'yes' | is.na(pca.towers$active))# & pca.towers$sitename != 'Resolute' & pca.towers$sitename != 'Kangiqsualujjuaq')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
wch  = matrix(nrow = nrow(df),ncol = num)
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#calculate the euc distance of the network and one extension site
orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
  for (i in 1:nrow(df)) {
    dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
    
  }
  eucis[,j] = dist
  progress(j,ncol(euci.ext))
}
Sys.time() - orig

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
orig = Sys.time()
for (i in 1:nrow(df)) {
  base.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
}
Sys.time() - orig

#create base image
base = rasterFromXYZ(xyz = cbind(df$x,df$y,base.dist),crs = crs(r))

towers = SpatialPointsDataFrame(coords = pca.towers[net,c(1,2)],data = pca.towers[net,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

base.ster = projectRaster(from = base,crs = proj)

png(filename = './output/aug/annual/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('Annual Sites')+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = 'gray80',lwd = 0.25)+
  layer_spatial(base.ster$base.dist)+
  geom_point(data = towers.st,aes(x.1,y.1),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,4),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
theme(text = element_text(size = 8),
      legend.text = element_text(size = 8),
      title = element_text(size = 10),
      axis.title = element_text(size = 8),
      legend.key.width = unit(x = 0.1,units = 'in'))
dev.off()

#save the base here
writeRaster(x = base,filename = './output/aug/annual/base_2_towers.tif',overwrite = T)

#now create rasters out of the euclidean distances calculated for extension sites
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rasterFromXYZ(xyz = tempdf,crs = crs(r))
  progress(i,ncol(eucis))
}

#create a path of file names
path = paste('./output/aug/annual/ext/',pca.towers$sitename[ext],'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}

#####################################################################################
#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/aug/annual/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
}

#reproject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#plot
path = paste('./output/aug/annual/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 200)
  plots =   ggplot()+theme_bw()+ggtitle(pca.towers$sitename[ext][i])+
    geom_polygon(data = wm,aes(x = long,y=lat,group=group),
                 fill = 'gray80',lwd = 0.25)+
    layer_spatial(dif.ster[[i]])+
    geom_point(data = towers.st,aes(x.1[i],y.1[i]),col='black',fill='white',pch=25)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-.3,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
    theme(text = element_text(size = 8),
          legend.text = element_text(size = 8),
          title = element_text(size = 10),
          axis.title = element_text(size = 8),
          legend.key.width = unit(x = 0.1,units = 'in'))
  print(plots)
  dev.off()
  progress(i,length(dif.ster))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = cellStats(x = difs[[i]],stat = mean)
}

#add other parts of the dataframe back in
bars = data.frame(pca.towers$sitename[ext])
bars$means = means
bars$country = pca.towers$country[ext]

pca.towers$type = paste(pca.towers$active,pca.towers$ch4,pca.towers$annual,sep = '_')
bars$type = pca.towers$type[ext]
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))

png(filename = './output/aug/annual/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle('Top 50 Percentile Improvements - Annual')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=type),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,-1*min(top$means)+0.005),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

write.csv(x = bars,file = './output/aug/annual/meanreduction.csv',row.names = F)

####################################################################################
#Winter CH4 NETWORK #########################################################
####################################################################################
#the winter and ch4 network
net = which(pca.towers$active == 'active' & pca.towers$ch4 == 'CH4' & pca.towers$annual == 'yes')# | pca.towers$sitename == 'Resolute' | pca.towers$sitename == 'Kangiqsualujjuaq')
ext = which(pca.towers$active != 'active' | pca.towers$ch4 != 'CH4' | pca.towers$annual != 'yes' |
              is.na(pca.towers$active))# & pca.towers$sitename != 'Resolute' & pca.towers$sitename != 'Kangiqsualujjuaq')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
wch  = matrix(nrow = nrow(df),ncol = num)
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#calculate the euc distance of the network and one extension site
orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
  for (i in 1:nrow(df)) {
    dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
  }
  eucis[,j] = dist
  progress(j,ncol(euci.ext))
}
Sys.time() - orig

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
orig = Sys.time()
for (i in 1:nrow(df)) {
  base.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
}
Sys.time() - orig

#create base image
base = rasterFromXYZ(xyz = cbind(df$x,df$y,base.dist),crs = crs(r))

towers = SpatialPointsDataFrame(coords = pca.towers[net,c(1,2)],data = pca.towers[net,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

base.ster = projectRaster(from = base,crs = proj)

png(filename = './output/aug/annual ch4/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle(expression('Annual'~CH[4]~'Sites'))+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = 'gray80',lwd = 0.25)+
  layer_spatial(base.ster$base.dist)+
  geom_point(data = towers.st,aes(x.1,y.1),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
theme(text = element_text(size = 12),
      legend.text = element_text(size = 12),
      title = element_text(size = 14),
      axis.title = element_text(size = 12),
      legend.key.height = unit(x = 0.1,units = 'in'),
      legend.position = c(0.2,0.92),
      legend.direction = 'horizontal')
dev.off()

#save the base here
writeRaster(x = base,filename = './output/aug/annual ch4/base_2_towers.tif',overwrite = T)

#now create rasters out of the euclidean distances calculated for extension sites
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rasterFromXYZ(xyz = tempdf,crs = crs(r))
  progress(i,ncol(eucis))
}

#create a path of file names
path = paste('./output/aug/annual ch4/ext/',pca.towers$sitename[ext],'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}

#####################################################################################
#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/aug/annual ch4/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
}

#repropject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#plot
path = paste('./output/aug/annual ch4/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 200)
  plots =   ggplot()+theme_bw()+ggtitle(pca.towers$sitename[ext][i])+
    geom_polygon(data = wm,aes(x = long,y=lat,group=group),
                 fill = 'gray80',lwd = 0.25)+
    layer_spatial(dif.ster[[i]])+
    geom_point(data = towers.st,aes(x.1[i],y.1[i]),col='black',fill='green',pch=25)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-.5,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
    theme(text = element_text(size = 8),
          legend.text = element_text(size = 8),
          title = element_text(size = 10),
          axis.title = element_text(size = 8),
          legend.key.width = unit(x = 0.1,units = 'in'))
  print(plots)
  dev.off()
  progress(i,length(dif.ster))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = cellStats(x = difs[[i]],stat = mean)
}

#add other parts of the dataframe back in
bars = data.frame(pca.towers$sitename[ext])
bars$means = means
bars$country = pca.towers$country[ext]

pca.towers$type = paste(pca.towers$active,pca.towers$ch4,pca.towers$annual,sep = '_')
bars$type = pca.towers$type[ext]
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
top = subset(bars,bars$means < quantile(bars$means)[2])

top$type = ifelse(top$type == 'active_no_','Active',
           ifelse(top$type == 'NA_NA_NA','New',
           ifelse(top$type == 'active_CH4_','Active w/ CH4',
           ifelse(top$means == 'active_no_yes','Annual','Inactive'))))


png(filename = './output/aug/annual ch4/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle(expression('Top 25 Percentile Improvements - Annual'~CH[4]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=type),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,-1*min(top$means)+0.005),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        title = element_text(size = 18),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

png(filename = './output/aug/annual ch4/barplot_reduction_country.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle(expression('Top 25 Percentile Improvements - Annual'~CH[4]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,-1*min(top$means)+0.005),'Mean Distance Reduction')+
  scale_x_discrete('Sites')+
  scale_fill_brewer(palette = "Dark2",'Country')+
  theme(axis.text.x = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        title = element_text(size = 18),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

write.csv(x = bars,file = './output/aug/annual ch4/meanreduction.csv',row.names = F)
