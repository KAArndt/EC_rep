#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())

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

#remove the above that we don't need anymore to save memory needed in later steps
rm(clim,soil,sat)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/extracted_tower_data.csv')

#cut down raster data to remove NAs
df = as.data.frame(r,xy = T)

#run a PCA on the full data set
#add the tower sites to the main data from the raster
pca.input = rbind.fill(df,tower.data)

#run the PCA
pca = prcomp(x = pca.input[,c(3:23)],center = T,scale. = T)
summary(pca)
pcadf = data.frame(pca$x[,c(1:4)])

pca.input$pc1 = pcadf$PC1
pca.input$pc2 = pcadf$PC2
pca.input$pc3 = pcadf$PC3
pca.input$pc4 = pcadf$PC4

#td = merge(tower.data,df2,by = 'cells',all.x = T)
pca.towers   = subset(pca.input,complete.cases(pca.input$sitename))
#pca.ext    = subset(pca.input,pca.input$type == 'ext')
pca.pixels = subset(pca.input,is.na(pca.input$sitename))

sub = subset(x = pca.towers,pca.towers$active == 'active')
comp = subset(pca.towers,pca.towers$annual != 'yes' & pca.towers$active == 'active')

ggplot()+theme_bw()+
  geom_hex(data = pca.pixels,aes(pc1,pc2),bins=150)+
  scale_fill_viridis_c()+
  #geom_point(data = sub,aes(pc1,pc2),col='red')
  geom_label(data = sub,aes(pc1,pc2,label = sitename),
             size=3.5,fill='transparent',col='red',label.size = NA)

#use this to check out drivers of the PCA
#library(ggfortify)
#autoplot(pca,loadings = T,loadings.label = T)

#remove some unnecessary data again
rm(pca)

#################################################################################
#calculate the euclidean distance for the whole data set 
#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(pca.pixels)
pca.towers = data.table(pca.towers)

#pre-populating the whole matrix makes computation time much faster *DO NOT USE A DATAFRAME
euci = matrix(nrow = nrow(pca.dt),ncol = nrow(pca.towers))

orig = Sys.time()
for (j in 1:nrow(pca.towers)) {  #j is the tower data frame
  for (i in 1:nrow(pca.dt)) { #i is the full data frame
euci[i,j] = sqrt((pca.dt$pc1[i]-pca.towers$pc1[j])^2 + 
                 (pca.dt$pc2[i]-pca.towers$pc2[j])^2 + 
                 (pca.dt$pc3[i]-pca.towers$pc3[j])^2 +
                 (pca.dt$pc4[i]-pca.towers$pc4[j])^2)
  }
 progress(j,nrow(pca.towers))
}
Sys.time() - orig

#save off
#colnames(euci) = pca.towers$sitename
#fwrite(x = euci,file = './data/euci.csv',row.names = F)

#load back in
euci = fread('./data/euci.csv')
euci = as.matrix(euci)

#things needed for all the plots
library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

#world map at a coarse resolution, spatial polygons dataframe
library(rworldmap)
library(cleangeo)
worldmap = getMap(resolution = 'coarse')
worldmap = clgeo_Clean(worldmap)

wm = crop(x = worldmap,y = extent(-180,180,45,90))
wm = spTransform(x = wm,CRSobj = proj)
wm = crop(x = wm,y = extent(c(-4780235,4580235,-3880235,4580235)))

#################################################################
####     ALL ACTIVE SITES #######################################
#################################################################
#find columns which are active sites
net = which(pca.towers$active == 'active')# | pca.towers$sitename == 'Resolute' | pca.towers$sitename == 'Kangiqsualujjuaq')
ext = which(pca.towers$active != 'active' | is.na(pca.towers$active))# & pca.towers$sitename != 'Resolute' & pca.towers$sitename != 'Kangiqsualujjuaq')

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

towers = SpatialPointsDataFrame(coords = pca.towers[net,c(1,2)],data = pca.towers[net,],proj4string = crs(base.2))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

#create the base
base.ster = projectRaster(from = base,crs = proj)

png(filename = './output/active/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('All Active Sites')+
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
writeRaster(x = base,filename = './output/active/base_all_towers.tif',overwrite = T)

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
path = paste('./output/active/ext/',pca.towers$sitename[ext],'.tif',sep = '')
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
path = paste('./output/active/difs/',pca.towers$sitename[ext],'_dif.tif',sep = '')
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

png(filename = './output/active/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+ggtitle('Top 50 Percentile Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')
dev.off()

write.csv(x = bars,file = './output/active/meanreduction.csv',row.names = F)

#site level improvement plots
#reproject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#plot
path = paste('./output/active/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.towers[ext,c(1,2)],data = pca.towers[ext,],proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 500)
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


####################################################################################
#ACTIVE SUMMER CH4 NETWORK #########################################################
####################################################################################
#the ch4 network
net = which(pca.towers$ch4    == 'CH4' & pca.towers$active == 'active')# | pca.towers$sitename == 'Resolute' | pca.towers$sitename == 'Kangiqsualujjuaq')
ext = which(pca.towers$ch4    != 'CH4' | is.na(pca.towers$ch4))# & pca.towers$sitename != 'Resolute' & pca.towers$sitename != 'Kangiqsualujjuaq')

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

png(filename = './output/ch4/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('CH4 Sites')+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = 'gray80',lwd = 0.25)+
  layer_spatial(base.ster$base.dist)+
  geom_point(data = towers.st,aes(x.1,y.1),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3),
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
writeRaster(x = base,filename = './output/ch4/base_2_towers.tif',overwrite = T)

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
path = paste('./output/ch4/ext/',pca.towers$sitename[ext],'.tif',sep = '')
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
path = paste('./output/ch4/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
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
path = paste('./output/ch4/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

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

png(filename = './output/ch4/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
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

png(filename = './output/ch4/barplot_reduction_country.png',width = 10,height = 4,units = 'in',res = 2000)
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

write.csv(x = bars,file = './output/ch4/meanreduction.csv',row.names = F)

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

png(filename = './output/annual/base.png',width = 6,height = 6,units = 'in',res = 500)
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
writeRaster(x = base,filename = './output/annual/base_2_towers.tif',overwrite = T)

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
path = paste('./output/annual/ext/',pca.towers$sitename[ext],'.tif',sep = '')
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
path = paste('./output/annual/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
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
path = paste('./output/annual/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

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

png(filename = './output/annual/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
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

write.csv(x = bars,file = './output/annual/meanreduction.csv',row.names = F)

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

png(filename = './output/annual ch4/base.png',width = 6,height = 6,units = 'in',res = 500)
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
writeRaster(x = base,filename = './output/annual ch4/base_2_towers.tif',overwrite = T)

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
path = paste('./output/annual ch4/ext/',pca.towers$sitename[ext],'.tif',sep = '')
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
path = paste('./output/annual ch4/dif/',pca.towers$sitename[ext],'_dif.tif',sep = '')
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
path = paste('./output/annual ch4/plots/',pca.towers$sitename[ext],'_dif.png',sep = '')

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


png(filename = './output/annual ch4/barplot_reduction.png',width = 10,height = 4,units = 'in',res = 2000)
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

png(filename = './output/annual ch4/barplot_reduction_country.png',width = 10,height = 4,units = 'in',res = 2000)
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

write.csv(x = bars,file = './output/annual ch4/meanreduction.csv',row.names = F)
