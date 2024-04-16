
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)

#load in the stack created in the other file
base = rast('C:/site.selection/reps/summean_v5_base.tif')
clim = rast('C:/site.selection/data/input data/climate/climate_resample.tif')
sat  = rast('C:/site.selection/data/input data/sat_data.tif')

sat = project(x = sat,y = clim)
names(sat) = c('gdd','fdd','ndvimax','ndvimean','ndwimin','lstmean')

r = c(clim,sat,base)
r = subset(x = r,subset = c(1,4,11:13,15:17))

names(r)
plot(r)

#load in sites
library(data.table)
tower.data = fread(file = 'C:/site.selection/data/extracted_base_data.csv')

#cut down raster data to remove NAs
df = as.data.frame(r,xy = T)
df$cell = seq(length.out = length(df$summean_v5_base)) #add an ID column for matching up data
df2 = df[complete.cases(df),]

#run a PCA on the full data set
library(plyr)
#add the tower sites to the main data from the raster
pca.input = rbind.fill(df2,tower.data)

#run the PCA
pca = prcomp(x = pca.input[,c(3:9)],center = T,scale. = T)
summary(pca)
pcadf = data.frame(pca$x[,c(1:3)])

pca.input$pc1 = pcadf$PC1
pca.input$pc2 = pcadf$PC2
pca.input$pc3 = pcadf$PC3

#td = merge(tower.data,df2,by = 'cells',all.x = T)
pca.towers = pca.input[complete.cases(pca.input$sitename),]
pca.pixels = pca.input[is.na(pca.input$sitename),]

ggplot()+theme_bw()+
  geom_hex(data = pca.pixels,aes(pc1,pc2),bins=200)+
  scale_fill_viridis_c()+
  geom_label(data = pca.towers,aes(pc1,pc2,label = sitename),size=2.5,fill='transparent',col='red',label.size = NA)

#use this to check out drivers of the PCA
library(ggfortify)
#autoplot(pca,loadings = T,loadings.label = T)

#calculate the euclidean distance for the whole data set
#convert dataframes to data tables which process faster in loops
pca.dt  = data.table(pca.pixels)
tdpcadt = data.table(pca.towers)

######################################################################################
#pre-populating the whole matrix makes computation time much faster *DO NOT USE A DATAFRAME
euci = matrix(nrow = nrow(pca.dt),ncol = nrow(tdpcadt))

for (j in 1:nrow(tdpcadt)) {  #j is the tower data frame
  for (i in 1:nrow(pca.dt)) { #i is the full data frame
euci[i,j] = sqrt((pca.dt$pc1[i]-tdpcadt$pc1[j])^2 + 
                 (pca.dt$pc2[i]-tdpcadt$pc2[j])^2 + 
                 (pca.dt$pc3[i]-tdpcadt$pc3[j])^2)
  }
 progress(j,nrow(tdpcadt))
}
Sys.time() - orig

#create a vector to calculate the minimum distance
num = 3 #how many closest towers you want
dist = vector(length = nrow(df2)) #again premaking a vector of the right length greatly speeds up comp time
wch  = matrix(nrow = nrow(df2),ncol = num)

#calulate based on the mean of the X lowest
orig = Sys.time()
for (i in 1:nrow(df2)) {
  wch[i,1:num] = which(frank(euci[i,], ties.method='first') <= num)
  dist[i]    = mean(euci[i,wch[i,]])
}
Sys.time() - orig

#calculate based just on the minimum
#min.dist = vector(length = nrow(df2))
#for (i in 1:nrow(df2)) {min.dist[i] = min(euci[i,])} #this takes the minimum distance

df2$dist = dist
#df2$min = min.dist

dist.r = rasterFromXYZ(xyz = df2[,c(1,2,12)],crs = crs(r))
dist.r = project(x = rast(dist.r),y = r)

hist(dist.r)
plot(dist.r)
#min.r = rasterFromXYZ(xyz = df2[,c(1,2,24)],crs = crs(r))
#min.r = projectRaster(from = min.r,to = r)

ggplot()+theme_bw()+
  geom_hex(aes(df2$summean_v5_base,dist),bins=300)+
  scale_fill_viridis_c()+
  scale_x_continuous('Pallandt',limits = c(0,8))+
  scale_y_continuous('Arndt',limits = c(0,8))+
  geom_abline(slope = 1,intercept = 0,col='red')

ggplot()+theme_bw()+
  geom_hex(aes(df2$summean_v5_base,min.dist),bins=300)+
  scale_fill_viridis_c()+
  scale_x_continuous('Pallandt',limits = c(0,8))+
  scale_y_continuous('Arndt',limits = c(0,8))+
  geom_abline(slope = 1,intercept = 0,col='red')

hist(dist.r,xlim = c(0,8))

library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')

#projection
proj = CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

dist.ster = projectRaster(from = stack(dist.r),crs = proj)
#min.ster = project(x = min.r,y = proj)

p = crs(raster(r))
towers = SpatialPointsDataFrame(coords = tower.data[,c(10,11)],data = tower.data,proj4string = p)
towers = spTransform(x = towers,CRSobj = proj)

towers.st = as.data.frame(towers)

#world map at a coarse resolution, spatial polygons dataframe
library(rworldmap)
library(cleangeo)
worldmap = getMap(resolution = 'coarse')
worldmap = clgeo_Clean(worldmap)

wm = crop(x = worldmap,y = extent(-180,180,45,90))
wm = spTransform(x = wm,CRSobj = proj)
wm = crop(x = wm,y = extent(c(-4772265,4580235,-3672265,4580235)))

dist.ster$dist

hist(dist.ster$dist)

ggplot()+theme_bw()+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = '#e1e1e1',lwd = 0.25)+
  layer_spatial(dist.ster$dist)+
  geom_point(data = towers.st,aes(x.1,y.1),col='green')+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                   #    trans = 'log',
                       limits = c(0,2.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4880235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-4880235,4580235),expand = c(0,0))


ggplot(data = df2)+
  geom_hex(aes(dist,Annual.Mean.Temperature),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,Annual.Precipitation),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,gdd),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,fdd),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,ndvimax),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,ndwimin),bins = 100)+
  scale_fill_viridis_c()

ggplot(data = df2)+
  geom_hex(aes(dist,lstmean),bins = 100)+
  scale_fill_viridis_c()



ggplot()+theme_bw()+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = '#e1e1e1',lwd = 0.25)+
  layer_spatial(min.ster$min)+
  geom_point(data = towers.st,aes(x.1,y.1),col='green')+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(0,3.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235))+
  scale_y_continuous(limits = c(-4780235,4580235))

writeRaster(x = dist.r,filename = 'C:/site.selection/rep analysis/base_3_towers.tif')



t = stack('C:/site.selection/rep analysis/base_5_towers.tif')

plot(t,zlim=c(0,3),ylim=c(60,70),xlim=c(-30,-10))
