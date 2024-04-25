#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
library(svMisc)
library(ggplot2)
library(ggspatial)
library(terra)
library(data.table)
library(kit)
library(sf)
library(foreach)
library(doParallel)
library(doSNOW)
library(viridis)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#load back in euclidean distance matrix
euci = fread('./euci_new.csv')
euci = as.matrix(euci)

#################################################################
#### first go to base image #####################################
#################################################################
pca.towers1 = tower.data
pca.towers1[,c('site','Activity')]

#find columns which are active sites
net = which(pca.towers1$Activity == 'active')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
rm(euci)

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network
# {orig = Sys.time()
#   cores = detectCores()        
#   cl = makeCluster(cores[1]-2) 
#   registerDoSNOW(cl) #register the cores
# 
# base.dist = foreach (i = 1:nrow(euci.net),.combine = c) %dopar% {
#   mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])}
# stopCluster(cl) #stop the clusters
# Sys.time() - orig} #stop the clock

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
for (i in 1:nrow(euci.net)) {
  base.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
}

#create base image
#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
df = as.data.frame(x = r,xy = T,na.rm = T)

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

plot(base,range = c(0,4))

#create tower data frames and reproject
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

base.towers = tower.data[net,]
#names(towers) = c('lon','lat')
towers = vect(x = base.towers,geom=c("LON", "LAT"), crs=crs(r))
towers.ster = project(x = towers,y = proj)

base.towers$x = crds(towers.ster)[,1]
base.towers$y = crds(towers.ster)[,2]

#reproject the base
base.ster = project(x = base,y = proj)

#load in base map
#things needed for all the plots
pal = viridis(n = 8,direction = -1,option = 'A')

#background world map for plotting
map = map_data('world')
plot(map)
wm = vect(x = map,geom = c('long','lat'),crs = crs(base))

wm = SpatialPointsDataFrame(coords = map[,c(1,2)],data = map,proj4string = CRS(r))
wm = project(x = wm,y = proj)

wm = spTransform(x = wm,CRSobj = proj)
plot(wm)

sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 40, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))

proj2 = crs('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs=True')

base2 = aggregate(x = base,fun=mean,fact=5)

base.aea = project(x = base2,y = proj2)
base.aea
plot(base.aea)

#create an aggregate for the plot
base.ag = aggregate(x = base.ster,fact = 4.5,fun = mean)
base.ag
base.ster
#plot the figure
png(filename = './manuscript/figures/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  geom_point(data = base.towers,aes(x,y),col='black',fill='green',pch=25,size=2)+
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
writeRaster(x = base.ster,filename = './output/feb/base.tif',overwrite = T)
