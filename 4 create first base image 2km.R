#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
gc()
#setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')
setwd('~')

library(svMisc)
library(maps)
library(ggplot2)
library(ggspatial)
library(terra)
#library(kit)
library(sf)
library(viridis)
library(data.table)
#library(readr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#load back in euclidean distance matrix
euci = read_rds('./data/euci_2km.rds')

pca.towers1 = tower.data
pca.towers1[,c('site','Activity')]

#add churchill and iqaluit to inactive towers
pca.towers1$Activity = ifelse(pca.towers1$site == 'Churchill Fen' | pca.towers1$site == 'Iqaluit',
                              'inactive',pca.towers1$Activity)
net = which(pca.towers1$Activity == 'active')

euci.net = euci[,c(net)]

rm(euci)
gc()

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
for (i in 1:nrow(euci.net)) {
  base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
  }
Sys.time() - orig} #stop the clock

#create base image
#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
df = as.data.frame(x = r,xy = T,na.rm = T)

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers database
base.towers = tower.data[net,]
towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))

hist(base)
plot(base,range=c(0,4.5))
points(towers)

#save the base here
writeRaster(x = base,filename = './output/base_2km.tif',overwrite = T)

#######################################################################################
base = rast('./output/base_2km.tif')
base = base/minmax(base)[2]

base
plot(base)
#load in base map
#things needed for all the plots
pal = viridis(n = 8,direction = -1,option = 'A')


#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 35, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

ch4 = subset(base.towers,base.towers$CH4 == 'CH4')
annualch4 = subset(base.towers,base.towers$CH4 == 'CH4' & base.towers$Annual_cover == 'annual')

#plot the figure
png(filename = './figures/base.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  geom_point(data = base.towers,aes(x,y),col='black',fill='red',pch=23,size=2)+
  geom_point(data = ch4,aes(x,y),col='black',fill='yellow',pch=23,size=2)+
  geom_point(data = annualch4,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3.25),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))
dev.off()


