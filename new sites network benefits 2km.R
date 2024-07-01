rm(list = ls())
gc()

library(readr)
library(terra)
library(kit)
library(svMisc)
library(data.table)
library(viridis)
library(maps)
library(sf)
library(ggplot2)
library(ggspatial)
setwd('~')

#load back in
euci = read_rds('./data/euci_2km.rds')

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=12,na.rm=T)
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
pca.towers1 = tower.data

#find columns which are active sites
pca.towers1$Activity = ifelse(pca.towers1$site == 'Churchill Fen' | 
                              pca.towers1$site == 'Iqaluit' |
                              pca.towers1$site == 'Resolute' |
                              pca.towers1$site == 'Pond Inlet' |
                              pca.towers1$site == 'Kangiqsualujjuaq' |
                              pca.towers1$site == 'CEF cluster' |
                              pca.towers1$site == 'Council (NGEE Arctic)',
                              'active',pca.towers1$Activity)

net = which(pca.towers1$Activity == 'active')

#create some subsets of the euclidean distance tables for easier calculations
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

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
improve = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers database
new.towers = tower.data[net,]
towers = vect(x = new.towers,geom=c("x", "y"), crs=crs(r))

hist(improve)

plot(improve,range=c(0,5))
points(towers)

#save the base here
writeRaster(x = improve,filename = './output/improvement_2km.tif',overwrite = T)

#caluclate difference
#load in the base
base = rast('./output/base_2km.tif')


#calulate difference
dif = base - improve


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
impr.ag = aggregate(x = improve,fact = 4,fun = mean,na.rm = T)
dif.ag  = aggregate(x = dif,fact = 4,fun = mean,na.rm = T)

#plot the figure
png(filename = './figures/improvement.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_bw()+ggtitle('Improvement')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(impr.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,4),
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

pal = viridis(n = 8,direction = -1,option = 'A')
hist(dif)

ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,2),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))


terra::summary(improve)
terra::summary(base)
