rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(ggplot2)
library(ggspatial)
library(terra)
library(data.table)
library(sf)
library(sp)
library(viridis)
library(readr)


#write the raster
base = rast('./5km/base_5km.tif')

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
net = which(tower.data$Activity == 'active')
base.towers = tower.data[net,]

#load in base map
#things needed for all the plots
pal = viridis(n = 8,direction = -1,option = 'A')

#background world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 40, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


#plot the figure
#png(filename = './manuscript/figures/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('Eddy Covariance Coverage')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base)+
  geom_point(data = base.towers,aes(x,y),col='black',fill='green',pch=25,size=2)+
  scale_fill_gradientn('Cover',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3.5),
                       labels = c('Good','Poor'),
                       breaks = c(0,3.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))
#dev.off()




#for chisasibi
basell = project(x = base,y = crs('+proj=longlat +datum=WGS84'))

plot(basell,xlim = c(-82,-65),ylim = c(48,60),range=c(0.2,3.5))
points(	-72.01071,53.98067)


#png(filename = './manuscript/figures/base.png',width = 6,height = 6,units = 'in',res = 500)
ggplot()+theme_bw()+ggtitle('Eddy Covariance Coverage')+
  layer_spatial(basell)+
  geom_point(aes(-72.01071,53.98067),col='black',fill='green',pch=25,size=3)+
  scale_fill_gradientn('Cover',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,3),
                       labels = c('Good','Poor'),
                       breaks = c(0,3),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-82,-65),expand = c(0,0))+
  scale_y_continuous(limits = c(48,60),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))

