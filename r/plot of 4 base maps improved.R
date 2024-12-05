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
pca.towers = tower.data

#add churchill and iqaluit to inactive towers
pca.towers$Activity = ifelse(pca.towers$site == 'Churchill Fen' | pca.towers$site == 'Iqaluit',
                              'inactive',pca.towers$Activity)

#create base image
#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
df = as.data.frame(x = r,xy = T,na.rm = T)

#load in all the base images
#######################################################################################
base = rast('./output/base_2km.tif')
ch4  = rast('./output/ch4_2km.tif')
year = rast('./output/annual_2km.tif')
ach4 = rast('./output/annualch4_2km.tif')

#plot the figure
#color pallette
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
ch4.ag  = aggregate(x = ch4,fact = 4,fun = mean,na.rm = T)
year.ag = aggregate(x = year,fact = 4,fun = mean,na.rm = T)
ach4.ag = aggregate(x = ach4,fact = 4,fun = mean,na.rm = T)

active = subset(pca.towers1,pca.towers1$Activity == 'active')
active$CH4 = ifelse(active$CH4 == '','no',active$CH4)


#plot the figure
p1 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag$base.dist)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.5),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',size=2,show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.9))

p2 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ch4.ag$base.dist)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.5),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',size=2,show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.9))

p3 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(year.ag$base.dist)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.5),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',size=2,show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.9))

p4 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ach4.ag$base.dist)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.5),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',size=2,show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.9))

png(filename = './figures/4scenarios.png',width = 5,height = 5,units = 'in',res = 1500)
plot_grid(p1,p2,p3,p4,labels = c('a','b','c','d'),label_size = 8)
dev.off()



#other single plots ##############################################################
new.sites = subset(tower.data,tower.data$site == 'Churchill Fen' |
                     tower.data$site == 'Council (NGEE Arctic)' |
                     tower.data$site == 'Iqaluit' |
                     tower.data$site == 'Kangiqsualujjuaq' |
                     tower.data$site == 'Lutose' |
                     tower.data$site == 'Pond Inlet' |
                     tower.data$site == 'Resolute' |
                     tower.data$site == 'Scotty Creek Bog' |
                     tower.data$site == 'Scotty Creek Landscape' |
                     tower.data$site == 'CEF cluster' |
                     tower.data$site == 'Chersky, control' |
                     tower.data$site == 'Chersky, drained' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, mesic' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, wetland' |
                     tower.data$site == 'Steen River' |
                     tower.data$site == 'Smith Creek' |
                     tower.data$site == 'Iqaluit')

#Base Network
png(filename = './figures/base_network.png',width = 5,height = 5,units = 'in',res = 2000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag$base.dist)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.5),
                       breaks = c(0,1.8,3.2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',show.legend = F)+
  geom_point(data = new.sites,aes(x,y),col='black',fill = 'red',pch = 21,show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')
dev.off()

