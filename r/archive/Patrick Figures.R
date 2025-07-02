
library(maps)
library(ggplot2)
library(ggspatial)
library(terra)
library(sf)
library(data.table)
library(cowplot)
library(ggnewscale)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')

#new sites
new = subset(tower.data,
             tower.data$site == 'Pond Inlet (PP)' | 
               tower.data$site == 'Resolute Bay' |
               tower.data$site == 'Churchill Fen' |
               tower.data$site == 'Iqaluit (PP)' |
               tower.data$site == 'Kangiqsuallujjuaq' |
               tower.data$site == 'Scotty Creek Landscape' |
     #          tower.data$site == 'Scotty Creek Bog' |
               tower.data$site == 'Council (Permafrost Pathways)'|
#     tower.data$site == 'Lutose' |
#     tower.data$site == 'Steen River' |
      tower.data$site == 'Cambridge Bay, Victoria Island, mesic' |
      tower.data$site == 'Cambridge Bay, Victoria Island, wetland')
#    tower.data$site == 'Smith Creek' |
#     tower.data$site == 'Steen River' |
#    tower.data$site == 'Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Burned 2015' |
#   tower.data$site == 'Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Unburned')

#create base image
#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,xy = T,na.rm = T)

#universal plotting elements
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(r))

#plot of 4 conditions ############################################################ ############################
base = rast('./output/base_2kmv2_mean.tif')
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

#tower categories
active  = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#plot the figure
png(filename = './figures/base_network_large.png',width = 8,height = 8,units = 'in',res = 3000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representation',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Mod.','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.5,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')
dev.off()

############################################################################################
# improved with new sites
improved = rast('./output/improved_base_2kmv2_mean.tif')
improved.ag = aggregate(x = improved,fact = 4,fun = mean,na.rm = T)

#tower categories
active  = subset(tower.data,tower.data$active == 'active')

#plot the figure
png(filename = './figures/improved_network_large.png',width = 8,height = 8,units = 'in',res = 3000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.ag)+
  scale_fill_gradientn('Representation',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Mod.','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y),col='black',size=2,pch=21,fill='red')+
  geom_point(data = new,aes(x,y),col='black',size=2,pch=21,fill='cyan')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.5,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')
dev.off()


############################################################################################
# Change Map
dif = improved.ag - base.ag

#tower categories
active  = subset(tower.data,tower.data$active == 'active')

#plot the figure
png(filename = './figures/change_large.png',width = 8,height = 8,units = 'in',res = 3000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-0.8,0),
                       breaks = c(-0.8,-0.4,0),
                       labels = c('High','Mod.','Low'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y),col='black',size=2,pch=21,fill='red')+
  geom_point(data = new,aes(x,y),col='black',size=2,pch=21,fill='cyan')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.5,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')
dev.off()
