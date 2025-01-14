#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
gc()

library(data.table)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggnewscale)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towersv2.csv')
tower.data$active = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active)
tower.data$Season_Activity = ifelse(tower.data$site == "Lutose" |
                                      tower.data$site == "Scotty Creek Landscape" |
                                      tower.data$site == "Steen River" |
                                      tower.data$site == "Scotty Creek Bog" |
                                      tower.data$site == "Resolute Bay" |
                                      tower.data$site == "Smith Creek",
                                    'All year',tower.data$Season_Activity)
tower.data$site
new.sites = subset(tower.data,tower.data$site == "Lutose" |
                     tower.data$site == "Scotty Creek Landscape" |
                     tower.data$site == "Steen River" |
                     tower.data$site == "Scotty Creek Bog" |
                     tower.data$site == "Resolute Bay" |
                     tower.data$site == "Smith Creek" |
                     tower.data$site == "Iqaluit (PP)" |
                     tower.data$site == "Pond Inlet (PP)" |
                     tower.data$site == "Churchill Fen" |
                     tower.data$site == "Council (Permafrost Pathways)" |
                     tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                     tower.data$site == "Cambridge Bay, Victoria Island, wetland")

#######################################################################################
base           = rast('./output/base_2kmv2_min.tif')
methane        = rast('./output/methane_2kmv2_min.tif')
annual         = rast('./output/annual_2kmv2_min.tif')
annual.methane = rast('./output/annual_methane_2kmv2_min.tif')

improved.base           = rast('./output/improved_base_2kmv2_min.tif')
improved.methane        = rast('./output/improved_methane_2kmv2_min.tif')
improved.annual         = rast('./output/improved_annual_2kmv2_min.tif')
improved.annual.methane = rast('./output/improved_annual_methane_2kmv2_min.tif')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))

#create aggregates for the plots
base.ag           = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
methane.ag        = aggregate(x = methane,fact = 4,fun = mean,na.rm = T)
annual.ag         = aggregate(x = annual,fact = 4,fun = mean,na.rm = T)
annual.methane.ag = aggregate(x = annual.methane,fact = 4,fun = mean,na.rm = T)

improved.base.ag           = aggregate(x = improved.base,fact = 4,fun = mean,na.rm = T)
improved.methane.ag        = aggregate(x = improved.methane,fact = 4,fun = mean,na.rm = T)
improved.annual.ag         = aggregate(x = improved.annual,fact = 4,fun = mean,na.rm = T)
improved.annual.methane.ag = aggregate(x = improved.annual.methane,fact = 4,fun = mean,na.rm = T)

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

active = subset(tower.data,tower.data$active == 'active')

#improved normal plots (i.e., not differences)
#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Base')
base.plot

#methane
methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Methane')
#methane.plot

#annual
annual.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Annual')
#annual.plot

#annual methane
annual.methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Annual Methane')
#annual.methane.plot


#save plots
#individual

# png(filename = './figures/improved.base.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# base.plot
# dev.off()
# 
# png(filename = './figures/improved.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# methane.plot
# dev.off()
# 
# png(filename = './figures/improved.annual.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.plot
# dev.off()
# 
# png(filename = './figures/improved.annual.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.methane.plot
# dev.off()


#plot all 4 together
#png(filename = './figures/base.all.4.scenarios.v2_min.png',width = 12,height = 12,units = 'in',res = 1000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
#dev.off()

#difference plots ##################################################################
base.dif           = base.ag - improved.base.ag
methane.dif        = methane.ag - improved.methane.ag
annual.dif         = annual.ag - improved.annual.ag
annual.methane.dif = annual.methane.ag - improved.annual.methane.ag

pal = c('#F2F7FB','#8AABD6','#72509A','#E88D7A','#FEEDB9')

#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('High','Moderate','None'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Base')
#base.plot

#methane
methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(methane.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('High','Moderate','None'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Methane')
#methane.plot

#annual
annual.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('High','Moderate','None'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Annual')
#annual.plot

#annual methane
annual.methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.methane.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('High','Moderate','None'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = 'Annual Methane')
#annual.methane.plot


#save plots
#individual

# png(filename = './figures/difference.base.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# base.plot
# dev.off()
# 
# png(filename = './figures/difference.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# methane.plot
# dev.off()
# 
# png(filename = './figures/difference.annual.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.plot
# dev.off()
# 
# png(filename = './figures/difference.annual.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.methane.plot
# dev.off()


#plot all 4 together
#png(filename = './figures/base.all.4.scenarios.v2_min.png',width = 12,height = 12,units = 'in',res = 1000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
#dev.off()



