
library(data.table)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggnewscale)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')
tower.data$active = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active)

#######################################################################################
base           = rast('./output/base_network/base_2kmv2_mean.tif')
methane        = rast('./output/base_network/methane_2kmv2_mean.tif')
annual         = rast('./output/base_network/annual_2kmv2_mean.tif')
annual.methane = rast('./output/base_network/annual_methane_2kmv2_mean.tif')

improved.base           = rast('./output/improved_network/improved_base_2kmv2_mean.tif')
improved.methane        = rast('./output/improved_network/improved_methane_2kmv2_mean.tif')
improved.annual         = rast('./output/improved_network/improved_annual_2kmv2_mean.tif')
improved.annual.methane = rast('./output/improved_network/improved_annual_methane_2kmv2_mean.tif')

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
active = subset(tower.data,tower.data$active == 'active')

new.sites.active   = subset(tower.data,
                     tower.data$site == "Scotty Creek Landscape" |
                     tower.data$site == "Kangiqsuallujjuaq" |
                     tower.data$site == "Resolute Bay" |
                     tower.data$site == "Iqaluit (PP)" |
                     tower.data$site == "Pond Inlet (PP)" |
                     tower.data$site == "Churchill Fen" |
                     tower.data$site == "Council (Permafrost Pathways)" |
                     tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                     tower.data$site == "Cambridge Bay, Victoria Island, wetland")

new.sites.ch4   = subset(tower.data,
                              tower.data$site == "Scotty Creek Landscape" |
                              tower.data$site == "Kangiqsuallujjuaq" |
                              tower.data$site == "Resolute Bay" |
                              tower.data$site == "Iqaluit (PP)" |
                              tower.data$site == "Pond Inlet (PP)" |
                              tower.data$site == "Churchill Fen" |
                              tower.data$site == "Council (Permafrost Pathways)" |
                              tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                              tower.data$site == "Cambridge Bay, Victoria Island, wetland")

new.sites.annual   = subset(tower.data,tower.data$site == "Lutose" |
                           tower.data$site == "Scotty Creek Landscape" |
                           tower.data$site == "Steen River" |
                           tower.data$site == "Kangiqsuallujjuaq" |
                           tower.data$site == "Scotty Creek Bog" |
                           tower.data$site == "Resolute Bay" |
                           tower.data$site == "Smith Creek" |
                           tower.data$site == "Iqaluit (PP)" |
                           tower.data$site == "Pond Inlet (PP)" |
                           tower.data$site == "Churchill Fen" |
                           tower.data$site == "Council (Permafrost Pathways)" |
                           tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                           tower.data$site == "Cambridge Bay, Victoria Island, wetland")

new.sites.annual.ch4   = subset(tower.data,tower.data$site == "Lutose" |
                              tower.data$site == "Scotty Creek Landscape" |
                              tower.data$site == "Steen River" |
                              tower.data$site == "Kangiqsuallujjuaq" |
                              tower.data$site == "Scotty Creek Bog" |
                              tower.data$site == "Resolute Bay" |
                              tower.data$site == "Smith Creek" |
                              tower.data$site == "Iqaluit (PP)" |
                              tower.data$site == "Pond Inlet (PP)" |
                              tower.data$site == "Churchill Fen" |
                              tower.data$site == "Council (Permafrost Pathways)" |
                              tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                              tower.data$site == "Cambridge Bay, Victoria Island, wetland")


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
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,cex=0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites.active,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=0.8)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        legend.text = element_text(size = 5),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Summer'~CO[2]),size=2)
#base.plot

#methane
methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(methane.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites.ch4,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=0.8)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Summer'~CH[4]),size=2)
#methane.plot

#annual
annual.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites.annual,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=0.8)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Annual'~CO[2]),size=2)
#annual.plot

#annual methane
annual.methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.methane.dif)+
  scale_fill_gradientn( na.value = 'transparent',
                       colours = pal,
                       oob = scales::squish,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       'Improvement')+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites.annual.ch4,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=0.8)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Annual'~CH[4]),size=2)
#annual.methane.plot

#plot all 4 together
png(filename = './figures/figure_sxx_different plot.png',width = 6,height = 5,units = 'in',res = 1000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
dev.off()



##############################################################################################

#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites.active,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        legend.text = element_text(size = 5),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Summer'~CO[2]),size=3)
#base.plot

#methane
methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(methane.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites.ch4,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Summer'~CH[4]),size=3)
#methane.plot

#annual
annual.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.dif)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.0*2),
                       breaks = c(0,1.0,1.0*2),
                       labels = c('0','1','2+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites.annual,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Annual'~CO[2]),size=3)
#annual.plot

#annual methane
annual.methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.methane.dif)+
  scale_fill_gradientn( na.value = 'transparent',
                        colours = pal,
                        oob = scales::squish,
                        limits = c(0,1.0*2),
                        breaks = c(0,1.0,1.0*2),
                        labels = c('0','1','2+'),
                        'Improvement')+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites.annual.ch4,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Annual'~CH[4]),size=3)
#annual.methane.plot

#plot all 4 together
png(filename = './figures/difference plot presentation.png',width = 6,height = 5,units = 'in',res = 2500)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
dev.off()

