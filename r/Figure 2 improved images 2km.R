
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
improved.base           = rast('./output/improved_network/improved_base_2km.tif')
improved.methane        = rast('./output/improved_network/improved_methane_2km.tif')
improved.annual         = rast('./output/improved_network/improved_annual_2km.tif')
improved.annual.methane = rast('./output/improved_network/improved_annual_methane_2km.tif')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(improved.base))

#create aggregates for the plots
improved.base.ag           = aggregate(x = improved.base,fact = 4,fun = mean,na.rm = T)
improved.methane.ag        = aggregate(x = improved.methane,fact = 4,fun = mean,na.rm = T)
improved.annual.ag         = aggregate(x = improved.annual,fact = 4,fun = mean,na.rm = T)
improved.annual.methane.ag = aggregate(x = improved.annual.methane,fact = 4,fun = mean,na.rm = T)

#plot the figure
#pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')
pal = hcl.colors(n = 9,palette = 'BuGn')


active = subset(tower.data,tower.data$active == 'active')
tower.data$site

new.sites.active   = subset(tower.data,
                            tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                            tower.data$site == "Cambridge Bay, Victoria Island, wetland" |
                            tower.data$site == "Churchill Fen" |
                            tower.data$site == "Council (Permafrost Pathways)" |
                            tower.data$site == "Focal Campaign Site 1" |
                            tower.data$site == "Focal Campaign Site 2" |
                            tower.data$site == "Iqaluit (PP)" |
                            tower.data$site == "Kangiqsuallujjuaq" |
                            tower.data$site == "Kilpisjaervi" |
                   #        tower.data$site == "Lutose" |
                            tower.data$site == "Oulanka, Puukkosuo fen" |
                            tower.data$site == "Pond Inlet (PP)" |
                    #        tower.data$site == "Scotty Creek Bog" |
                            tower.data$site == "Scotty Creek Landscape" |
                   #         tower.data$site == "Smith Creek" |
                    #        tower.data$site == "Steen River" |
                            tower.data$site == "Resolute Bay" |
                            tower.data$site == "Trail Valley Creek (tall)" |
                            tower.data$site == "Udleg Forest Research Station" |
                            tower.data$site == "ZOTTO-Fen EC")

new.sites.ch4   = subset(tower.data,
                         tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                           tower.data$site == "Cambridge Bay, Victoria Island, wetland" |
                           tower.data$site == "Churchill Fen" |
                           tower.data$site == "Council (Permafrost Pathways)" |
                           tower.data$site == "Focal Campaign Site 1" |
                           tower.data$site == "Focal Campaign Site 2" |
                           tower.data$site == "Iqaluit (PP)" |
                           tower.data$site == "Kangiqsuallujjuaq" |
                           #tower.data$site == "Kilpisjaervi" |
                           tower.data$site == "Lutose" |
                           tower.data$site == "Oulanka, Puukkosuo fen" |
                           tower.data$site == "Pond Inlet (PP)" |
                           #        tower.data$site == "Scotty Creek Bog" |
                           tower.data$site == "Scotty Creek Landscape" |
                           #         tower.data$site == "Smith Creek" |
                           #        tower.data$site == "Steen River" |
                           tower.data$site == "Resolute Bay" |
                           #tower.data$site == "Trail Valley Creek (tall)" |
                           #tower.data$site == "Udleg Forest Research Station" |
                           tower.data$site == "ZOTTO-Fen EC")

new.sites.annual   = subset(tower.data,
                            tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                              tower.data$site == "Cambridge Bay, Victoria Island, wetland" |
                              tower.data$site == "Churchill Fen" |
                              tower.data$site == "Council (Permafrost Pathways)" |
                              #tower.data$site == "Focal Campaign Site 1" |
                              #tower.data$site == "Focal Campaign Site 2" |
                              tower.data$site == "Iqaluit (PP)" |
                              tower.data$site == "Kangiqsuallujjuaq" |
                              tower.data$site == "Kilpisjaervi" |
                              tower.data$site == "Lutose" |
                              #tower.data$site == "Oulanka, Puukkosuo fen" |
                              tower.data$site == "Pond Inlet (PP)" |
                              tower.data$site == "Scotty Creek Bog" |
                              tower.data$site == "Scotty Creek Landscape" |
                              tower.data$site == "Smith Creek" |
                              tower.data$site == "Steen River" |
                              #tower.data$site == "Resolute Bay" |
                              #tower.data$site == "Trail Valley Creek (tall)" |
                              tower.data$site == "Udleg Forest Research Station")
                              #tower.data$site == "ZOTTO-Fen EC")

new.sites.annual.ch4   = subset(tower.data,
                                tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                                  tower.data$site == "Cambridge Bay, Victoria Island, wetland" |
                                  tower.data$site == "Churchill Fen" |
                                  tower.data$site == "Council (Permafrost Pathways)" |
                                  #tower.data$site == "Focal Campaign Site 1" |
                                  #tower.data$site == "Focal Campaign Site 2" |
                                  tower.data$site == "Iqaluit (PP)" |
                                  tower.data$site == "Kangiqsuallujjuaq" |
                                  #tower.data$site == "Kilpisjaervi" |
                                  tower.data$site == "Lutose" |
                                  #tower.data$site == "Oulanka, Puukkosuo fen" |
                                  tower.data$site == "Pond Inlet (PP)" |
                                  tower.data$site == "Scotty Creek Bog" |
                                  tower.data$site == "Scotty Creek Landscape" |
                                  tower.data$site == "Smith Creek" |
                                  tower.data$site == "Steen River")
                                  #tower.data$site == "Resolute Bay" |
                                  #tower.data$site == "Trail Valley Creek (tall)" |
                                  #tower.data$site == "Udleg Forest Research Station" |
                                  #tower.data$site == "ZOTTO-Fen EC")

#improved normal plots (i.e., not differences)
#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('0','1.5','3+'),
                       oob = scales::squish)+  
#  new_scale("fill") +
#  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,cex=1.5)+
#  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
#  scale_fill_manual(values = c('red','green3'))+
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
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Summer'~CO[2]),size=2)
base.plot

#methane
methane.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
  geom_point(data = new.sites.methane,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
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
  layer_spatial(improved.annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green3'))+
  geom_point(data = new.sites.annual,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,cex=1.5)+
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
  layer_spatial(improved.annual.methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
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
  annotate(geom = 'text',x = -3193909,y = 3474170,label = expression('Annual'~CH[4]),size=2)
#annual.methane.plot


#plot all 4 together
png(filename = './figures/figure_2_improved.all.4.scenarios.v2_mean.png',width = 6,height = 5,units = 'in',res = 1000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot,labels = c('a','b','c','d'),label_size = 7)
dev.off()



#difference plots ##################################################################
#improved normal plots (i.e., not differences)
#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(improved.base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('0','1.5','3+'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green3'))+
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
  layer_spatial(improved.methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
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
  layer_spatial(improved.annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex=1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green3'))+
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
  layer_spatial(improved.annual.methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
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
png(filename = './figures/presentation improved.all.4.scenarios.png',width = 6,height = 5,units = 'in',res = 2500)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
dev.off()
