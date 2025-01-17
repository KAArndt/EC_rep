#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
gc()

library(ggplot2)
library(ggspatial)
library(terra)
library(sf)
library(data.table)
library(cowplot)
library(ggnewscale)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.base.csv')

#######################################################################################
base           = rast('./output/base_2kmv2_min.tif')
methane        = rast('./output/methane_2kmv2_min.tif')
annual         = rast('./output/annual_2kmv2_min.tif')
annual.methane = rast('./output/annual_methane_2kmv2_min.tif')

#base = base/minmax(base)[2] #use this to rescale from 0-1

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

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

#create a scatter to show data spread
# base.df = as.data.frame(base)
# 
# ggplot()+theme_classic()+
#   geom_density(aes(base.df),fill='gray',alpha=0.5)+
#   scale_x_continuous(expand = c(0,0),'Euc. Dist.')+
#   scale_y_continuous(expand = c(0,0),limits = c(0,1))+
#   theme(text = element_text(size = 8))

base.towers            = subset(tower.data,tower.data$active == 'active' &
                                  tower.data$Start_CO2 < 2022)
# 
# methane.towers         = subset(tower.data,tower.data$active == 'active' & 
#                                   tower.data$Start_CO2 < 2022 & 
#                                   tower.data$methane == 'methane')
# 
# annual.towers          = subset(tower.data,tower.data$active == 'active' & 
#                                   tower.data$Start_CO2 < 2022 &
#                                   tower.data$Season_Activity == 'All year')
# 
# annual.methane.towers  = subset(tower.data,tower.data$active == 'active' & 
#                                   tower.data$Start_CO2 < 2022 &
#                                   tower.data$Season_Activity == 'All year' &
#                                   tower.data$methane == 'methane')


#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
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
  layer_spatial(methane.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
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
  layer_spatial(annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
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
  layer_spatial(annual.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','transparent'))+
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

# png(filename = './figures/base.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# base.plot
# dev.off()
# 
# png(filename = './figures/base.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# methane.plot
# dev.off()
# 
# png(filename = './figures/base.annual.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.plot
# dev.off()
# 
# png(filename = './figures/base.annual.methane.v2_min.png',width = 6,height = 6,units = 'in',res = 1000)
# annual.methane.plot
# dev.off()


#plot all 4 together
#png(filename = './figures/base.all.4.scenarios.v2_min.png',width = 12,height = 12,units = 'in',res = 1000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
#dev.off()
