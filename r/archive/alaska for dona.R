
library(ggplot2)
library(ggspatial)
library(terra)
library(sf)
library(data.table)
library(cowplot)
library(ggnewscale)

#######################################################################################
base.i           = rast('./output/improved_network/improved_base_2km.tif')
r = rast('./spatial_data/mir_aug_10yrmean.tif')
base.ak = project(base.i,r)

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base.ak))

#create aggregates for the plots
base.ak.ag  = aggregate(x = base.ak,fact = 4,fun = mean,na.rm = T)

#improved network
tower.data.i = fread(file = './data/pca.towers.upgraded.csv')
base.towers.i   = subset(tower.data.i,tower.data.i$active == 'active')


pal2 = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

ak = crop(x = base.ak,y = ext(-180,-140,50,72))
plot(ak)

ak = aggregate(x = ak,fact=2,fun='mean',na.rm=T)
#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ak)+
  geom_sf(data = countries,fill='transparent',col='black')+
 scale_fill_gradientn('Representativeness',
                      na.value = 'transparent',
                      colours = pal2,
                      limits = c(0,1.53*2),
                      breaks = c(0,1.53,1.53*2),
                      labels = c('Good','Cutoff','Poor'),
                      oob = scales::squish)+
 new_scale("fill") +
  geom_point(data = base.towers.i,aes(Longitude,Latitude,fill=methane,pch=Season_Activity,col=methane),
             col='black',show.legend = F,cex = 2)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','lightblue'))+
  scale_x_continuous(limits = c(-180,-140))+
  scale_y_continuous(limits = c(50,72))

  base.plot


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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CH[4]),size=2)
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','turquoise'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Year-Round'~CO[2]),size=2)
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Year-Round'~CH[4]),size=2)
#annual.methane.plot
####################################################################################################

#base.i
base.plot.i = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag.i)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers.i,aes(x,y,fill=methane,pch=Season_Activity,col=methane),
             col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','turquoise'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CO[2]),size=2)
#base.plot

#methane
methane.plot.i = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(methane.ag.i)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers.i,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CH[4]),size=2)
#methane.plot

#annual
annual.plot.i = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.ag.i)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers.i,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','turquoise'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Year-Round'~CO[2]),size=2)
#annual.plot

#annual methane
annual.methane.plot.i = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annual.ag.i)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers.i,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.04,units = 'in'),
        legend.key.width = unit(x = 0.18,units = 'in'),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6.5),
        legend.position = c(0.03,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Year-Round'~CH[4]),size=2)
#annual.methane.plot.i
###################################################################################################

#plot all 8 together
png(filename = './figures/figure 1 all.4.scenarios.png',width = 6,height = 9.5,units = 'in',res = 2000)
plot_grid(base.plot,base.plot.i,
          methane.plot,methane.plot.i,
          annual.plot,annual.plot.i,
          annual.methane.plot,annual.methane.plot.i,
          labels = c('a','e',
                     'b','f',
                     'c','g',
                     'd','h'),label_size = 7,rows = 4)
dev.off()


#presentation size ###########################################################
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity,col=methane),
             col='black',show.legend = F,cex = 1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','blue'))+
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
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Summer'~CO[2]),size=3)
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 1.5)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Summer'~CH[4]),size=3)
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','blue'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Annual'~CO[2]),size=3)
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
  geom_point(data = base.towers,aes(x,y,fill=methane,pch=Season_Activity),col='black',show.legend = F,cex = 1.5)+
  scale_shape_manual(values = c(21,2),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('orange1','transparent'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        axis.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Annual'~CH[4]),size=3)
#annual.methane.plot

#plot all 4 together
png(filename = './figures/figure 1 base.all.4.scenarios_presentation.png',width = 6,height = 5,units = 'in',res = 2000)
plot_grid(base.plot,methane.plot,annual.plot,annual.methane.plot)
dev.off()
