
library(ggplot2)
library(ggspatial)
library(terra)
library(sf)
library(data.table)
library(cowplot)
library(ggnewscale)

#######################################################################################
base             = rast('./output/base_network/base_2km.tif')
methane          = rast('./output/base_network/methane_2km.tif')
annual           = rast('./output/base_network/annual_2km.tif')
annual.methane   = rast('./output/base_network/annual_methane_2km.tif')

base.i           = rast('./output/improved_network/improved_base_2km.tif')
methane.i        = rast('./output/improved_network/improved_methane_2km.tif')
annual.i         = rast('./output/improved_network/improved_annual_2km.tif')
annual.methane.i = rast('./output/improved_network/improved_annual_methane_2km.tif')

#base = base/minmax(base)[2] #use this to re-scale from 0-1

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

base.ag.i           = aggregate(x = base.i,fact = 4,fun = mean,na.rm = T)
methane.ag.i        = aggregate(x = methane.i,fact = 4,fun = mean,na.rm = T)
annual.ag.i         = aggregate(x = annual.i,fact = 4,fun = mean,na.rm = T)
annual.methane.ag.i = aggregate(x = annual.methane.i,fact = 4,fun = mean,na.rm = T)

#plot the figure
pal = c('#FEEDB9','#E88D7A',F2F7FB,'#72509A','#8AABD6','#')

#create a scatter to show data spread
# base.df = as.data.frame(base)
# 
# ggplot()+theme_classic()+
#   geom_density(aes(base.df),fill='gray',alpha=0.5)+
#   scale_x_continuous(expand = c(0,0),'Euc. Dist.')+
#   scale_y_continuous(expand = c(0,0),limits = c(0,1))+
#   theme(text = element_text(size = 8))

#base network towers
#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.base.csv')

base.towers            = subset(tower.data,tower.data$active == 'active' &
                                  tower.data$Start_CO2 < 2022)

methane.towers         = subset(tower.data,tower.data$active == 'active' &
                                  tower.data$Start_CO2 < 2022 &
                                  tower.data$methane == 'methane')

annual.towers          = subset(tower.data,tower.data$active == 'active' &
                                  tower.data$Start_CO2 < 2022 &
                                  tower.data$Season_Activity == 'All year')

annual.methane.towers  = subset(tower.data,tower.data$active == 'active' &
                                  tower.data$Start_CO2 < 2022 &
                                  tower.data$Season_Activity == 'All year' &
                                  tower.data$methane == 'methane')

#improved network
tower.data.i = fread(file = './data/pca.towers.upgraded.csv')

base.towers.i            = subset(tower.data.i,tower.data.i$active == 'active')

methane.towers.i         = subset(tower.data.i,tower.data.i$active == 'active' &
                                    tower.data.i$methane == 'methane')

annual.towers.i          = subset(tower.data.i,tower.data.i$active == 'active' &
                                    tower.data.i$Season_Activity == 'All year')

annual.methane.towers.i  = subset(tower.data.i,tower.data.i$active == 'active' &
                                    tower.data.i$Season_Activity == 'All year' &
                                    tower.data.i$methane == 'methane')

RColorBrewer::brewer.pal

pal = RColorBrewer::brewer.pal(n = 11,name = 'PuOr')


install.packages("ggblend")
remotes::install_github("mjskay/ggblend")


pal = palette(pal,inverse=T)
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
             col='black',show.legend = F,cex = 3)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','lightblue'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CO[2]),size=2)
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
