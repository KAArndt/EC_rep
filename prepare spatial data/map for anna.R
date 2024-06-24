rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(raster)
#library(svMisc)
#library(MASS)
library(ggplot2)
library(ggspatial)
#library(plotrix)
library(terra)
#library(plyr)
library(data.table)
#library(kit)
library(ggthemes)
library(sf)
library(tidyterra)

#m = rast('./data/predictions_1km_raster_agg_NEE_gC_m2_2001_2020_avg.tif')
pp = rast('./data/input data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')

#background world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 30, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(pp))


#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))
eco = project(x = eco,y = pp)


#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
base.towers = subset(tower.data,tower.data$Activity == 'active' & tower.data$Annual_cover == 'annual')

ggplot()+theme_bw()+ggtitle('Annual Eddy Covariance Coverage')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  geom_spatvector(data = eco,aes(fill=WWF_MHTNAM),col='transparent')+
  geom_point(data = base.towers,aes(x,y),col='white',fill='black',pch=21,size=3)+
  scale_x_continuous(limits = c(-5194909,5194909),expand = c(0,0),'')+
  scale_y_continuous(limits = c(-4580235,4580235),expand = c(0,0),'')+
  scale_fill_manual(values = c('Boreal Forests/Taiga' = 'forestgreen',
                               'Rock and Ice'= 'white',
                               'Tundra' = 'yellow3'),
                    'Biome',
                    breaks = c('Boreal Forests/Taiga','Tundra'))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 8),
        panel.background = element_rect('lightblue1'),
        legend.position = c(0.87,0.09),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))

