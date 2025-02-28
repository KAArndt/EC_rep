
library(terra)
library(data.table)
library(ggspatial)
library(ggplot2)
library(cowplot)
library(sf)
library(dplyr)
library(Polychrome)

sites = fread('./data/pca.towers.base.csv')

#load in the permafrost layer
pp = rast('./spatial_data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')

#base Extrapolation index image from TNC shapefile
eco = vect('./spatial_data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

# #subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' |
               eco$WWF_MHTNAM   == 'Tundra' |
               eco$WWF_MHTNAM   == 'Boreal Forests/Taiga'
               # eco$ECO_NAME     == 'Sayan Alpine Meadows And Tundra' |
               # eco$ECO_NAME     == 'Sayan Montane Conifer Forests' |
               # eco$ECO_NAME     == 'South Siberian Forest Steppe' |
               # eco$ECO_NAME     == 'Western Siberian Hemiboreal Forests' |
               # eco$ECO_NAME     == 'Da Hinggan-Dzhagdy Mountains Conifer Forests' |
               # eco$ECO_NAME     == 'Daurian Forest Steppe' |
               # eco$ECO_NAME     == 'Eastern Canadian Forest-Boreal Transition' |
               # eco$ECO_NAME     ==  'Alberta-British Columbia Foothills Forests' |
               # eco$ECO_NAME     ==  'Sayan Intermontane Steppe'
             )

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))
eco = project(x = eco,y = pp)
plot(eco)

#clusters
km = rast('./output/clusts.tif')

km40 = km$km40

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 40, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(km40))

#aggregate the km for plotting and ease, use modal aggregation since it's a category
km.ag = aggregate(x = km40,fact = 4,fun = 'modal',na.rm = T)


#cluster and Ecoregions figure
kmdf = as.data.frame(km.ag,xy=T,na.rm=T)

#colors
library(RColorBrewer)
n = 40
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_pals = qual_col_pals[c(2,4,6,7,8),]

col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pal =sample(col_vector, n)

png(filename = './figures/Figure sxx total cluster_map.png',width = 4,height = 3.2,units = 'in',res = 1500)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  geom_raster(data = kmdf,aes(x,y,fill=factor(km40)))+
  scale_fill_manual('Cluster',values = pal)+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='black')+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(x = 0.05,units = 'in'))
dev.off()


#separate clusters
p1 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[1],limits = c(1,1))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p2 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[2],limits = c(2,2))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p3 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[3],limits = c(3,3))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p4 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[4],limits = c(4,4))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p5 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[5],limits = c(5,5))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p6 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[6],limits = c(6,6))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p7 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[7],limits = c(7,7))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p8 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[8],limits = c(8,8))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p9 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[9],limits = c(9,9))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p10 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[10],limits = c(10,10))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p11 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[11],limits = c(11,11))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p12 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[12],limits = c(12,12))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p13 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[13],limits = c(13,13))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p14 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[14],limits = c(14,14))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))


p15 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[15],limits = c(15,15))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p16 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[16],limits = c(16,16))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p17 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[17],limits = c(17,17))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p18 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[18],limits = c(18,18))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p19 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[19],limits = c(19,19))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p20 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[20],limits = c(20,20))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p21 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[21],limits = c(21,21))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p22 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[22],limits = c(22,22))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p23 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[23],limits = c(23,23))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p24 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[24],limits = c(24,24))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p25 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[25],limits = c(25,25))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p26 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[26],limits = c(26,26))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p27 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[27],limits = c(27,27))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p28 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[28],limits = c(28,28))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p29 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[29],limits = c(29,29))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p30 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[30],limits = c(30,30))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p31 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[31],limits = c(31,31))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p32 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[32],limits = c(32,32))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p33 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[33],limits = c(33,33))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p34 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[34],limits = c(34,34))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p35 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[35],limits = c(35,35))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p36 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[36],limits = c(36,36))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p37 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[37],limits = c(37,37))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p38 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[38],limits = c(38,38))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p39 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[39],limits = c(39,39))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

p40 = ggplot()+theme_map()+
  geom_sf(data = countries,fill='black',col='black')+
  layer_spatial(km.ag$km40)+
  scale_fill_gradientn(na.value = NA,'',colors = pal[40],limits = c(40,40))+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  layer_spatial(data = eco,fill='transparent',col='white')+
  theme(legend.position = c(0.1,0.9),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 8),
        legend.key.height = unit(x = 0.02,units = 'in'),
        legend.key.width = unit(x = 0.03,units = 'in'))

png('./figures/separate_clusters1-8.png',width = 5,height=9,res = 1500,units = 'in')
plot_grid(p1,p2,
          p3,p4,
          p5,p6,
          p7,p8,
          nrow = 4)
dev.off()

png('./figures/separate_clusters9-16.png',width = 5,height=9,res = 1500,units = 'in')
plot_grid(p9,p10,
          p11,p12,
          p13,p14,
          p15,p16,
          nrow = 4)
dev.off()

png('./figures/separate_clusters17-24.png',width = 5,height=9,res = 1500,units = 'in')
plot_grid(p17,p18,
          p19,p20,
          p21,p22,
          p23,p24,
          nrow = 4)
dev.off()

png('./figures/separate_clusters25-32.png',width = 5,height=9,res = 1500,units = 'in')
plot_grid(p25,p26,
          p27,p28,
          p29,p30,
          p31,p32,
          nrow = 4)
dev.off()

png('./figures/separate_clusters33-40.png',width = 5,height=9,res = 1500,units = 'in')
plot_grid(p33,p34,
          p35,p36,
          p37,p38,
          p39,p40,
          nrow = 4)
dev.off()
