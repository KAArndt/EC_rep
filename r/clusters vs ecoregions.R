
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


km.df = as.data.frame(km40,xy=T,na.rm=T)
km.df = as.data.frame(eco,xy=T,na.rm=T)

kmex = extract(x = km.ag,y = eco,xy=T,na.rm=T)

kmex = kmex[complete.cases(kmex$ID),]
kmex = kmex[complete.cases(kmex$km40),]

kmex$kmchar = as.character(kmex$km40)

ggplot(data = kmex)+
  geom_bar(aes(kmchar,fill=ID))

ecoregions = eco$ECO_NAME
ecoregions = as.data.frame(ecoregions)
ecoregions$ID = 1:49

kmex = merge(kmex,ecoregions,by = 'ID',all = T)

kmex$one = 1

kms = kmex %>%
  group_by(ID,km40) %>%
  summarise(sums = sum(one))

totals = kmex %>%
  group_by(ID) %>%
  summarise(total = sum(one))

kms = merge(kms,totals,by = 'ID',all=T)
kms = merge(kms,ecoregions,by = 'ID',all = T)

kms$percent = kms$sums/kms$total*100

bars = ggplot(data = kms)+facet_wrap(~ecoregions)+
  geom_bar(aes(km40,percent,fill=(km40)),stat = 'identity')+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.3))
bars


map = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='black')+
  layer_spatial(data = km.ag)+
  scale_fill_viridis_c(na.value = 'transparent')+
  layer_spatial(data = eco,fill='transparent',col='red')
map
plot_grid(map,bars)
eco$ECO_NAME
write.csv(kms,file = './output/kms.csv')

base = rast('./output/base_2kmv2_mean.tif')


plot(basekm)

base.ag = aggregate(x = base,fact = 4,fun = 'mean',na.rm = T)
base.ag
km.ag
basekm = c(base.ag,km.ag)

plot(basekm)

rep.df = as.data.frame(basekm,na.rm=T)
rep.ex = extract(basekm,eco,na.rm=T)

rep.ex = merge(rep.ex,ecoregions,by = 'ID')

ggplot(data = rep.ex)+
  geom_boxplot(aes(as.character(km40),base.dist))

ggplot(data = rep.ex)+
  geom_boxplot(aes(as.character(ecoregions),base.dist))+
  theme(axis.text.x = element_text(angle = 90))
