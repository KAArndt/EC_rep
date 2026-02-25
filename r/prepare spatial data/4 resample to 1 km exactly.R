
library(terra)
library(data.table)
library(ggspatial)
library(ggplot2)
library(sf)


lc = rast('./spatial_data/HybridLandCover_1km.tif')

#load in the permafrost layer
r = rast('./spatial_data/spatial_repro_new.tif')

rr = resample(x = r,y = lc)


plot(rr$MeanTemp)

#ecoregions2017
eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')

eco = subset(eco,eco$BIOME_NAME == 'Rock and Ice' |
               eco$BIOME_NAME   == 'Tundra' |
               eco$BIOME_NAME   == 'Boreal Forests/Taiga')
               # eco$ECO_NAME     == 'Sayan alpine meadows and tundra' |
               # eco$ECO_NAME     == 'Sayan montane conifer forests' |
               # eco$ECO_NAME     == 'South Siberian forest steppe' |
               # eco$ECO_NAME     == 'Western Siberian hemiboreal forests' |
               # eco$ECO_NAME     == 'Da Hinggan-Dzhagdy Mountains conifer forests' |
               # eco$ECO_NAME     == 'Daurian forest steppe' |
               # eco$ECO_NAME     == 'Eastern Canadian Forest-Boreal transition' |
               # eco$ECO_NAME     ==  'Alberta-British Columbia foothills forests' |
               # eco$ECO_NAME     ==  'Sayan Intermontane steppe')


#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))
eco = project(x = eco,y = rr)
plot(eco)

rr = crop(x = rr,y = eco)
rr = mask(x = rr,mask = eco)

plot(rr$)
pp.r

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 40, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(pp.r))

pp.ag = aggregate(x = pp.r,fact = 4,fun = mean,na.rm = T)

ggplot()+
  layer_spatial(data = pp.ag)+
  layer_spatial(data = eco,fill='transparent',col='red')+
  geom_sf(data = countries,fill='transparent',col='black')+
  scale_color_viridis_c(na.value = 'transparent')




writeRaster(x = final,filename = './spatial_data/spatial_repro_new.tif',overwrite = T)
