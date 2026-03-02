library(terra)
library(data.table)

lc = rast('./spatial_data/HybridLandCover_1km.tif')

#load in the permafrost layer
r = rast('./spatial_data/spatial_repro_new.tif')

rr = resample(x = r,y = lc)
rr

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
#rr = mask(x = rr,mask = eco)

writeRaster(x = rr,filename = './spatial_data/spatial_repro.tif',overwrite = T)
