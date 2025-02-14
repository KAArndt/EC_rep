
library(terra)
library(data.table)
library(ggspatial)
library(ggplot2)
library(sf)

ext = fread('./data/extracted_tower_data.csv')
mong = subset(ext,ext$Country == "Mongolia")

#load in the permafrost layer
pp = rast('./spatial_data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')

#base Extrapolation index image from TNC shapefile
# eco = vect('./spatial_data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')
# 
# #subset to rock and ice and tundra and boreal
# eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
#                eco$WWF_MHTNAM   == 'Tundra' |
#                eco$WWF_MHTNAM   == 'Boreal Forests/Taiga' |
#                eco$ECO_NAME     == 'Sayan Alpine Meadows And Tundra' |
#                eco$ECO_NAME     == 'Sayan Montane Conifer Forests' |
#                eco$ECO_NAME     == 'South Siberian Forest Steppe' |
#                eco$ECO_NAME     == 'Western Siberian Hemiboreal Forests' |
#                eco$ECO_NAME     == 'Da Hinggan-Dzhagdy Mountains Conifer Forests' |
#                eco$ECO_NAME     == 'Daurian Forest Steppe' |
#                eco$ECO_NAME     == 'Eastern Canadian Forest-Boreal Transition' |
#                eco$ECO_NAME     ==  'Alberta-British Columbia Foothills Forests' |
#                eco$ECO_NAME     ==  'Sayan Intermontane Steppe'
#              )

#ecoregions2017
eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')

eco = subset(eco,eco$BIOME_NAME == 'Rock and Ice' | 
               eco$BIOME_NAME   == 'Tundra' |
               eco$BIOME_NAME   == 'Boreal Forests/Taiga' |
               eco$ECO_NAME     == 'Sayan alpine meadows and tundra' |
               eco$ECO_NAME     == 'Sayan montane conifer forests' |
               eco$ECO_NAME     == 'South Siberian forest steppe' |
               eco$ECO_NAME     == 'Western Siberian hemiboreal forests' |
               eco$ECO_NAME     == 'Da Hinggan-Dzhagdy Mountains conifer forests' |
               eco$ECO_NAME     == 'Daurian forest steppe' |
               eco$ECO_NAME     == 'Eastern Canadian Forest-Boreal transition' |
               eco$ECO_NAME     ==  'Alberta-British Columbia foothills forests' |
               eco$ECO_NAME     ==  'Sayan Intermontane steppe')


#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))
eco = project(x = eco,y = pp)
plot(eco)

pp = crop(x = pp,y = eco)
pp = mask(x = pp,mask = eco)

plot(pp)
points(mong$x,mong$y,pch=16)

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 40, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(pp))


pp.ag = aggregate(x = pp,fact = 4,fun = mean,na.rm = T)

ggplot()+
  layer_spatial(data = pp.ag)+
  layer_spatial(data = eco,fill='transparent',col='red')+
  geom_sf(data = countries,fill='transparent',col='black')+
  scale_color_viridis_c(na.value = 'transparent')+
  geom_point(data = mong,aes(x = x,y = y),col='red')



#load in the differeneconomics#load in the different data files
clim  = rast('./spatial_data/climate.tif')
clim = subset(clim,subset = -c(2,3,5,6,8:11,15,16,18,19)) #subset down to layers we want to save space
modis = rast('./spatial_data/modis.tif')
soil  = rast('./spatial_data/soils.tif')

#merge them all together
#r = c(clim,modis,soil)

#test with single layers, may be faster and simpler
#climate
clim2 = project(x = clim$MeanTemp,                y = pp)
clim3 = project(x = clim$Precip,                  y = pp)
clim4 = project(x = clim$PrecipitationSeasonality,y = pp)
clim5 = project(x = clim$MeanDiurnalRange,        y = pp)
clim6 = project(x = clim$Isothermality,           y = pp)
clim7 = project(x = clim$TempSeasonality,         y = pp)
clim8 = project(x = clim$TempAnnualRange,         y = pp)

#modis
modis2 = project(x = modis$ndvimax,y = pp)
modis3 = project(x = modis$ndvisum,y = pp)
modis4 = project(x = modis$evimax, y = pp)
modis5 = project(x = modis$ndwimin,y = pp)
modis6 = project(x = modis$mirsaug,y = pp)

#soil grids
soil2 = project(x = soil$bd_100_agg,           y = pp)
soil3 = project(x = soil$ph0_100,              y = pp)
soil4 = project(x = soil$OCSTHA_M_100cm_1km_ll,y = pp)
soil5 = project(x = soil$soc0_100,             y = pp)
soil6 = project(x = soil$sand_100_agg,         y = pp)
soil7 = project(x = soil$silt_100_agg,         y = pp)
soil8 = project(x = soil$clay_100_agg,         y = pp)

final.clim  = c(clim2,clim3,clim4,clim5,clim6,clim7,clim8)
final.modis = c(modis2,modis3,modis4,modis5,modis6)
final.soil  = c(soil2,soil3,soil4,soil5,soil6,soil7,soil8)

#crop and mask the stacks
#eco2 = subset(eco,eco$WWF_MHTNAM != 'Rock and Ice') #TNC
eco2 = subset(eco,eco$BIOME_NAME != 'Rock and Ice') #Ecoregions2017

final.clim2 = crop(x = final.clim,y = eco2)
final.clim2 = mask(x = final.clim2,mask = eco2)

final.modis2 = crop(x = final.modis,y = eco2)
final.modis2 = mask(x = final.modis2,mask = eco2)

final.soil2 = crop(x = final.soil,y = eco2)
final.soil2 = mask(x = final.soil2,mask = eco2)

final = c(final.clim2,final.modis2,final.soil2,pp)

#mask out where any layer is NA, using one layer from each data set
final[is.na(final$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)] = NA #permafrost
final[is.na(final$OCSTHA_M_100cm_1km_ll)] = NA #soil grids
final[is.na(final$MeanTemp)] = NA #worldclim
final[is.na(final$ndwimin)] = NA #MODIS
final[is.na(final$mirsaug)] = NA #MODIS

writeRaster(x = final,filename = './spatial_data/spatial_repro_extended.tif',overwrite = T)
