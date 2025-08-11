library(terra)
library(plyr)

#load in all data
eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')
towers = read.csv('./data/pca.towers.upgraded.csv')
towers = read.csv('./data/ARGO_EC_Tower_edited.csv')

eco = subset(eco,eco$BIOME_NAME == 'Rock and Ice' |
               eco$BIOME_NAME   == 'Tundra' |
               eco$BIOME_NAME   == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 40, 83.6236))

#subset to active towers
towers = vect(x = towers,geom=c("Longitude", "Latitude"), crs=crs(eco))

#create a buffer from current tower sites
buff = buffer(x = towers,width = 100000)

#load in the places
places = vect('./spatial_data/ne_10m_populated_places/ne_10m_populated_places.shp')

#load in science stations
ss = read.csv(file = './data/interact_ecactive_representativeness.csv')
ss = vect(x = ss,geom=c('Longitude..west.from.Greenwhich.i.e..W....negative.longitude.','Latitude'),crs=crs(eco))

#subset to abz
places = mask(x = places,mask = eco)
ss = mask(x = ss,mask = eco)

#remove within x distance of towers
places.f = mask(x = places,mask = buff,inverse=T)
ss.f = mask(x = ss,mask = buff,inverse=T)

length(places)
length(places.f)

length(ss)
length(ss.f)

plot(eco)
points(buff,col='red',cex=0.2)
points(places.f,col='blue')
points(ss.f,col='green')

placesdf = as.data.frame(places.f,row.names = F,xy=T)
placesdf = placesdf[,c('NAME','SOV0NAME','ADM1NAME','LATITUDE','LONGITUDE','POP_MAX')]

ssdf = as.data.frame(ss.f,row.names = F,geom='XY')
ssdf = ssdf[,c('Sub.Category','X.1','x','y')]
names(ssdf) = c('NAME','SOV0NAME','LONGITUDE','LATITUDE')

ssdf$TYPE     = 'Research_Station'
placesdf$TYPE = 'Populated_Place'

extension_sites = rbind.fill(ssdf,placesdf)


write.csv(x = extension_sites,file = './data/ext_sites.csv',row.names = F)



library(ggplot2)
library(ggspatial)


buff$active = ifelse(buff$End_CO2 == 2024,'active','inactive')

ggplot()+theme_bw()+
  layer_spatial(data = eco)+
  layer_spatial(data = buff,aes(col=active),fill='transparent')+
  layer_spatial(places.f)


places.f$NAME
  