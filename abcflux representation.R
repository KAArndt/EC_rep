
library(ggplot2)
library(ggspatial)
library(terra)
library(sf)
library(data.table)
library(cowplot)
library(ggnewscale)

abc = fread('./data/ABCFlux.forKyle.csv')
abc = subset(abc,abc$flux_method == 'EC')

towers = fread('./data/ARGO_EC_Tower_edited.csv')

#towers = subset(towers,complete.cases(towers$active))



ggplot()+
  geom_point(data = towers,aes(Longitude,Latitude,col = 'towers'),cex=3)+
  geom_point(data = abc,aes(longitude,latitude,col = 'ABC'))



active  = subset(towers,towers$active == 'active')
abc2    = subset(abc,as.numeric(abc$end_year) >= 2022)
abc3    = subset(abc,as.numeric(abc$end_year) >= 2024)


ggplot()+
  geom_point(data = active,aes(Longitude,Latitude,col = 'towers'),cex=3)+
  geom_point(data = abc2,aes(longitude,latitude,col = 'ABC 2022'))+
  geom_point(data = abc3,aes(longitude,latitude,col = 'ABC 2024'))


#merge by coordinates
dec = 2

towers$lat = round(towers$Latitude,digits = dec)
towers$lon = round(towers$Longitude,digits = dec)

abc$lat = round(abc$latitude,digits = dec)
abc$lon = round(abc$longitude,digits = dec)
abc$lat
abc$lon

names(abc)[1] = 'site'

m = merge(x = towers,y = abc,by = 'site',all=T)
m
write.csv(x = m,file = './data/merged.csv',row.names = F)



base = rast(x = './output/base_network/base_2km.tif')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


ggplot()+
  geom_point(data = towers,aes(Longitude,Latitude,col = 'towers'),cex=3)+
  geom_point(data = abc,aes(longitude,latitude,col = 'ABC'))


