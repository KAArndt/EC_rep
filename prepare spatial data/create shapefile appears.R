rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(sp)

#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

eco
#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

e = as(raster::extent(-180, 180, 43, 83.6236), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e)
e = vect(e)
e

writeVector(x = e,filename = './data/input data/appears.shp')