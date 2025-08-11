library(sp)
library(terra)

#base Extrapolation index image from TNC shapefile
eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
               eco$WWF_MHTNAM == 'Tundra' |
               eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

e = as(raster::extent(-180, 180, 43, 83.6236), "SpatialPolygons")

e = as(raster::extent(-180, 180, 43, 83.6236), "SpatialPolygons")
proj4string(e) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e)
e = vect(e)

writeVector(x = e,filename = './spatial_data/shapes/abx_all.shp')



e_180_120 = as(raster::extent(-180, -120, 43, 83.6236), "SpatialPolygons")
proj4string(e_180_120) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e_180_120)
e_180_120 = vect(e_180_120)

writeVector(x = e_180_120,filename = './spatial_data/shapes/abx_180_120.shp')




e_120_60 = as(raster::extent(-120, -60, 43, 83.6236), "SpatialPolygons")
proj4string(e_120_60) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e_120_60)
e_120_60 = vect(e_120_60)

writeVector(x = e_120_60,filename = './spatial_data/shapes/abx_120_60.shp')


10^12

e_60_0 = as(raster::extent(-60, 0, 43, 83.6236), "SpatialPolygons")
proj4string(e_60_0) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e_60_0)
e_60_0 = vect(e_60_0)

writeVector(x = e_60_0,filename = './spatial_data/shapes/abx_60_0.shp')




e_0_60 = as(raster::extent(0, 60, 43, 83.6236), "SpatialPolygons")
proj4string(e_0_60) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e_0_60)
e_0_60 = vect(e_0_60)

writeVector(x = e_0_60,filename = './spatial_data/shapes/abx_0_60.shp')



e_120_180 = as(raster::extent(120, 180, 43, 83.6236), "SpatialPolygons")
proj4string(e_120_180) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e_120_180)
e_120_180 = vect(e_120_180)

writeVector(x = e_120_180,filename = './spatial_data/shapes/abx_120_180.shp')
