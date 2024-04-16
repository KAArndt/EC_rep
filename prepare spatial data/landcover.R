rm(list = ls())

library(data.table)
library(raster)
library(terra)
library(ncdf4)
library(ggplot2)

#base Extrapolation index image from martijn, use as domain mask
base = raster('./summean_v5_base.tif')

#landcover from MODIS################################
lcn = nc_open(filename = './data/input data/ei inputs/MCD12Q1plusC4_fraction.GLOBAL01KM.2001001.LC.10KM.nc')
lcn

lon <- ncvar_get(lcn, "lon")
lat <- ncvar_get(lcn, "lat", verbose = F)

lc.array <- ncvar_get(lcn, "MCD12Q1plusC4_fraction") # store the data in a 3-dimensional array
lc = raster(t(lc.array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

#load ESA landcover
esa = raster('./data/input data/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')

#crop and reproject both to the same base image
esa = crop(x = esa,y = extent(base))
lc = crop(lc,extent(base))

#esa = projectRaster(from = esa,to = base,method = 'ngb')
#lc  = projectRaster(from = lc,to = base,method = 'ngb')

#stack and plot both
plot(esa)
plot(lc)

#load in tower data
tower.data = fread(file = './data/extracted_tower_data.csv')

#set just the coordinates for the extract
xy.tower = tower.data[,c(24,25)]

#extract landcover data from the datasets
tower.esa = extract(x = esa,y = xy.tower)
tower.mod = extract(x = lc,y = xy.tower)

#merge into the tower data dataframe
tower.data$esa = tower.esa
tower.data$mod = tower.mod

#add names for land cover types
esa.classes = fread('./data/input data/ESACCI-LC-Legend-edit.csv')
names(esa.classes)[1] = 'esa'
names(esa.classes)[2] = 'esa.class'

tower.data = merge(tower.data,esa.classes,by = 'esa',all.x = T)

active = subset(tower.data,tower.data$active == 'active')

ggplot(data = active)+theme_bw()+
  geom_bar(aes(y = esa.class),stat = 'count')+
  scale_x_continuous(limits = c(0,35),expand = c(0,0))

ggplot(data = active)+theme_bw()+
  geom_point(aes(MeanTemp,OCSTHA_M_100cm_1km_ll,col = esa.class,label = sitename),
             size=2,label.size = NA,fill = 'transparent')+
  scale_y_continuous('SOC Stock (t/ha)')+
  scale_x_continuous('Mean Temp (deg. C)')

ggplot(data = active)+theme_bw()+
  geom_label(aes(MeanTemp,OCSTHA_M_100cm_1km_ll,col = esa.class,label = sitename),
             size=3,label.size = NA,fill = 'transparent',position = position_jitter(width = 1,height = 10))+
  scale_y_continuous('SOC Stock (t/ha)')+
  scale_x_continuous('Mean Temp (deg. C)')



