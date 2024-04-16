rm(list = ls())

library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(plyr)
library(data.table)
library(kit)
library(raster)
library(sf)

#things needed for all the plots
library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

#world map at a coarse resolution, spatial polygons dataframe
library(rnaturalearth)
library(rnaturalearthdata)

world = ne_countries(scale = "medium", returnclass = "sf")

ggplot(world)+
  geom_sf()+
  scale_y_continuous(limits = c(45,80))


st_bbox(world)
#library(cleangeo)
#worldmap = getMap(resolution = 'coarse')
#worldmap = clgeo_Clean(worldmap)
crs(world)
st_crs(world)
st_make_valid()
box = c(xmin = -180, ymin = 45, xmax = 180, ymax = 80)
?st_crop
wm = st_crop(x = world,y = st_bbox(box,crs = st_crs(world)))
wm = st_crop(x = world,st_bbox(box))
wm = st_crop(x = world,box)
wm = st_transform(x = world,crs = proj)
box
ggplot(wm)+
  geom_sf()




#wm = spTransform(x = world,CRSobj = proj)
#wm = raster::crop(x = wm,y = extent(c(-4780235,4580235,-3880235,4020235))) #line crashes R, need to investigate

#plot(wm);abline(v = -4780235);abline(v=4580235);abline(h = -3880235);abline(h=4020235)
#load in the initial rep map without new pp towers
init = rast('./output/active/base_all_towers.tif')
#init.st = project(x = init,y = proj) #project to steridean for plots


ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(init$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       limits = c(0,3),
                       oob = scales::squish)


#load in extracted site data from extraction codes
tower.data = fread(file = './data/extracted_tower_data.csv')
towers = SpatialPointsDataFrame(coords = tower.data[,c(24,25)],data = tower.data) #convert to spatial points data frame

crs(towers) = crs(init) #assign a crs
towers.st = spTransform(x = towers,CRSobj = proj) #project to sterridean for plotting
towers.df = as.data.frame(towers.st)


#load in images with added sites, put this
plus1 = rast('./output/active/base1/base_all_towers.tif')
plus2 = rast('./output/active/base2/base_all_towers.tif')
plus3 = rast('./output/active/base3/base_all_towers.tif')
plus4 = rast('./output/active/base4/base_all_towers.tif')
plus5 = rast('./output/active/base5/base_all_towers.tif')
plus6 = rast('./output/active/base6/base_all_towers.tif')

#take the difference at this stage
dif1 = plus1 - init
dif2 = plus2 - plus1
dif3 = plus3 - plus2
dif4 = plus4 - plus3
dif5 = plus5 - plus4
dif6 = plus6 - plus5

#plot all differences
pdif1 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif1$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                   #    limits = c(0,3),
                       oob = scales::squish)

pdif2 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif2$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       #    limits = c(0,3),
                       oob = scales::squish)

pdif3 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif3$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       #    limits = c(0,3),
                       oob = scales::squish)

pdif4 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif4$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       #    limits = c(0,3),
                       oob = scales::squish)

pdif5 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif5$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       #    limits = c(0,3),
                       oob = scales::squish)

pdif6 = ggplot(world)+theme_minimal()+
  geom_sf()+
  layer_spatial(dif6$base_all_towers)+
  scale_y_continuous(limits = c(45,80),expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn('ED',na.value = 'transparent',
                       colours = pal,#trans = 'log',
                       #    limits = c(0,3),
                       oob = scales::squish)

pdif1 #resolute
pdif2 #kangiq
pdif3 #pond inlet
pdif4 #la grande
pdif5 #Kular
pdif6 #Batagai

terra::summary(init)

base.df  = terra::as.data.frame(x = init)
plus1.df = terra::as.data.frame(x = plus1)
plus2.df = terra::as.data.frame(x = plus2)
plus3.df = terra::as.data.frame(x = plus3)
plus4.df = terra::as.data.frame(x = plus4)
plus5.df = terra::as.data.frame(x = plus5)
plus6.df = terra::as.data.frame(x = plus6)

ggplot()+
  geom_density(data = base.df,aes(base_all_towers,fill='0'))+
  geom_density(data = plus1.df,aes(base_all_towers,fill='1'))+
  geom_density(data = plus2.df,aes(base_all_towers,fill='2'))+
  geom_density(data = plus3.df,aes(base_all_towers,fill='3'))+
  geom_density(data = plus4.df,aes(base_all_towers,fill='4'))+
  geom_density(data = plus5.df,aes(base_all_towers,fill='5'))+
  geom_density(data = plus5.df,aes(base_all_towers,fill='6'))+
  scale_x_continuous(expand = c(0,0))

base.df$id = 'base'
plus1.df$id = 'plus1'
plus2.df$id = 'plus2'
plus3.df$id = 'plus3'
plus4.df$id = 'plus4'
plus5.df$id = 'plus5'
plus6.df$id = 'plus6'

base.df$add = 0
plus1.df$add = 1
plus2.df$add = 2
plus3.df$add = 3
plus4.df$add = 4
plus5.df$add = 5
plus6.df$add = 6

#combine all into one data frame
df = rbind(base.df,plus1.df,plus2.df,plus3.df,plus4.df,plus5.df,plus6.df)

library(dplyr)


total = length(base.df$base_all_towers)
df$under = ifelse(test = df$base_all_towers > 2.1,yes = 1,no = 0)

stats = df %>%
  group_by(id) %>%
  summarise(mean = mean(base_all_towers),
            max  = max(base_all_towers),
            med  = median(base_all_towers),
            add  = mean(add),
            rep  = sum(under))


stats$perc = stats$rep/total*100

ggplot(data = stats)+
  geom_bar(aes(add,perc),stat = 'identity')+
  scale_y_continuous(limits = c(0,30),expand = c(0,0))

count(x = df$under)




