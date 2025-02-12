
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggnewscale)

#load in NEE data
nee.f    = rast('./spatial_data/CO2Fluxes_Arctic_Boreal_NEEfire_2002_2020_avg.tif')
nee      = rast('./spatial_data/CO2Fluxes_Arctic_Boreal_NEE_2002_2020_avg.tif')

#load in spatial shape data
states = readRDS(file = './spatial_data/states.rds') #load in states sf file
states = st_transform(x = states,crs = crs(nee.f))
can    = subset(states,states$admin == 'Canada')
nwt    = subset(states,states$name == 'Northwest Territories')

#cut to Canadian and NWT extents #######################################
# WITH FIRE
#load in the NEE and crop and mask to Canada and NWT
nee.f.can = crop(x = nee.f,y = ext(can))
nee.f.can = mask(x = nee.f.can,mask = can)

nee.f.nwt = crop(x = nee.f,y = ext(nwt))
nee.f.nwt = mask(x = nee.f.nwt,mask = nwt)

#Calculate Canada budget
nee.f.can.df = as.data.frame(nee.f.can,xy=T)
nee.f.can.df$carbonflux = nee.f.can.df$mean*999.9711
sum(nee.f.can.df$carbonflux)/10^9

#Calculate NWT budget
nee.f.nwt.df = as.data.frame(nee.f.nwt,xy=T)
nee.f.nwt.df$carbonflux = nee.f.nwt.df$mean*999.9711
sum(nee.f.nwt.df$carbonflux)/10^9

# WITHOUT FIRE
#load in the NEE and crop and mask to Canada and NWT
nee.can = crop(x = nee,y = ext(can))
nee.can = mask(x = nee.can,mask = can)

nee.nwt = crop(x = nee,y = ext(nwt))
nee.nwt = mask(x = nee.nwt,mask = nwt)

#Calculate Canada budget
nee.can.df = as.data.frame(nee.can,xy=T)
nee.can.df$carbonflux = nee.can.df$mean*999.9711
sum(nee.can.df$carbonflux)/10^9

#Calculate NWT budget
nee.nwt.df = as.data.frame(nee.nwt,xy=T)
nee.nwt.df$carbonflux = nee.nwt.df$mean*999.9711
sum(nee.nwt.df$carbonflux)/10^9

# PLOTS #######################################################################
#load in ESA land cover layer for the projection
esa = rast('./spatial_data/ESA_landcover.tif') #load in ESA land cover
can.ll = st_transform(x = can,crs = crs(esa)) #t

#crop and mask the landcover layer to Canada
esa.can = crop(x = esa,y = can.ll)
esa.can = mask(x = esa.can,mask = can.ll)

#aggregate to match the scale of the other images
esa.ag = aggregate(x = esa.can,fun = 'modal',fact = 8,cores = 10,verbose=T) #aggregate to about a 2km size

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(nee.can))

#fire map ########################################################################
pal =  c("#053061" ,"#2166AC" ,"#4393C3", "#92C5DE" ,"#D1E5F0" ,"#F7F7F7" ,"#FDDBC7" ,"#F4A582" ,"#D6604D" ,"#B2182B" ,"#67001F")
#pal =  c("#053061" ,"#2166AC" , "#92C5DE" ,"#D1E5F0" ,"#F7F7F7" ,"#FDDBC7" ,"#F4A582"  ,"#B2182B" ,"#67001F")

#aggregate plots for timing of plots
nee.can.ag = aggregate(x = nee.can,fact = 4,fun='mean',na.rm=T)

nee.ll = project(x = nee.can,y = esa.ag)
nee.ll.ag = aggregate(x = nee.ll,fact = 4,fun='mean',na.rm=T)

nee.f.ll = project(x = nee.f.can,y = esa.ag)
nee.f.ll.ag = aggregate(x = nee.f.ll,fact = 4,fun='mean',na.rm=T)

#
ggplot()+theme_map()+
  geom_sf(data = can.ll,fill='gray',col='gray40')+
  layer_spatial(nee.ll.ag)+
  scale_fill_gradientn('Carbon Exchange',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-200,200),
                       breaks = c(-200,0,200),
                       labels = c('Sink','Neutral','Source'),
                       oob = scales::squish)+
  geom_sf(data = can.ll,fill='transparent',col='black')+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')


#fire
ggplot()+theme_map()+
  geom_sf(data = can.ll,fill='gray',col='gray40')+
  layer_spatial(nee.f.ll.ag)+
  scale_fill_gradientn('Carbon Exchange',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-200,200),
                       breaks = c(-200,0,200),
                       labels = c('Sink','Neutral','Source'),
                       oob = scales::squish)+
  geom_sf(data = can.ll,fill='transparent',col='black')+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')
