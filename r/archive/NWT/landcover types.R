
library(data.table)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggnewscale)
library(readr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')
tower.data$active = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active)

new.sites = subset(tower.data,tower.data$site == "Lutose" |
                     tower.data$site == "Scotty Creek Landscape" |
                     tower.data$site == "Steen River" |
                     tower.data$site == "Kangiqsuallujjuaq" |
                     tower.data$site == "Scotty Creek Bog" |
                     tower.data$site == "Resolute Bay" |
                     tower.data$site == "Smith Creek" |
                     tower.data$site == "Iqaluit (PP)" |
                     tower.data$site == "Pond Inlet (PP)" |
                     tower.data$site == "Churchill Fen" |
                     tower.data$site == "Council (Permafrost Pathways)" |
                     tower.data$site == "Cambridge Bay, Victoria Island, mesic" |
                     tower.data$site == "Cambridge Bay, Victoria Island, wetland" |
                     tower.data$site == "Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Burned 2015" |
                     tower.data$site == "Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Unburned" |
                     tower.data$site == "Chersky, drained" |
                     tower.data$site == "Chersky, control")

#NWT maps
states = readRDS(file = './spatial_data/states.rds') #load in states sf file
esa = rast('./spatial_data/ESA_landcover.tif') #load in esa land cover

states = st_transform(x = states,crs = crs(esa)) #transform the states file to lat-lon system
can = subset(states,states$admin == 'Canada') #subset the states file to canada

tower.s = st_as_sf(x = tower.data,coords = c("Longitude",'Latitude'))
tower.s = st_set_crs(x = tower.s,value = crs(can))

join = st_join(x = tower.s,y = can)

sites = which(join$name == "Northwest Territories")
nwt.sites = subset(join,join$name == "Northwest Territories")

esa.can = crop(x = esa,y = ext(can)) #crop the esa file to the canadian extent
esa.ag  = aggregate(esa.can,fact = 4,fun = 'modal',cores=12,verbose=T)


nwt = subset(can,can$name == "Northwest Territories")
msk = ifelse(can$name == 'Northwest Territories', 1, NA)

nwt.esa = crop(x = esa.ag,y = nwt)
nwt.esa = mask(x = nwt.esa,mask = nwt)


plot(nwt.esa)

esa.sites = extract(x = nwt.esa,y = nwt.sites)

esa.sites$landcover = ifelse(esa.sites$ESA_landcover == 150, "Sparse vegetation (tree shrub herbaceous cover)",
                             esa.sites$ESA_landcover == 140, "Lichens and mosses",
                             esa.sites$ESA_landcover == 210, "Water Bodies",

                             esa.sites$ESA_landcover == 100, "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
                             esa.sites$ESA_landcover == 140, "Lichens and mosses",
                             esa.sites$ESA_landcover == 140, "Lichens and mosses",
)


ggplot(data = esa.sites)+
  geom_bar(aes(ESA_landcover))
esa.sites

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(rep))


#NWT maps ###########################################################################################

plot(rep.can)

nwt = subset(can,can$name == "Northwest Territories")
msk = ifelse(can$name == 'Northwest Territories', 1, NA)

nwt.rep = crop(x = rep.can,y = nwt)
nwt.rep = mask(x = nwt.rep,mask = nwt)

nwt.sites = subset(join,join$name == "Northwest Territories" & join$active == 'active')

#plot
#base
ggplot()+theme_map()+
#  geom_sf(data = can,fill='gray',col='gray40')+
  layer_spatial(nwt.rep)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_sf(data = nwt,fill='transparent',col='black')+
  geom_sf(data = nwt.sites,aes(fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,size=2)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  theme(text = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0,0.05),
        legend.title.position = 'top')
