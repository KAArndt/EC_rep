
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

#run rep based on just NWT
#load back in euclidean distance matrix
euci = read_rds('./euclidean_distance_matrix/euci_2kmv2.rds')

tower.s = st_as_sf(x = tower.data,coords = c("Longitude",'Latitude'))
tower.s = st_set_crs(x = tower.s,value = crs(can))

join = st_join(x = tower.s,y = can)


#load in the other spatial data
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,xy = T,na.rm = T)

##########################################################################
# BASE
net = which(join$name == 'Northwest Territories' & join$active == 'active')
tower.data$site[net]
euci.net = euci[,c(net)]

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    base.dist[i] = min(euci.net[i,])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
base.towers = tower.data[net,]
towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))

hist(base)
plot(base,range=c(0,4.5))
points(towers,col='red')

#save the base here
writeRaster(x = base,filename = './output/nwt_2kmv2_min.tif',overwrite = T)


#######################################################################################
base           = rast('./output/base_2kmv2_min.tif')
methane        = rast('./output/methane_2kmv2_min.tif')
annual         = rast('./output/annual_2kmv2_min.tif')
annual.methane = rast('./output/annual_methane_2kmv2_min.tif')

improved.base           = rast('./output/improved_base_2kmv2_min.tif')
improved.methane        = rast('./output/improved_methane_2kmv2_min.tif')
improved.annual         = rast('./output/improved_annual_2kmv2_min.tif')
improved.annual.methane = rast('./output/improved_annual_methane_2kmv2_min.tif')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))

# states = rnaturalearth::ne_states(returnclass = "sf") %>%
#   st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
#   smoothr::densify(max_distance = 1) %>%
#   st_transform(crs(base))

#create aggregates for the plots
base.ag           = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
methane.ag        = aggregate(x = methane,fact = 4,fun = mean,na.rm = T)
annual.ag         = aggregate(x = annual,fact = 4,fun = mean,na.rm = T)
annual.methane.ag = aggregate(x = annual.methane,fact = 4,fun = mean,na.rm = T)

improved.base.ag           = aggregate(x = improved.base,fact = 4,fun = mean,na.rm = T)
improved.methane.ag        = aggregate(x = improved.methane,fact = 4,fun = mean,na.rm = T)
improved.annual.ag         = aggregate(x = improved.annual,fact = 4,fun = mean,na.rm = T)
improved.annual.methane.ag = aggregate(x = improved.annual.methane,fact = 4,fun = mean,na.rm = T)

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

active.sites = subset(tower.data,tower.data$active == 'active')

#NWT maps ###########################################################################################
states = readRDS(file = './spatial_data/states.rds') #load in states sf file
esa = rast('./spatial_data/ESA_landcover.tif') #load in esa land cover
esa.ag = aggregate(x = esa,fun = 'modal',fact = 8,cores = 10,verbose=T) #aggregate to about a 2km size
states = st_transform(x = states,crs = crs(esa.ag)) #transform the states file to lat-lon system
can = subset(states,states$admin == 'Canada') #subset the states file to canada
esa.can = crop(x = esa.ag,y = ext(can)) #crop the esa file to the canadian extent
rep.can = project(x = improved.base,y = esa.can) #crop and reduce the rep file to the area of the esa plot
rep.can.2 = project(x = improved.annual.methane,y = esa.can) #crop and reduce the rep file to the area of the esa plot


can.sites = subset(active.sites,active.sites$Country == 'Canada')
new.sites = subset(new.sites,new.sites$Country == 'Canada')

#plot
#base
ggplot()+theme_map()+
  geom_sf(data = can,fill='gray',col='gray40')+
  layer_spatial(rep.can)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_sf(data = can,fill='transparent',col='black')+
  geom_point(data = can.sites,aes(Longitude,Latitude,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,size=2)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(Longitude,Latitude),col='black',fill='yellow',pch = 21,show.legend = F,size=2)+
  # scale_x_continuous(limits = c(-5093909,4542996))+
  # scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

#annual methane
ggplot()+theme_map()+
  geom_sf(data = can,fill='gray',col='gray40')+
  layer_spatial(rep.can.2)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_sf(data = can,fill='transparent',col='black')+
  geom_point(data = can.sites,aes(Longitude,Latitude,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,size=2)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(Longitude,Latitude),col='black',fill='yellow',pch = 21,show.legend = F,size=2)+
  # scale_x_continuous(limits = c(-5093909,4542996))+
  # scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')



#crocan#crop ESA to near area of intecancor()#crocan#crop ESA to near area of interest to save processing time on later steps
esa = crop(x = esa,y = ext(can)) #crop
esa.can = mask(x = esa)

plot(can)
esa.can = 

#plot to check out
plot(esa)

#states
ggplot()+theme_map()+
  geom_sf(data = states,fill='gray',col='gray40')+
  layer_spatial(improved.base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.53*2),
                       breaks = c(0,1.53,1.53*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_sf(data = states,fill='transparent',col='black')+
  geom_point(data = active,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F,size=2)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green'))+
  geom_point(data = new.sites,aes(x,y),col='black',fill='yellow',pch = 21,show.legend = F,size=2)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')+
  annotate(geom = 'text',x = -3093909,y = 3374170,label = expression("Summer"~CO[2]))


#fire map ########################################################################
nee = rast('./spatial_data/CO2Fluxes_Arctic_Boreal_NEEfire_2002_2020_avg.tif')
nee = aggregate(x = nee,fact = 4,fun = mean,na.rm = T)
nee = project(x = nee,y = crs(base.ag))
hist(nee)

pal =  c("#053061" ,"#2166AC" ,"#4393C3", "#92C5DE" ,"#D1E5F0" ,"#F7F7F7" ,"#FDDBC7" ,"#F4A582" ,"#D6604D" ,"#B2182B" ,"#67001F")
pal =  c("#053061" ,"#2166AC" ,"#4393C3", "#92C5DE"  ,"#F7F7F7" ,"#F4A582" ,"#D6604D" ,"#B2182B" ,"#67001F")

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(nee)+
  scale_fill_gradientn('Carbon Exchange',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-200,200),
                       breaks = c(-200,0,200),
                       labels = c('Sink','Neutral','Source'),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')


# NWT ############################################################
#Canada map for plotting
canada_map = readRDS('./spatial_data/canada.rds')
nee = project(x = base.ag,y = crs(canada_map))

plot(nee)
plot(canada_map)

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(nee)+
  scale_fill_gradientn('Carbon Exchange',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-200,200),
                       breaks = c(-200,0,200),
                       labels = c('Sink','Neutral','Source'),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

crs(canada_map)
