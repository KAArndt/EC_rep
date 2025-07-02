
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

states = readRDS(file = './spatial_data/states.rds') #load in states sf file
can = subset(states,states$admin == 'Canada') #subset the states file to canada

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
writeRaster(x = base,filename = './r/NWT/nwt_2kmv2_min.tif',overwrite = T)
