
library(data.table)
library(ggplot2)
library(terra)
library(sf)
library(dplyr)
library(ggspatial)
library(cowplot)
library(ggnewscale)

r  = rast('./spatial_data/soils.tif')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(r))

df = fread('./data/final.tower.data.csv')

#total active towers
a24 = subset(df,df$active.2024 == 'active')

a24 = a24[,c('site','Latitude','Longitude','Season_Activity.2024','methane.2024')]

toolik = a24

toolik = toolik[-c(2:149),]
toolik[1,1] = 'Toolik RTS'
toolik[1,2] = 68.527238
toolik[1,3] = -149.549063

df = rbind(a24,toolik)

df$pp   = ifelse(   #support
                    df$site == "Cambridge Bay, Victoria Island, mesic" |
                    df$site == "Cambridge Bay, Victoria Island, wetland" |
                    df$site == "Smith Creek" |
                    df$site == "Steen River" |
                    df$site == "Lutose" |
                    df$site == "Scotty Creek Bog" |
                    df$site == "Chersky, Pleistocene Park" |
                    df$site == "Chersky, control" |
                    df$site == "Chersky, drained" |
                    df$site == "Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Burned 2015" |
                    df$site == "Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Unburned" |
                      #new
                    df$site == "Churchill Fen" |
                    df$site == "Council (Permafrost Pathways)" |
                    df$site == "Iqaluit (PP)" |
                    df$site == "Kangiqsuallujjuaq" |
                    df$site == "Pond Inlet (PP)" |
                    df$site == "Scotty Creek Landscape" |
                    df$site == "Toolik RTS" |
                    df$site == "Resolute Bay", 'PP Supported','Other')

df$towersites = ifelse(df$pp == 'PP Supported','PP Supported',
                ifelse(df$Season_Activity.2024 == 'All year','Other Year-round','Other network'))

df$towersites = ordered(df$towersites,levels = c('PP Supported','Other Year-round','Other network'))

write.csv(x = df,file = './data/tower.sites.mapping.csv',row.names = F)
