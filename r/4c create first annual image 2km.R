#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
gc()

library(svMisc)
library(maps)
library(ggplot2)
library(ggspatial)
library(terra)
#library(kit)
library(sf)
library(viridis)
library(data.table)
library(readr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towersv2.csv')

#load back in euclidean distance matrix
euci = read_rds('./data/euci_2kmv2.rds')

pca.towers = tower.data

#add new sites to inactive towers
net = which(complete.cases(pca.towers$`2022 list`) & pca.towers$active == 'active' & pca.towers$Start_CO2 < 2022 & pca.towers$Season_Activity == 'All year')

euci.net = euci[,c(net)]

#rm(euci)
#gc()

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
for (i in 1:nrow(euci.net)) {
  base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
#  base.dist[i] = min(euci.net[i,])
  }
Sys.time() - orig} #stop the clock

#create base image
#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)
df = as.data.frame(x = r,xy = T,na.rm = T)

#make the base image
basedf = data.frame(df$x,df$y,base.dist)

base = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
base.towers = tower.data[net,]
towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))

hist(base)
plot(base,range=c(0,3.5))
points(towers,col='red')

#save the base here
writeRaster(x = base,filename = './output/annual_2kmv2.tif',overwrite = T)

#######################################################################################
base = rast('./output/annual_2kmv2.tif')
#base = base/minmax(base)[2] #use this to rescale from 0-1

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))

#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

#create a scatter to show data spread
# base.df = as.data.frame(base)
# ggplot()+theme_classic()+
#   geom_density(aes(base.df$base.dist),fill='gray',alpha=0.5)+
#   scale_x_continuous(expand = c(0,0),'Euc. Dist.')+
#   scale_y_continuous(expand = c(0,0),limits = c(0,1))+
#   theme(text = element_text(size = 8))

towers  = subset(pca.towers,complete.cases(pca.towers$`2022 list`) & pca.towers$active == 'active' & pca.towers$Start_CO2 < 2022 & pca.towers$Season_Activity == 'All year')

png(filename = './figures/annualv2.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag$base.dist)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = towers,aes(x,y,fill=methane,pch=Season_Activity,col=methane),col='black',show.legend = F)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('cyan','green'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')
dev.off()


