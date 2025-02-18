rm(list = ls())
gc()

library(readr)
library(terra)
library(kit)
library(svMisc)

#load back in
euci = read_rds('./data/euci_2km.rds')

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=12,na.rm=T)
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
pca.towers1 = tower.data

#find columns which are active sites
pca.towers1$Activity = ifelse(pca.towers1$site == 'Churchill Fen' | pca.towers1$site == 'Iqaluit',
                              'inactive',pca.towers1$Activity)

pca.towers1$Activity = ifelse(tower.data$site == 'Churchill Fen' |
                     tower.data$site == 'Council (NGEE Arctic)' |
                     tower.data$site == 'Iqaluit' |
                     tower.data$site == 'Kangiqsualujjuaq' |
                     tower.data$site == 'Lutose' |
                     tower.data$site == 'Pond Inlet' |
                     tower.data$site == 'Resolute' |
                     tower.data$site == 'Scotty Creek Bog' |
                     tower.data$site == 'Scotty Creek Landscape' |
                     tower.data$site == 'CEF cluster' |
                     tower.data$site == 'Chersky, control' |
                     tower.data$site == 'Chersky, drained' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, mesic' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, wetland' |
                     tower.data$site == 'Steen River' |
                     tower.data$site == 'Smith Creek' |
                     tower.data$site == 'Iqaluit','active',pca.towers1$Activity)

net = which(pca.towers1$Activity == 'active')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]

rm(euci)
gc()

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
#eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
impdf = data.frame(df$x,df$y,base.dist)
imp = rast(x = impdf,type = 'xyz',crs = crs(r))

#project the towers dataimp
imp.towers = tower.data[net,]
towers = vect(x = imp.towers,geom=c("x", "y"), crs=crs(r))

hist(imp)
plot(imp,range=c(0,4.5))
points(towers)

#save the imp here
writeRaster(x = imp,filename = './output/improve_2km.tif',overwrite = T)

#######################################################################################
imp = rast('./output/improve_2km.tif')
base = rast('./output/base_2km.tif')

#imp = imp/minmax(imp)[2]

hist(imp)
summary(imp)
imp
plot(imp)
#load in imp map
#things needed for all the plots
pal = viridis(n = 8,direction = -1,option = 'A')


#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(imp))


#create an aggregate for the plot
imp.ag = aggregate(x = imp,fact = 4,fun = mean,na.rm = T)
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

ch4 = subset(imp.towers,imp.towers$CH4 == 'CH4')
annualch4 = subset(imp.towers,imp.towers$CH4 == 'CH4' & imp.towers$Annual_cover == 'annual')

library(RColorBrewer)

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')


new.sites = subset(tower.data,tower.data$site == 'Churchill Fen' |
                     tower.data$site == 'Council (NGEE Arctic)' |
                     tower.data$site == 'Iqaluit' |
                     tower.data$site == 'Kangiqsualujjuaq' |
                     tower.data$site == 'Lutose' |
                     tower.data$site == 'Pond Inlet' |
                     tower.data$site == 'Resolute' |
                     tower.data$site == 'Scotty Creek Bog' |
                     tower.data$site == 'Scotty Creek Landscape' |
                     tower.data$site == 'CEF cluster' |
                     tower.data$site == 'Chersky, control' |
                     tower.data$site == 'Chersky, drained' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, mesic' |
                     tower.data$site == 'Cambridge Bay, Victoria Island, wetland' |
                     tower.data$site == 'Steen River' |
                     tower.data$site == 'Smith Creek' |
                     tower.data$site == 'Iqaluit')

active = subset(pca.towers1,pca.towers1$Activity == 'active')
active$CH4 = ifelse(active$CH4 == '','no',active$CH4)

#improved plot
png(filename = './figures/improved.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(imp.ag$base.dist)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.8),
                       breaks = c(0,1.8,3.8),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',show.legend = F)+
  geom_point(data = new.sites,aes(x,y),col='black',fill = 'red',pch = 21,show.legend = F)+
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
#  annotate(geom = 'text',label='Improved',x = -4093909,y = 3075097)
dev.off()


#base lot
png(filename = './figures/base_2km.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag$base.dist)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,3.8),
                       breaks = c(0,1.8,3.8),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = active,aes(x,y,fill=CH4,pch=Annual_cover,col=CH4),col='black',show.legend = F)+
  geom_point(data = new.sites,aes(x,y),col='black',fill = 'red',pch = 21,show.legend = F)+
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
#  annotate(geom = 'text',label='Improved',x = -4093909,y = 3075097)
dev.off()
