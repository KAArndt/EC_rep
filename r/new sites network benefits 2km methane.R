rm(list = ls())
gc()

library(readr)
library(terra)
library(kit)
library(svMisc)
library(data.table)
library(viridis)
library(maps)
library(sf)
library(ggplot2)
library(ggspatial)
setwd('~')

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
pca.towers1$Activity = ifelse(pca.towers1$site == 'Churchill Fen' | 
                              pca.towers1$site == 'Iqaluit' |
                              pca.towers1$site == 'Resolute' |
                              pca.towers1$site == 'Pond Inlet' |
                              pca.towers1$site == 'Kangiqsualujjuaq' |
                              pca.towers1$site == 'CEF cluster' |
                              pca.towers1$site == 'Council (NGEE Arctic)',
                              'active',pca.towers1$Activity)

pca.towers1$CH4 = ifelse(pca.towers1$site == 'Churchill Fen' | 
                                pca.towers1$site == 'Iqaluit' |
                                pca.towers1$site == 'Resolute' |
                                pca.towers1$site == 'Pond Inlet' |
                                pca.towers1$site == 'Kangiqsualujjuaq' |
                                pca.towers1$site == 'CEF cluster' |
                                pca.towers1$site == 'Council (NGEE Arctic)',
                              'CH4',pca.towers1$CH4)

pca.towers1$Annual_cover = ifelse(pca.towers1$site == 'Churchill Fen' | 
                           pca.towers1$site == 'Iqaluit' |
                           pca.towers1$site == 'Resolute' |
                           pca.towers1$site == 'Pond Inlet' |
                           pca.towers1$site == 'Kangiqsualujjuaq' |
                           pca.towers1$site == 'CEF cluster' |
                           pca.towers1$site == 'Council (NGEE Arctic)',
                         'annual',pca.towers1$Annual_cover)

net = which(pca.towers1$Activity == 'active' & pca.towers1$CH4 == 'CH4')
pca.towers1[,c(net)]
#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]

rm(euci)
gc()

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
improve = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers database
new.towers = tower.data[net,]
towers = vect(x = new.towers,geom=c("x", "y"), crs=crs(r))

hist(improve)

plot(improve,range=c(0,4.5))
points(towers,col='red')

#save the base here
#writeRaster(x = improve,filename = './output/improvement_ch4_2km.tif',overwrite = T)

#caluclate difference
#load in the base
base = rast('./output/ch4_2km.tif')
summary(base)

improve = rast('./output/improvement_ch4_2km.tif')

#calulate difference
dif = base - improve

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 35, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
impr.ag = aggregate(x = improve,fact = 4,fun = mean,na.rm = T)
dif.ag  = aggregate(x = dif,fact = 4,fun = mean,na.rm = T)

#this is for the github so it can handle a file of this size
#writeRaster(x = impr.ag,filename = './output/improvement_3.7km_2024.tif',overwrite = T)

#Create Column for added sites
new.towers1 = subset(new.towers,new.towers$site   == 'Churchill Fen' | 
                          new.towers$site == 'Iqaluit' |
                          new.towers$site == 'Resolute' |
                          new.towers$site == 'Pond Inlet' |
                          new.towers$site == 'Kangiqsualujjuaq' |
                          new.towers$site == 'CEF cluster' |
                          new.towers$site == 'Council (NGEE Arctic)')

pca.towers1$Annual_cover = ifelse(pca.towers1$site   == 'Churchill Fen' | 
                                    pca.towers1$site == 'Iqaluit' |
                                    pca.towers1$site == 'Resolute' |
                                    pca.towers1$site == 'Pond Inlet' |
                                    pca.towers1$site == 'Kangiqsualujjuaq' |
                                    pca.towers1$site == 'CEF cluster' |
                                    pca.towers1$site == 'Council (NGEE Arctic)',
                                  "annual",pca.towers1$Annual_cover)

pca.towers1$CH4 = ifelse(pca.towers1$site   == 'Churchill Fen' | 
                                    pca.towers1$site == 'Iqaluit' |
                                    pca.towers1$site == 'Resolute' |
                                    pca.towers1$site == 'Pond Inlet' |
                                    pca.towers1$site == 'Kangiqsualujjuaq' |
                                    pca.towers1$site == 'CEF cluster' |
                                    pca.towers1$site == 'Council (NGEE Arctic)',
                                  "CH4",pca.towers1$CH4)

active          = subset(pca.towers1,pca.towers1$Activity == 'active')
ch4towers       = subset(pca.towers1,pca.towers1$Activity == 'active' & 
                           pca.towers1$CH4 == 'CH4')
annualch4towers = subset(pca.towers1,pca.towers1$Activity == 'active' & 
                           pca.towers1$CH4 == 'CH4' & 
                           pca.towers1$Annual_cover == 'annual')
annual = subset(pca.towers1,pca.towers1$Activity == 'active' & 
                           pca.towers1$Annual_cover == 'annual')

#things needed for all the plots
pal = viridis(n = 8,direction = -1,option = 'A')

#plot the figure
png(filename = './figures/improvement_ch4.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_bw()+ggtitle('Improvement Methane')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(impr.ag)+
  geom_point(data = active,aes(x,y),col='black',fill='transparent',pch=23,size=4)+
  geom_point(data = annual,aes(x,y),col='black',fill='blue',pch=23,size=5)+
  geom_point(data = annualch4towers,aes(x,y),col='black',fill='red',pch=23,size=2)+
  geom_point(data = new.towers1,aes(x,y),col='black',fill='yellow',pch=23,size=5)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,6),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996),'')+
  scale_y_continuous(limits = c(-3687122,4374170),'')+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))
dev.off()


pal = viridis(n = 8,direction = 1,option = 'A')
hist(dif.ag)
png(filename = './figures/ch4 diff.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_bw()+ggtitle('Methane Improvement')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.ag)+
  geom_point(data = new.towers1,aes(x,y),col='black',fill='cyan',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,1.5),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996),'')+
  scale_y_continuous(limits = c(-3687122,4374170),'')+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))
dev.off()

pal = viridis(n = 8,direction = 1,option = 'A')
hist(dif)

ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,2),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))


terra::summary(improve)
terra::summary(base)
summary(df.base)

df.base = as.data.frame(x = base,na.rm=T)
df.improve = as.data.frame(x = improve,na.rm=T)

summary(df.base)

ggplot()+
  geom_histogram(data = df.base,aes(x = base.dist),fill='gray',bins=200)+
  geom_histogram(data = df.improve,aes(x = base.dist),fill='red',bins=200)


#things needed for all the plots
pal = viridis(n = 11,direction = -1,option = 'A')
pal = brewer.pal(n = 11,name = 'PRGn')
pal = c(pal[11],pal[10],pal[9],pal[8],pal[7],pal[6],pal[5],pal[4],pal[3],pal[2],pal[1])
col = c(pal[7],pal[8],pal[9],pal[10],pal[11],'black',pal[1],pal[2],pal[3],pal[4],pal[5])


library(RColorBrewer)


#plot the figure
#png(filename = './figures/improvement.png',width = 6,height = 6,units = 'in',res = 1000)
ggplot()+theme_bw()+ggtitle('Improvement')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                        colours = col,
                       #trans = 'log',
                       limits = c(0,1.89919  *2),
                       labels = c('good','cutoff','poor'),
                       breaks = c(0,1.89919  ,1.89919  *2),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996),'')+
  scale_y_continuous(limits = c(-3687122,4374170),'')+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))
#dev.off()

ggplot()+theme_bw()+ggtitle('Improvement')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(impr.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  geom_point(data = new.towers1,aes(x,y),col='black',fill='white',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = col,
                       #trans = 'log',
                       limits = c(0,1.89919  *2),
                       labels = c('good','cutoff','poor'),
                       breaks = c(0,1.89919  ,1.89919  *2),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996),'')+
  scale_y_continuous(limits = c(-3687122,4374170),'')+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))

pal = viridis(n = 8,direction = 1,option = 'A')
hist(dif)

ggplot()+theme_bw()+ggtitle('All Active Sites')+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.ag)+
  geom_point(data = new.towers,aes(x,y),col='black',fill='green',pch=23,size=2)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #trans = 'log',
                       limits = c(0,2),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'),
        panel.background = element_rect(fill = 'lightblue3'))


terra::summary(improve)
terra::summary(base)

df.base = as.data.frame(x = base,na.rm=T)
df.improve = as.data.frame(x = improve,na.rm=T)

ggplot()+
  geom_histogram(data = df.base,aes(x = base.dist),fill='gray',bins=200)+
  geom_histogram(data = df.improve,aes(x = base.dist),fill='red',bins=200)

b = summary(df.base$base.dist)
i = summary(df.improve$base.dist)

i[1] - b[1]/b[5]
(b[5] - i[5])/b[5]

(sum(df.base$base.dist) - sum(df.improve$base.dist))/sum(df.base$base.dist)

dif.df = as.data.frame(x = dif,na.rm=T)
sub = subset(dif.df,dif.df$base.dist > 0)

length(sub$base.dist)/length(df.base$base.dist)

sub = subset(df.improve,sqrt(df.improve$base.dist) > 1.2155)
length(sub$base.dist)/length(df.improve$base.dist)

sub = subset(df.base,sqrt(df.base$base.dist) > 1.2155)

length(sub$base.dist)/length(df.improve$base.dist)

length(df.base$base.dist)*2*.05

hist(sqrt(df.base$base.dist))
summary(sqrt(df.base$base.dist))
