
library(terra)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(sf)
library(ggspatial)

#load all the maps at different scales
b1 = rast('./output/base_network/base_1km.tif')
b2 = rast('./output/base_network/base_2km.tif')
b5 = rast('./output/base_network/base_5km.tif')
b10 = rast('./output/base_network/base_10km.tif')
b1
#make data frames
df1 = as.data.frame(x = b1)
df2 = as.data.frame(x = b2)
df5 = as.data.frame(x = b5)
df10 = as.data.frame(x = b10)

#plot density plots
pal = brewer.pal(n = 4,name = 'Spectral')


png(filename = './figures/figure sxx kernel spatial scales.png',width = 4,height = 3,units = 'in',res = 1500)
ggplot()+theme_bw()+
  geom_density(data = df10,aes(x= base.dist,col='10'))+
  geom_density(data = df5,aes(x= base.dist,col='5'))+
  geom_density(data = df2,aes(x= base.dist,col='2'))+
  geom_density(data = df1,aes(x= base.dist,col='1'))+
  scale_x_continuous(limits = c(0,5),expand = c(0,0),'Euclidean Distance')+
  scale_y_continuous(limits = c(0,1),expand = c(0,0),'Kernel Density')+
  scale_color_manual(values = c('1' = pal[1],'2' = pal[2],'5' = pal[3],'10' = pal[4]),
                     'Spatial Scale (km)',
                     breaks = c('1','2','5','10'))+
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.background = element_rect(colour = 'black'))
dev.off()


plot(b1,range = c(0,5));abline(v=-1200000);abline(v= -500000);abline(h = 1850000);abline(h = 2200000)

plot(b1,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b2,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b5,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b10,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))


summary(b1)
summary(b2)
summary(b5)
summary(b10)

b12 = aggregate(x = b1,fact=2,fun='mean',na.rm=T)
b15 = aggregate(x = b1,fact=5,fun='mean',na.rm=T)
b110 = aggregate(x = b1,fact=10,fun='mean',na.rm=T)


#compare 10
b110 = project(x = b110,y = b10)

names(b10) = 'b10'
names(b110) = 'b110'

t12 = c(b10,b110)

plot(t12$b10 - t12$b110)
hist(t12$b10 - t12$b110)

#compare 2
b12 = project(x = b12,y = b2)

names(b2) = 'b2'
names(b12) = 'b12'

t12 = c(b2,b12)

plot(t12$b2 - t12$b12)
hist(t12$b2 - t12$b12)


#compare 5
b15 = project(x = b15,y = b5)

names(b5) = 'b5'
names(b15) = 'b15'

t12 = c(b5,b15)

plot(t12$b5 - t12$b15)
hist(t12$b5 - t12$b15)

sum(df1$base.dist)/length(df1$base.dist)
sum(df2$base.dist)/length(df2$base.dist)
sum(df5$base.dist)/length(df5$base.dist)
sum(df10$base.dist)/length(df10$base.dist)



#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(b1))


#color pallette
pal = hcl.colors(n = 9,palette = 'Vik')
pal = pal[-c(4,6)]


plot(b1,range = c(0,5));abline(v=-1200000);abline(v= -500000);abline(h = 1850000);abline(h = 2200000)

plot(b1,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b2,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b5,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))
plot(b10,xlim=c(-1200000,-500000),ylim = c(1850000,2200000),range=c(0,3.1))


b1c = crop(x = b1,y = c(-1200000,-500000,1850000,2200000))
b2c = crop(x = b2,y = c(-1200000,-500000,1850000,2200000))
b5c = crop(x = b5,y = c(-1200000,-500000,1850000,2200000))
b10c = crop(x = b10,y = c(-1200000,-500000,1850000,2200000))

#base
big = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(b10)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','ER4','Poor'),
                       oob = scales::squish)+  
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  geom_vline(xintercept = -1200000)+
  geom_vline(xintercept = -500000)+
  geom_hline(yintercept = 1850000)+
  geom_hline(yintercept = 2200000)+
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')
#  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CO[2]),size=2)
#base.plot

p1 = ggplot()+theme_map()+
  layer_spatial(b1c)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','ER4','Poor'),
                       oob = scales::squish)+  
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -700000,y = 1900000,label = expression('1 km'),size=2)

p2 = ggplot()+theme_map()+
  layer_spatial(b2c)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','ER4','Poor'),
                       oob = scales::squish)+  
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -700000,y = 1900000,label = expression('2 km'),size=2)

p3 = ggplot()+theme_map()+
  layer_spatial(b5c)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','ER4','Poor'),
                       oob = scales::squish)+  
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -700000,y = 1900000,label = expression('5 km'),size=2)


p4 = ggplot()+theme_map()+
  layer_spatial(b10c)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','ER4','Poor'),
                       oob = scales::squish)+  
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -700000,y = 1900000,label = expression('10 km'),size=2)




p = plot_grid(p1,p2,p3,p4)

png(filename = './figures/si xx spatial scale visual.png',width = 10,height = 4.5,units = 'in',res = 1500)
plot_grid(big,p,nrow = 1,rel_widths = c(0.4,0.6))
dev.off()
