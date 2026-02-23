
library(terra)
library(ggplot2)

b1 = rast('./output/base_network/base_1km.tif')
b2 = rast('./output/base_network/base_2km.tif')
b5 = rast('./output/base_network/base_5km.tif')
b10 = rast('./output/base_network/base_10km.tif')
b20 = rast('./output/base_network/base_20km.tif')

df1 = as.data.frame(x = b1)
df2 = as.data.frame(x = b2)
df5 = as.data.frame(x = b5)
df10 = as.data.frame(x = b10)
df20 = as.data.frame(x = b20)

ggplot()+
  geom_density(data = df20,aes(x= base.dist,col='20'))+
  geom_density(data = df10,aes(x= base.dist,col='10'))+
  geom_density(data = df5,aes(x= base.dist,col='5'))+
  geom_density(data = df2,aes(x= base.dist,col='2'))+
  geom_density(data = df1,aes(x= base.dist,col='1'))+
  scale_x_continuous(limits = c(0,4))

ggplot()+
  geom_histogram(data = df20,aes(x= base.dist,fill='20'),bins = 50,alpha=0.1)+
  geom_histogram(data = df10,aes(x= base.dist,fill='10'),bins = 50,alpha=0.1)+
  geom_histogram(data = df5,aes(x= base.dist,fill='5'),bins = 50,alpha=0.1)+
  geom_histogram(data = df2,aes(x= base.dist,fill='2'),bins = 50,alpha=0.1)+
  geom_histogram(data = df1,aes(x= base.dist,fill='1'),bins = 50,alpha=0.1)




plot(b1,range = c(0,5));abline(h = -1500000);abline(h = -3500000);abline(v=2000000);abline(v= 200000)

plot(b1,ylim = c(-3500000,-1500000),xlim=c(200000,2000000))

plot(b2,range = c(0,5))
plot(b4,range = c(0,5))

summary(b1)
summary(b2)
summary(b4)

b12 = aggregate(x = b1,fact=2,fun='mean',na.rm=T)
b14 = aggregate(x = b1,fact=4,fun='mean',na.rm=T)

b4 = crop(b4,ext(b14))
b14 = crop(x = b14,y = b4)
?crop
b14
dif12 = b12-b2
dif14 = b14-b4
b14
b4
b14
b4
hist(dif)
plot(dif,range=c(-0.5,0.5))

d1 = as.data.frame(b1)
d2 = as.data.frame(b2)

sum(d1$base.dist)/length(d1$base.dist)
sum(d2$base.dist)/length(d2$base.dist)


#base
base.plot = ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(b1)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = base.towers,aes(x,y,pch=Season_Activity,fill=methane),col='black',show.legend = F,cex = 0.8)+
  scale_shape_manual(values = c(21,24),'Annual Cover',labels = c('Annual','Not Annual'))+
  scale_fill_manual(values = c('red','green3'))+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'none')+
  annotate(geom = 'text',x = -3403909,y = 3474170,label = expression('Growing Season'~CO[2]),size=2)
#base.plot



