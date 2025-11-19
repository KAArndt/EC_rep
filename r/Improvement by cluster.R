library(terra)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggrepel)

#load in all the spatial data
#rep distances
gsco2 = rast('./output/base_network/base_2km.tif')
gsch4 = rast('./output/base_network/methane_2km.tif')
anco2 = rast('./output/base_network/annual_2km.tif')
anch4 = rast('./output/base_network/annual_methane_2km.tif')

igsco2 = rast('./output/improved_network/improved_base_2km.tif')
igsch4 = rast('./output/improved_network/improved_methane_2km.tif')
ianco2 = rast('./output/improved_network/improved_annual_2km.tif')
ianch4 = rast('./output/improved_network/improved_annual_methane_2km.tif')

#clusters
clust = rast('./output/clusts.tif')
clust = clust$km40

#pca results
#pca = rast('./spatial_data/pca_2km.tif')
#pca = crop(x = pca,y = clust)

#environmental data
#r = rast('./spatial_data/spatial_repro_2km.tif')
#r = crop(x = r,y = clust)

#merge all into one stack
base     = c(gsco2,gsch4,anco2,anch4)
improved = c(igsco2,igsch4,ianco2,ianch4)

names(base) = c('gsco2','gsch4','anco2','anch4')
names(improved) = c('gsco2','gsch4','anco2','anch4')

#aggregate to make plotting and playing with data more manageable
ag.base     = aggregate(x = base,fact = 10,fun = 'mean',cores = 6,na.rm = T)
ag.imporved = aggregate(x = improved,fact = 10,fun = 'mean',cores = 6,na.rm = T)

ag.km = aggregate(x = clust,fact = 10,fun = 'modal',na.rm = T)

ag.base     = c(ag.base,ag.km)
ag.improved = c(ag.imporved,ag.km)

#extract all as a data frame
df.base     = as.data.frame(x = ag.base,xy=T)
df.improved = as.data.frame(x = ag.improved,xy=T)

df.base     = df.base[complete.cases(df.base$gsco2),]
df.improved = df.improved[complete.cases(df.improved$gsco2),]

df.base$cat = 'base'
df.improved$cat = 'improved'

df = rbind(df.base,df.improved)

#ecoregions
# eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')
# eco = crop(x = eco,y = c(-180,180,40,90))
# eco = subset(eco,eco$BIOME_NAME == 'Tundra' | eco$BIOME_NAME == 'Boreal Forests/Taiga')
# eco = project(x = eco,y = crs(ag))

#make the cluster a character for plotting
df$km40 = as.character(df$km40)

df$km40 = ordered(x = df$km40,c(
'1', '2' ,'3' ,'4' ,'5' ,'6' ,'7' ,'8' ,'9' ,'10',
'11','12','13','14','15','16','17','18','19','20',
'21','22','23','24','25','26','27','28','29','30',
'31','32','33','34','35','36','37','38','39','40'))

#calculate summary statistics
library(plotrix)
df$count = 1

stats = df %>%
  group_by(km40,cat) %>%
  summarise_all(list(mean,sum,std.error))


#color pallette
pal = hcl.colors(n = 9,palette = 'Vik')
pal = pal[-c(4,6)]
#pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

m = merge(df.base,df.improved,by = c('x','y'))

stat.base = subset(stats,stats$cat == 'base')
stat.imp = subset(stats,stats$cat == 'improved')

m.stat = merge(stat.base,stat.imp,by = 'km40')

#play with data
ggplot(data = m.stat)+
  geom_bar(aes(x = km40,y = count_fn2.x,fill=gsco2_fn1.y),stat = 'identity',position = position_dodge())+
  scale_fill_gradientn('Rep.',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)

a = ggplot(data = m.stat,aes(gsco2_fn1.x,gsco2_fn1.y,label=km40))+
  theme_bw()+geom_abline(slope = 1,intercept = 0)+
  annotate(geom = 'segment',x = 1.56,xend = 6,y = 1.56,col='red',lty=2)+
  annotate(geom = 'segment',x = 1.56,yend = 1.56,y = 0.5,col='red',lty=2)+
  geom_point(size=1)+
  scale_y_continuous('2024 Rep.',limits = c(0.5,6),expand = c(0,0))+
  scale_x_continuous('',limits = c(0.5,6),expand = c(0,0))+
  geom_label_repel(label.size = NA,max.overlaps = 20,size=2)+
  annotate(geom = "polygon", x = c(0.5, 1.56 ,1.56), y = c(0.5, 1.56, 0.5), fill = "blue", alpha = 0.1)+
  annotate(geom = "polygon", x = c(1.56, 6 ,6), y = c(1.56, 6, 1.56), fill = "brown", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.56,xmax =  6,ymin = 0.5, ymax = 1.56, fill = "green", alpha = 0.1)+
  theme(text = element_text(size = 7))+
  annotate(geom = 'text',x = 3,y = 5.5,label=expression("Growing Season "*CO[2]),size = 3)

b = ggplot(data = m.stat,aes(gsch4_fn1.x,gsch4_fn1.y,label=km40))+
  theme_bw()+geom_abline(slope = 1,intercept = 0)+
  annotate(geom = 'segment',x = 1.56,xend = 6,y = 1.56,col='red',lty=2)+
  annotate(geom = 'segment',x = 1.56,yend = 1.56,y = 0.5,col='red',lty=2)+  
  geom_point(size=1)+
  scale_y_continuous('',limits = c(0.5,6),expand = c(0,0))+
  scale_x_continuous('',limits = c(0.5,6),expand = c(0,0))+
  geom_label_repel(label.size = NA,max.overlaps = 20,size=2)+
  annotate(geom = "polygon", x = c(0.5, 1.56 ,1.56), y = c(0.5, 1.56, 0.5), fill = "blue", alpha = 0.1)+
  annotate(geom = "polygon", x = c(1.56, 6 ,6), y = c(1.56, 6, 1.56), fill = "brown", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.56,xmax =  6,ymin = 0.5, ymax = 1.56, fill = "green", alpha = 0.1)+
  theme(text = element_text(size = 7))+
  annotate(geom = 'text',x = 3,y = 5.5,label=expression("Growing Season "*CH[4]),size = 3)


c = ggplot(data = m.stat,aes(anco2_fn1.x,anco2_fn1.y,label=km40))+
  theme_bw()+geom_abline(slope = 1,intercept = 0)+
  annotate(geom = 'segment',x = 1.56,xend = 6,y = 1.56,col='red',lty=2)+
  annotate(geom = 'segment',x = 1.56,yend = 1.56,y = 0.5,col='red',lty=2)+
  geom_point(size=1)+
  scale_y_continuous('2024 Rep.',limits = c(0.5,6),expand = c(0,0))+
  scale_x_continuous('2022 Rep.',limits = c(0.5,6),expand = c(0,0))+
  geom_label_repel(label.size = NA,max.overlaps = 20,size=2)+
  annotate(geom = "polygon", x = c(0.5, 1.56 ,1.56), y = c(0.5, 1.56, 0.5), fill = "blue", alpha = 0.1)+
  annotate(geom = "polygon", x = c(1.56, 6 ,6), y = c(1.56, 6, 1.56), fill = "brown", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.56,xmax =  6,ymin = 0.5, ymax = 1.56, fill = "green", alpha = 0.1)+
  theme(text = element_text(size = 7))+
  annotate(geom = 'text',x = 3,y = 5.5,label=expression("Year-round "*CO[2]),size = 3)


d = ggplot(data = m.stat,aes(anch4_fn1.x,anch4_fn1.y,label=km40))+
  theme_bw()+geom_abline(slope = 1,intercept = 0)+
  annotate(geom = 'segment',x = 1.56,xend = 6,y = 1.56,col='red',lty=2)+
  annotate(geom = 'segment',x = 1.56,yend = 1.56,y = 0.5,col='red',lty=2)+  
  geom_point(size=1)+
  scale_y_continuous('',limits = c(0.5,6),expand = c(0,0))+
  scale_x_continuous('2022 Rep.',limits = c(0.5,6),expand = c(0,0))+
  geom_label_repel(label.size = NA,max.overlaps = 20,size=2)+
  annotate(geom = "polygon", x = c(0.5, 1.56 ,1.56), y = c(0.5, 1.56, 0.5), fill = "blue", alpha = 0.1)+
  annotate(geom = "polygon", x = c(1.56, 6 ,6), y = c(1.56, 6, 1.56), fill = "brown", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.56,xmax =  6,ymin = 0.5, ymax = 1.56, fill = "green", alpha = 0.1)+
  theme(text = element_text(size = 7))+
  annotate(geom = 'text',x = 3,y = 5.5,label=expression("Year-round "*CH[4]),size = 3)


png(filename = './figures/scatter.cluster.reduction.png',width = 6,height = 6,units = 'in',res = 1500)
plot_grid(a,b,c,d,labels = c('a','b','c','d'),label_size = 8)
dev.off()
