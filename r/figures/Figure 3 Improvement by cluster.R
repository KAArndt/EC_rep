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
clust = rast('./spatial_data/km40.tif')

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

#calculate summary statistics
library(plotrix)
df$count = 1

stats = df %>%
  group_by(newkm,cat) %>%
  summarise_all(list(mean,sum,std.error))


#color pallette
pal = hcl.colors(n = 9,palette = 'Vik')
pal = pal[-c(4,6)]
#pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

m = merge(df.base,df.improved,by = c('x','y'))

stat.base = subset(stats,stats$cat == 'base')
stat.imp = subset(stats,stats$cat == 'improved')

m.stat = merge(stat.base,stat.imp,by = 'newkm')

#play with data
ggplot(data = m.stat)+
  geom_bar(aes(x = newkm,y = count_fn2.x,fill=gsco2_fn1.y),stat = 'identity',position = position_dodge())+
  scale_fill_gradientn('Rep.',
                       colours = pal,
                       limits = c(0,1.96*2),
                       breaks = c(0,1.96,1.96*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)

#plots ##########
a = ggplot(data = m.stat)+theme_bw()+ggtitle(expression('Growing Season '*CO[2]))+
  geom_segment(aes(xend = gsco2_fn1.y+0.01,x = gsco2_fn1.x,y = newkm))+
  geom_point(aes(gsco2_fn1.x,newkm,col='2022'))+
  geom_point(aes(gsco2_fn1.y,newkm,col='2024'))+
  scale_color_manual(values = c(pal[7],pal[1]))+
  geom_vline(xintercept = 2.11)+
  geom_vline(xintercept = 1.77)+
  scale_x_continuous(limits = c(0.5,6),expand = c(0,0),'')+
  scale_y_continuous(limits = c(0,41),expand = c(0,0),'')+
  annotate(geom = "rect", xmin = 0.5,xmax =  1.77,ymin = 0, ymax = 41, fill = "green", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.77,xmax =  2.11,ymin = 0, ymax = 41, fill = "blue", alpha = 0.1)+
  annotate(geom = "rect", xmin = 2.11,xmax =  6,ymin = 0, ymax = 41, fill = "brown", alpha = 0.1)+
  theme(legend.position = c(0.87,0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent',colour = 'black'),
        legend.key = element_blank(),
        legend.key.size = unit(0.1,units = 'in'),
        title = element_text(size = 7))

b = ggplot(data = m.stat)+theme_bw()+ggtitle(expression('Growing Season '*CH[4]))+
  geom_segment(aes(xend = gsch4_fn1.y+0.01,x = gsch4_fn1.x,y = newkm))+
  geom_point(aes(gsch4_fn1.x,newkm,col='2022'))+
  geom_point(aes(gsch4_fn1.y,newkm,col='2024'))+
  scale_color_manual(values = c(pal[7],pal[1]))+
  geom_vline(xintercept = 2.11)+
  geom_vline(xintercept = 1.77)+
  scale_x_continuous(limits = c(0.5,6),expand = c(0,0),'')+
  scale_y_continuous(limits = c(0,41),expand = c(0,0),'')+
  annotate(geom = "rect", xmin = 0.5,xmax =  1.77,ymin = 0, ymax = 41, fill = "green", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.77,xmax =  2.11,ymin = 0, ymax = 41, fill = "blue", alpha = 0.1)+
  annotate(geom = "rect", xmin = 2.11,xmax =  6,ymin = 0, ymax = 41, fill = "brown", alpha = 0.1)+
  theme(legend.position = 'none',
        title = element_text(size = 7))

c = ggplot(data = m.stat)+theme_bw()+ggtitle(expression('Year-round '*CO[2]))+
  geom_segment(aes(xend = anco2_fn1.y+0.01,x = anco2_fn1.x,y = newkm))+
  geom_point(aes(anco2_fn1.x,newkm,col='2022'))+
  geom_point(aes(anco2_fn1.y,newkm,col='2024'))+
  scale_color_manual(values = c(pal[7],pal[1]))+
  geom_vline(xintercept = 2.11)+
  geom_vline(xintercept = 1.77)+
  scale_x_continuous(limits = c(0.5,6),expand = c(0,0),'')+
  scale_y_continuous(limits = c(0,41),expand = c(0,0),'')+
  annotate(geom = "rect", xmin = 0.5,xmax =  1.77,ymin = 0, ymax = 41, fill = "green", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.77,xmax =  2.11,ymin = 0, ymax = 41, fill = "blue", alpha = 0.1)+
  annotate(geom = "rect", xmin = 2.11,xmax =  6,ymin = 0, ymax = 41, fill = "brown", alpha = 0.1)+
  theme(legend.position = 'none',
        title = element_text(size = 7))

d = ggplot(data = m.stat)+theme_bw()+ggtitle(expression('Year-round '*CH[4]))+
  geom_segment(aes(xend = anch4_fn1.y+0.01,x = anch4_fn1.x,y = newkm))+
  geom_point(aes(anch4_fn1.x,newkm,col='2022'))+
  geom_point(aes(anch4_fn1.y,newkm,col='2024'))+
  scale_color_manual(values = c(pal[7],pal[1]))+
  geom_vline(xintercept = 2.11)+
  geom_vline(xintercept = 1.77)+
  scale_x_continuous(limits = c(0.5,6),expand = c(0,0),'')+
  scale_y_continuous(limits = c(0,41),expand = c(0,0),'')+
  annotate(geom = "rect", xmin = 0.5,xmax =  1.77,ymin = 0, ymax = 41, fill = "green", alpha = 0.1)+
  annotate(geom = "rect", xmin = 1.77,xmax =  2.11,ymin = 0, ymax = 41, fill = "blue", alpha = 0.1)+
  annotate(geom = "rect", xmin = 2.11,xmax =  6,ymin = 0, ymax = 41, fill = "brown", alpha = 0.1)+
  theme(legend.position = 'none',
        title = element_text(size = 7))

cp = plot_grid(a,b,c,d,labels = c('a','b','c','d'),label_size = 8,nrow = 2,hjust = -3)

final.plot = ggdraw(cp) + 
  draw_label("Euclidean distance", x = 0.5, y = 0, vjust = -0.5, size = 11)+
  draw_label("Cluster", x = 0, y = 0.5,angle = 90, vjust = 1,size = 11)
  
final.plot

png(filename = './figures/figure 3 cluster.reduction.png',width = 6,height = 6,units = 'in',res = 1500)
final.plot
dev.off()
