
library(terra)
library(data.table)
library(ggplot2)
library(sf)
library(cowplot)
library(ggspatial)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.base.csv')
tower.data$active = ifelse(is.na(tower.data$active),'extension',tower.data$active)

#load in the base image for the plot
base = rast('./output/improved_network/improved_base_2km.tif')
meth = rast('./output/improved_network/improved_methane_2km.tif')
annu = rast('./output/improved_network/improved_annual_2km.tif')
anme = rast('./output/improved_network/improved_annual_methane_2km.tif')

#create aggregates for the plots
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
meth.ag = aggregate(x = meth,fact = 4,fun = mean,na.rm = T)
annu.ag = aggregate(x = annu,fact = 4,fun = mean,na.rm = T)
anme.ag = aggregate(x = anme,fact = 4,fun = mean,na.rm = T)

########################################################################################################
gsco2 = fread('./data/reductions/meanreduction_remaining.csv')
anco2 = fread('./data/reductions/meanreduction_annual.csv')
gsch4 = fread('./data/reductions/meanreduction_methane.csv')
anch4 = fread('./data/reductions/meanreduction_annual_methane.csv')


gsco2$rank = rank(x = gsco2$means,ties.method = 'first')
anco2$rank = rank(x = anco2$means,ties.method = 'first')
gsch4$rank = rank(x = gsch4$means,ties.method = 'first')
anch4$rank = rank(x = anch4$means,ties.method = 'first')

topsites.gsco2 = subset(gsco2,gsco2$rank <=20)
topsites.anco2 = subset(anco2,anco2$rank <=20)
topsites.gsch4 = subset(gsch4,gsch4$rank <=20)
topsites.anch4 = subset(anch4,anch4$rank <=20)


library(RColorBrewer)
#Canada Finland Greenland Norway Russia   USA
pal = brewer.pal(n = 6,name = 'Spectral')



a = ggplot(data = topsites.gsco2)+theme_bw()+ggtitle(expression('Growing Season '*CO[2]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.08),'Mean Improvement')+
  scale_x_discrete('')+
  scale_fill_manual(values = pal[c(5)],'Country')+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size = 6,angle = 90,hjust = 1,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        title = element_text(size = 6),
        legend.position = 'none')

b = ggplot(data = topsites.gsch4)+theme_bw()+ggtitle(expression('Growing Season '*CH[4]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.08),'')+
  scale_x_discrete('')+
  scale_fill_manual(values = pal[c(1,3,4,5,6)],'Country')+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size = 6,angle = 90,hjust = 1,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        title = element_text(size = 6),
        legend.position = 'none')

c = ggplot(data = topsites.anco2)+theme_bw()+ggtitle(expression('Year-round '*CO[2]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.08),'')+
  scale_x_discrete('')+
  scale_fill_manual(values = pal[c(1,2,5,6)],'Country')+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size = 6,angle = 90,hjust = 1,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        title = element_text(size = 6),
        legend.position = 'none')

d = ggplot(data = topsites.anch4)+theme_bw()+ggtitle(expression('Year-round '*CH[4]))+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.08),'')+
  scale_x_discrete('')+
  scale_fill_manual(values = pal[c(1,2,3,4,5,6)],'Country')+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size = 6,angle = 90,hjust = 1,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.1,units = 'in'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        title = element_text(size = 6),
        legend.position = c(0.7,0.78),
        legend.direction = 'vertical')


png(filename = './figures/figure sxx barplot_reduction_mean.png',width = 8,height = 5,units = 'in',res = 1800)
plot_grid(a,b,c,d,nrow = 1,align = 'hv',labels = c('a','b','c','d'),label_size = 8)
dev.off()





names(tower.data)[21] = 'sitename'




#top10$y = ifelse(top10$site == 'Kimmirut',top10$y+100000,no = top10$y)
#top10$x = ifelse(top10$site == 'Kimmirut',top10$x-100000,no = top10$x)



#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))


#png(filename = './figures/figure Sx top 12 sites map.png',width = 6,height = 5,units = 'in',res = 1500)
#map of the top sites
#map of the top sites
topgsco2 = subset(gsco2,gsco2$rank <= 20)
topgsco2 = merge(topgsco2,tower.data,by = 'sitename')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label_repel(data = topgsco2,aes(x,y,label=rank),col='black',show.legend = F,cex = 2)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 5),
        legend.text = element_text(size = 5),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

#methane
topmethane = subset(gsch4,gsch4$rank <= 20)
topmethane = merge(topmethane,tower.data,by = 'sitename')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(meth.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label_repel(data = topmethane,aes(x,y,label=rank),col='black',show.legend = F,cex = 3)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

#methane
topanco2 = subset(anco2,anco2$rank <= 20)
topanco2 = merge(topanco2,tower.data,by = 'sitename')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annu.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label_repel(data = topanco2,aes(x,y,label=rank),col='black',show.legend = F,cex = 3)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

#annual methane
topannualmethane = subset(anch4,anch4$rank <= 20)
topannualmethane = merge(topannualmethane,tower.data,by = 'sitename')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(meth.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.56*2),
                       breaks = c(0,1.56,1.56*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label_repel(data = topannualmethane,aes(x,y,label=rank),col='black',show.legend = F,cex = 3)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.05,units = 'in'),
        legend.key.width = unit(x = 0.2,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

bars = fread('./output/reductions/meanreduction_remaining_mean.csv')
upper.limit = -1*min(bars$means)+0.005

bars$rank = rank(x = bars$means,ties.method = 'first')
tower.data$sitename = tower.data$site

sites = merge(bars,tower.data,by = 'sitename')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(base))

#create aggregates for the plots
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

#plot the figure
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

topsites = subset(bars,bars$rank <=100)

png(filename = './figures/figure 3 barplot_reduction_mean.png',width = 6,height = 4,units = 'in',res = 2500)
ggplot(data = topsites)+theme_bw()+
  geom_bar(aes(reorder(rank, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean Improvement')+
  scale_x_discrete('Site Rank')+
  scale_fill_brewer(palette = "Spectral",'Country')+
  theme(axis.text.x = element_text(size = 4.5,angle = 90,hjust = 1,vjust = 0.5),
        panel.grid.major.x = element_blank(),
        legend.key.size = unit(0.1,units = 'in'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = c(0.55,0.55),
        legend.direction = 'horizontal')
dev.off()

