
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
base = rast('./output/base_network/base_2kmv2_mean.tif')
base
########################################################################################################
bars = fread('./output/reductions/meanreduction_mean.csv')
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

#map of the top sites
top10 = subset(sites,sites$rank <= 12)

top10$y = ifelse(top10$site == 'Kimmirut',top10$y+100000,no = top10$y)
top10$x = ifelse(top10$site == 'Kimmirut',top10$x-100000,no = top10$x)

png(filename = './figures/figure Sx top 12 sites map.png',width = 6,height = 5,units = 'in',res = 1500)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.69*2),
                       breaks = c(0,1.69,1.69*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label(data = top10,aes(x,y,label=rank),col='black',show.legend = F,cex = 2)+
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
dev.off()

##########################################################################
png(filename = './figures/presentation top 12 sites map.png',width = 6,height = 5,units = 'in',res = 2500)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.69*2),
                       breaks = c(0,1.69,1.69*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+ 
  geom_label(data = top10,aes(x,y,label=rank),col='black',show.legend = F,cex = 3)+
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
dev.off()





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

