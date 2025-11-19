
library(data.table)
library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(seegSDM)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(cowplot)

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towers.base.csv')
active.sites = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#set just the coordinates for the extract
xy.tower = active.sites[,c(59,60)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')

clust = clust$km40
names(clust) = 'cluster'

#extract data
clustdat = extract(x = clust,y = xy.tower,cells=T,xy=T)
nas = clustdat[is.na(clustdat$cluster),] #extract where nas
clustr = stack(clust) #make a raster version

#find coordinates
na.cor = as.data.frame(nearestLand(points = nas[,c(4,5)],raster = clustr,max_distance = 2000000))
summary(na.cor)

#place in original data frame
clustdat[nas$ID,] = extract(x = clust,y = na.cor,cells=T,xy=T)
clustdat$site = active.sites$site
active.sites$cluster = clustdat$cluster


#rename site statuses
active.sites$methane         = ifelse(active.sites$methane == 'methane','Methane','Non-Methane')
active.sites$Season_Activity = ifelse(active.sites$Season_Activity == 'All year','Year-Round','Growing Season')

active.sites$status = paste(active.sites$Season_Activity,active.sites$methane,sep = ' ')
active.sites$cluster = ifelse(is.na(active.sites$cluster),30,active.sites$cluster)


pal = brewer.pal(n = 12,name = 'Paired')
pal = pal[c(4,3,2,1)]

#calculate area of clusters
df = as.data.frame(clust)
df$count = 1
library(dplyr)

stat = df %>%
  group_by(cluster) %>%
  summarise(area = sum(count*1853.251^2))

towers = ggplot(data = active.sites)+theme_bw()+
  geom_hline(yintercept = 4,lty=2)+
  geom_hline(yintercept = 1,lty=2)+
  geom_bar(aes(cluster,fill = status))+
  scale_y_continuous(expand = c(0,0),limits = c(0,24),'Number of Tower Sites',
                     breaks = c(1,4,8,12,16,20,24))+
  scale_x_continuous(expand = c(0,0),limits = c(0,41),breaks = seq(1,40),'Cluster')+
  scale_fill_manual(values = pal,'Site Status')+
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size = 8),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.2,0.8))

area = ggplot(data = stat)+theme_bw()+
  geom_bar(aes(x = cluster,y = area/100),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,13000000000),'Area (Ha)')+
  scale_x_continuous(expand = c(0,0),limits = c(0,41),breaks = seq(1,40),'Cluster')+
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size = 8))

png(filename = './figures/sites per cluster.png',width = 6,height = 5,units = 'in',res = 1500)
plot_grid(towers,area,nrow=2,labels = c('a','b'),align = 'hv',label_size = 8)
dev.off()
