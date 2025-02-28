
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

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
tower.data = fread(file = './data/pca.towers.base.csv')
active = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#set just the coordinates for the extract
xy.tower = active[,c(56,57)]

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
na.cor = as.data.frame(nearestLand(points = nas[,c(4,5)],raster = clustr,max_distance = 1000000))
summary(na.cor)

#place in original data frame
clustdat[nas$ID,] = extract(x = clust,y = na.cor,cells=T,xy=T)
clustdat$site = active$site
active$cluster = clustdat$cluster

active$status = paste(active$methane,active$Season_Activity,sep = '_')

active$cluster = ifelse(is.na(active$cluster),13,active$cluster)

png(filename = './figures/sites per cluster.png',width = 5,height = 3,units = 'in',res = 1500)
ggplot(data = active)+theme_bw()+
  geom_hline(yintercept = 4,lty=2)+
  geom_hline(yintercept = 1,lty=2)+
  geom_bar(aes(cluster,fill = status))+
  scale_y_continuous(expand = c(0,0),limits = c(0,17),'Number of Tower Sites',
                     breaks = c(1,4,8,12,16))+
  scale_x_continuous(expand = c(0,0),limits = c(0,41),breaks = seq(1,40),'Cluster')+
  scale_fill_brewer(palette = 'Dark2','Status')+
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size = 6),
        legend.key.size = unit(x = 0.1,units = 'in'),
        legend.position = c(0.8,0.8))
dev.off()
