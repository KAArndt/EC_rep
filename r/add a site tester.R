rm(list=setdiff(ls(), "euci"))

library(readr)
library(terra)
library(data.table)
library(svMisc)
library(ggplot2)
# library(sf)
# library(dplyr)
# library(cowplot)
# library(ggspatial)

#load back in
euci = read_rds('./euclidean_distance_matrix/euci_2kmv2.rds')

#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')
tower.data$active = ifelse(is.na(tower.data$active),'extension',tower.data$active)

#adding or subtracting an additional site
tower.data$site

#adding sites
#Pleistocene park ###################
tower.data$active = ifelse(tower.data$site == 'Chersky, Pleistocene Park',
                           'active',tower.data$active)

#find columns which are active sites
net = which(tower.data$active == 'active')
euci.net = euci[,c(net)]

#create a new improved with the extra site
#calculate the base network, parallel processing is much slower here
improved.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    improved.dist[i] = min(euci.net[i,])
  }
  Sys.time() - orig} #stop the clock

#make the improved image
improvedf = data.frame(df$x,df$y,improved.dist)
improved = rast(x = improvedf,type = 'xyz',crs = crs(r))

#reload in the base image for comparison
base = rast('./output/improved_base_2kmv2_min.tif')

#calculate the difference
dif = improved-base

plot(dif)

#calculate mean improvements
means    = as.numeric(global(dif,'mean',na.rm=T))
sitename = 'Chersky, Pleistocene Park'
country  = 'Russia'
type     = ''

new.bars = data.frame(means,sitename,country,type)


#load back in old importance points and merge with new points
bars = read.csv(file = './output/meanreduction_remaining.csv')
bars = rbind(bars,new.bars)

upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')


#spatial plots
plot(difs[[1]])
plot(difs[[2]])
