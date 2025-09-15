#rm(list=setdiff(ls(), "euci"))

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
#subtracting site(s)
tower.data$site
tower.data$active = ifelse(tower.data$site == 'Chersky, control' | tower.data$site == 'Chersky, drained',
                           'subtraction',tower.data$active)

#find columns which are active sites
net = which(tower.data$active == 'active')
ext = which(tower.data$active == 'subtraction')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again pre-making vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#parallel processing also much slower here
{orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
  for (i in 1:nrow(df)) {
     dist[i] = min(temp.euci[i,])
  }
  eucis[,j] = dist
  progress(j,ncol(euci.ext))
}
Sys.time() - orig}


#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#create a new base
#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    base.dist[i] = min(euci.net[i,])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

#calculate difference maps
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = global(difs[[i]],'mean',na.rm=T)
  progress(value = i,max.value = length(difs))
}

meansv = numeric(length = length(difs))
for (i in 1:length(difs)) {
  meansv[i] = c(means[[i]])
}

#create importance plots
new.bars = data.frame(tower.data$site[ext])
new.bars$means = meansv
new.bars$country = tower.data$Country[ext]

tower.data$type = paste(tower.data$active,tower.data$methane,tower.data$Season_Activity,sep = '_')
new.bars$type = tower.data$type[ext]
names(new.bars)[1] = 'sitename'

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
