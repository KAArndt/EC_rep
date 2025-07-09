rm(list=setdiff(ls(), "euci"))

library(readr)
library(terra)
library(data.table)
library(ggplot2)
library(svMisc)
library(sf)
library(dplyr)
library(cowplot)
library(ggspatial)
library(kit)

#load back in
euci = read_rds('./euclidean_distance_matrix/euci_2km.rds')

#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')

tower.data$order = seq(1,length(tower.data$MeanTemp)) #important for merging back the tower order

#First addition ############################################################################################
#ranking of sites
ranks = read.csv(file = './output/reductions/meanreduction_methane.csv')
ranks$rank = rank(x = ranks$means)
names(ranks)[1] = 'site'
top.limit = max(ranks$means*-1)+0.005

ggplot(data = ranks)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(site, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,top.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

tower.data = merge(tower.data,ranks,by = 'site',all.x=T)
tower.data = tower.data[order(tower.data$order),]

#add the #1 site
name = subset(tower.data,tower.data$rank == 1)$site
tower.data$methane  = ifelse(tower.data$site == name,'methane',tower.data$methane)

#find columns which are active sites
net = which(tower.data$active == 'active' & tower.data$methane == 'methane')
ext = which(tower.data$active == 'active' & tower.data$methane != 'methane')

#save off
tower.data = tower.data[,-c('rank','country','means','order','type')]
write_csv(x = tower.data,'./data/next_5_sites/methane_1.csv')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#calculate the base network
num=2
base.dist = numeric(length = nrow(euci.net))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net)) {
    base.dist[i] = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(df$x,df$y,base.dist)
base = rast(x = basedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
base.towers = tower.data[net,]
towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))

orig = rast('./output/improved_network/improved_methane_2km.tif')

plot(base,range=c(0,4.5))
points(towers,col='red')

new = subset(towers,towers$site == name)
plot(orig - base,range=c(0.1,1.15))
points(new,col='red')

#save the base here
writeRaster(x = base,filename = './output/improved_network/next_five_sites/improved_methane_2km_1.tif',overwrite = T)

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)
num = 2

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(12) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  eucis = foreach (j = 1:ncol(euci.ext),.verbose = T,.combine = cbind,.packages = c('kit')) %dopar% {
    temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
    for (i in 1:nrow(euci.net)) {
      dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
    }
    dist}
  stopCluster(cl) #stop the clusters
  Sys.time() - orig}

#save off this file for later use ###########################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/ext_eucis_methane_1_tura.rds')
#eucis = read_rds(file = './euclidean_distance_matrix/ext_eucis_methane_1_tura.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base
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

#add other parts of the dataframe back in
bars = data.frame(tower.data$site[ext])
bars$means = meansv
bars$country = tower.data$Country[ext]

tower.data$type = paste(tower.data$active,tower.data$methane,tower.data$Season_Activity,sep = '_')
bars$type = tower.data$type[ext]
names(bars)[1] = 'sitename'

#bars = fread('./output/meanreduction_remaining_mean.csv')
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './output/reductions/meanreduction_remaining_methane_1.csv',row.names = F)

#################################################################################################