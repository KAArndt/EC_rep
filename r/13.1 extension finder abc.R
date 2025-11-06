rm(list=setdiff(ls(), "euci"))

library(readr)
library(terra)
library(data.table)
library(svMisc)
library(ggplot2)
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
tower.data$order = seq(1,nrow(tower.data))

#load ABC Flux
abc = fread('./data/ABCFlux.forKyle.csv')
abc = subset(abc,abc$dataset == 'Terrestrial')

#merge based on abcflux
names(tower.data)[25] = 'abc.site'
names(abc)[1]         = 'abc.site'

abcf = merge(x = abc,y = tower.data,all.y = T,by = 'abc.site')
abcf = abcf[!duplicated(abcf$site),]
abcf = abcf[order(abcf$order),]

abcf$active = ifelse(is.na(abcf$active),'extension',abcf$active)
abcf$end_year = ifelse(is.na(abcf$end_year),0,abcf$end_year)

##########################################################################
#   GROWING SEASON CO2
##########################################################################

#find columns which are active sites
net = which(abcf$active == 'active' & as.numeric(abcf$end_year) >= 2022)
ext = which(abcf$active == 'active' & as.numeric(abcf$end_year) < 2022)

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again pre-making vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
#eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)
num = 2

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(10) #assign number of cores
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
    
#   for (j in 1:ncol(euci.ext)) {
#   #create a temp matrix with the base distances and the site of interest
#   temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
#   for (i in 1:nrow(df)) {
#     dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
#   }
#   eucis[,j] = dist
#   progress(j,ncol(euci.ext))
# }
# Sys.time() - orig}

#save off this file for later use ############################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')
#eucis = read_rds(file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#load in the base
base = rast('./output/abc_network/abc_base_2km.tif')

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
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
 # scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './data/reductions/meanreduction_abc_gsco2.csv',row.namexs = F)
###############################################################################################
#    GROWING SEASON CH4
################################################################################

#find columns which are active sites
net = which(abcf$active == 'active' & as.numeric(abcf$end_year) >= 2022 & abcf$methane == 'methane')
ext = which(abcf$active == 'active' & as.numeric(abcf$end_year) < 2022  & abcf$methane == 'methane')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again pre-making vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
#eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)
num = 2

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(10) #assign number of cores
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

#   for (j in 1:ncol(euci.ext)) {
#   #create a temp matrix with the base distances and the site of interest
#   temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
#   for (i in 1:nrow(df)) {
#     dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
#   }
#   eucis[,j] = dist
#   progress(j,ncol(euci.ext))
# }
# Sys.time() - orig}

#save off this file for later use ############################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')
#eucis = read_rds(file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#load in the base
base = rast('./output/abc_network/abc_methane_2km.tif')

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
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  # scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './data/reductions/meanreduction_abc_gsch4.csv',row.names = F)
###############################################################################################
#    YEAR ROUND CO2
###############################################################################################

#find columns which are active sites
testnet = which(abcf$active == 'active' & abcf$Season_Activity == 'All year')

net = which(abcf$active == 'active' & as.numeric(abcf$end_year) >= 2022 & abcf$Season_Activity == 'All year')
ext = which(abcf$active == 'active' & as.numeric(abcf$end_year) < 2022  & abcf$Season_Activity == 'All year')
37+17
tower.data$site[testnet]
testnet
net
#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again pre-making vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
#eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)
num = 2

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(10) #assign number of cores
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

#   for (j in 1:ncol(euci.ext)) {
#   #create a temp matrix with the base distances and the site of interest
#   temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
#   for (i in 1:nrow(df)) {
#     dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
#   }
#   eucis[,j] = dist
#   progress(j,ncol(euci.ext))
# }
# Sys.time() - orig}

#save off this file for later use ############################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')
#eucis = read_rds(file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#load in the base
base = rast('./output/abc_network/abc_annual_2km.tif')

difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base
  progress(i,length(dist.rasts))
}

hist(base)
plot(base,range=c(0,5))
plot(difs[[1]])

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
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  # scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './data/reductions/meanreduction_abc_annualco2.csv',row.names = F)
###############################################################################################
#   YEAR ROUND CH4
###############################################################################################
#find columns which are active sites
abcf$annualmethane = paste(abcf$Season_Activity,abcf$methane,sep = '_')

net = which(abcf$active == 'active' & as.numeric(abcf$end_year) >= 2022 & abcf$annualmethane == 'All year_methane')
ext = which(abcf$active == 'active' & as.numeric(abcf$end_year) < 2022  & abcf$annualmethane == 'All year_methane')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again pre-making vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
#eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)
num = 2

#set up parallel processing
library(foreach)
library(doParallel)
library(doSNOW)

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(10) #assign number of cores
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

#   for (j in 1:ncol(euci.ext)) {
#   #create a temp matrix with the base distances and the site of interest
#   temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
#   for (i in 1:nrow(df)) {
#     dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
#   }
#   eucis[,j] = dist
#   progress(j,ncol(euci.ext))
# }
# Sys.time() - orig}

#save off this file for later use ############################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')
#eucis = read_rds(file = './euclidean_distance_matrix/ext_eucis_2km_methane.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

#load in the base
base = rast('./output/abc_network/abc_annual_methane_2km.tif')

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
names(bars)[1] = 'sitename'

top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  # scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './data/reductions/meanreduction_abc_annualch4.csv',row.names = F)
###############################################################################################

