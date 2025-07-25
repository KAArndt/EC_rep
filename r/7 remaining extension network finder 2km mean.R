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

#load back in
euci = read_rds('./euclidean_distance_matrix/euci_2km.rds')

#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')
tower.data$order = seq(1,length(tower.data$MeanTemp)) #important for merging back the tower order

#ranking of sites
ranks = read.csv(file = './output/reductions/meanreduction.csv')
ranks$rank = rank(x = ranks$means)
names(ranks)[1] = 'site'
top.limit = max(ranks$means*-1)+0.005
top = subset(ranks,ranks$rank<100)

ggplot(data = ranks)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(site, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,top.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

tower.data = merge(tower.data,ranks,by = 'site',all.x=T)
tower.data$active = ifelse(is.na(tower.data$active),'inactive',tower.data$active)
tower.data = tower.data[order(tower.data$order),]

#find columns which are active sites
net = which(tower.data$active == 'active')
ext = which(tower.data$active == 'inactive' & tower.data$rank <= 107)

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

#again premaking vectors and matrices of the right length greatly speeds up comp time
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

#parallel processing also much slower here
# {orig = Sys.time()
# for (j in 1:ncol(euci.ext)) {
#   #create a temp matrix with the base distances and the site of interest
#   temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
#   for (i in 1:nrow(df)) {
#     dist[i] = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
#   }
#   eucis[,j] = dist
#   progress(j,ncol(euci.ext))
# }
# Sys.time() - orig}


#save off this file for later use ###########################################################
#saveRDS(object = eucis,file = './euclidean_distance_matrix/remaining_ext_eucis_2km.rds')
eucis = read_rds(file = './euclidean_distance_matrix/remaining_ext_eucis_2km.rds')

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

# #create a path of file names
# path = paste('./output/remaining_ext/mean/',tower.data$site[ext],'.tif',sep = '')
# #save off rasters
# for (i in 1:length(dist.rasts)) {
#   writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
#   progress(i,length(dist.rasts))
# }

#load back in if not already here #####################################################
 # extpath = list.files(path = './output/remaining_ext/mean/',pattern = '*.tif',full.names = T)
 # dist.rasts = lapply(X = extpath,FUN = rast)

#load in the base
base = rast('./output/improved_network/improved_base_2km.tif')

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

write.csv(x = bars,file = './output/reductions/meanreduction_remaining.csv',row.names = F)

########################################################################################################
# bars = fread('./output/meanreduction_remaining_mean.csv')
# 
# png(filename = './figures/barplot_reduction_remaining_mean.png',width = 6,height = 3,units = 'in',res = 2000)
# ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
#   geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
#   scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
#   scale_x_discrete('Site')+
#   scale_fill_brewer(palette = "Spectral")+
#   theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
#         legend.position = c(0.5,0.9),
#         legend.direction = 'horizontal')
# dev.off()

#################################################################################################
#save off difference maps
#aggregate all the difference plots
# dif.ag = lapply(X = difs,FUN = aggregate,fact = 5,fun = mean,na.rm = T)
# 
# #plot difference maps
# sf_use_s2(FALSE) #need to run this before next line
# countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
#   st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
#   smoothr::densify(max_distance = 1) %>%
#   st_transform(crs(base))
# 
# #plot the figure
# pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')
# extention.towers = tower.data[ext]
# 
# plot_list = list()
# for (i in 1:length(dif.ag)) {
#   p = ggplot()+theme_map()+
#     geom_sf(data = countries,fill='gray',col='gray40')+
#     layer_spatial(dif.ag[[i]])+
#     scale_fill_gradientn('Improvement',
#                          na.value = 'transparent',
#                          colours = pal,
#                          limits = c(-1,0),
#                          breaks = c(-1,-0.5,0),
#                          labels = c('High','Low','None'),
#                          oob = scales::squish)+
#     geom_point(aes(extention.towers$x[i],extention.towers$y[i]),col='black',show.legend = F)+
#     scale_x_continuous(limits = c(-5093909,4542996))+
#     scale_y_continuous(limits = c(-3687122,4374170))+
#     theme(text = element_text(size = 8),
#           legend.text = element_text(size = 8),
#           axis.title = element_blank(),
#           legend.key.height = unit(x = 0.1,units = 'in'),
#           legend.key.width = unit(x = 0.3,units = 'in'),
#           legend.direction = 'horizontal',
#           legend.position = c(0.1,0.05),
#           legend.title.position = 'top')+
#     annotate(geom = 'text',x = -3093909,y = 3374170,label = extention.towers$site[i])
#   plot_list[[i]] = p
#   progress(value = i,max.value = length(dif.ag))
# }
# 
# #plot all the files here, takes awhile
# for (i in 1:length(dif.ag)) {
#   png(filename = paste('./output/remaining_difs/mean/',extention.towers$site[i],'.png',sep = ''),width = 4,height = 4,units = 'in',res = 100)
#   print(plot_list[[i]])
#   dev.off()
#   progress(value = i,max.value = length(dif.ag))
# }
