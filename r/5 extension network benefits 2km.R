rm(list = ls())
gc()

library(readr)
library(terra)
library(kit)
library(svMisc)

#load back in
euci = read_rds('./data/euci_2kmv2.rds')

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=12,na.rm=T)
df = as.data.frame(x = r,na.rm = T,xy = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
pca.towers1 = tower.data

#find columns which are active sites
pca.towers1$Activity = ifelse(pca.towers1$site == 'Churchill Fen' | pca.towers1$site == 'Iqaluit',
                              'inactive',pca.towers1$Activity)
net = which(pca.towers1$Activity == 'active')
ext = which(pca.towers1$Activity != 'active' | is.na(pca.towers1$Activity))

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
euci.ext = euci[,c(ext)]

rm(euci)
gc()

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.net)+1)

#parallel processing also much slower here
{orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
  for (i in 1:nrow(df)) {
    dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
    # dist[i] = min(temp.euci[i,])
  }
  eucis[,j] = dist
  progress(j,ncol(euci.ext))
}
Sys.time() - orig}

# #manually add pond inlet which was missed
# pi = cbind(euci.net,euci[,179])
# pi.dist = vector(length = nrow(df))
# for (i in 1:nrow(df)) {
#   pi.dist[i]    = mean(pi[i,topn(vec = pi[i,],n = num,decreasing = F,hasna = F)])
# }

#create rasters
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rast(x = tempdf,type = 'xyz',crs = crs(r))
  progress(i,ncol(eucis))
}

# pidf = cbind(df[,c(1,2)],pi.dist)
# pi.rasts = rast(x = pidf,type = 'xyz',crs = crs(r))

#create a path of file names
path = paste('./output/ext/',pca.towers1$site[ext],'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}

 # path = './output/ext/Pond Inlet.tif'
 # writeRaster(x = pi.rasts,filename = path,overwrite=T)

#calculate differences
extpath = list.files(path = './output/ext',pattern = '*.tif',full.names = T)
dist.rasts = lapply(X = extpath,FUN = rast)

#load in the base
base = rast('./output/base_2km.tif')

difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/difs/',pca.towers1$site[ext],'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
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
bars = data.frame(pca.towers1$site[ext])
bars$means = meansv
bars$country = pca.towers1$Country[ext]

pca.towers1$type = paste(pca.towers1$Activity,pca.towers1$CH4,pca.towers1$Annual_cover,sep = '_')
bars$type = pca.towers1$type[ext]
names(bars)[1] = 'sitename'


#top = subset(bars,bars$means < median(bars$means))
upper.limit = -1*min(bars$means)+0.005

ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,size = 7),
        legend.position = c(0.5,0.9),
        legend.direction = 'horizontal')

write.csv(x = bars,file = './output/meanreduction.csv',row.names = F)


########################################################################################################
bars = fread('./output/meanreduction.csv')
top = subset(bars,bars$means < mean(bars$means))

png(filename = './figures/barplot_reduction.png',width = 6,height = 3,units = 'in',res = 2000)
ggplot(data = bars)+theme_bw()+ggtitle('Mean Improvements')+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,upper.limit),'Mean ED Reduction')+
  scale_x_discrete('')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_blank(),
        legend.key.size = unit(0.01,units = 'in'),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.position = c(0.6,0.6),
        legend.direction = 'horizontal')
dev.off()
