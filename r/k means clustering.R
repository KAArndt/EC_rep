rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggspatial)
library(terra)
library(foreach)
library(doParallel)
library(doSNOW)
library(cowplot)
library(sf)
library(viridis)
library(Polychrome)

#load in the pca image
r = rast('./data/input data/pca.tif')
r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=12,na.rm=T)
df = as.data.frame(x = r,na.rm = T,xy = T)

#take just the coordinates from column 1 and 2 (will need for transforming back into a raster)
cor = df[,c(1,2)]

#remove the coordinates from the stack
df = df[,-c(1,2)]

#re-scale all variables between 0 and 1 to ensure units are not the deciding factor
#for (i in 1:length(df2)) {df2[,i] = plotrix::rescale(x = df2[,i],newrange = c(0,1))}

#run kmeans clustering in parallel, determine what is the best by running several size clusters
set.seed(100) #set seed so the kmeans always starts in the same spot, makes the results more similar if ran again
cents = c(5,10,20,30,40,50,60,70,80,90,100,200,300,400,500) #set how many clusters we want in the different iterations

#setup parallel back end to use many processors
  cores = detectCores() #detect the number of cores
  cl = makeCluster(length(cents)) #set the clusters to the number of kmeans to be run, or 1 less than total cores
  {orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the kmeans calculations in parallel
  km = foreach (i = cents,.verbose = T) %dopar% {
    kmeans(x = df,centers = i,iter.max = 500,nstart = 10,algorithm = 'Lloyd')}
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock
  
#saveRDS(object = km,file = './output/clusters.rds')
  
km = readRDS(file = './output/clusters.rds')

#error calculations for deciding appropriate amount of clusters
error  = km[[1]]$tot.withinss
for (i in 2:length(km)) {
  error = c(error,km[[i]]$tot.withinss)
}

slope = vector(length = length(error)-1)
for (i in 1:length(error)-1) {
  slope[i] = (error[i]-error[i+1])/(cents[i]-cents[i+1])
}

plot(slope)


#error kmeans plot
png(filename = './figures/kmeans_error.png',width = 5,height = 2.5,units = 'in',res = 2000)
ggplot()+theme_bw()+
  geom_line(aes(cents,error))+
  geom_point(aes(cents,error))+
  scale_x_continuous('Clusters',limits = c(0,510),expand = c(0,0),breaks = cents,
                     labels = c('5','','20','','40','','60','','80','','100','200','300','400','500'))+
  scale_y_continuous('Within-Cluster Sum of Squares Error')+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.minor.x = element_blank())
dev.off()

#rasterize the clusters
cor$km5  = km[[1]]$cluster #add to the coordinates
cor$km10 = km[[2]]$cluster #add to the coordinates
cor$km20 = km[[3]]$cluster #add to the coordinates
cor$km30 = km[[4]]$cluster #add to the coordinates
cor$km40 = km[[5]]$cluster #add to the coordinates
cor$km50 = km[[6]]$cluster #add to the coordinates
cor$km60 = km[[7]]$cluster #add to the coordinates
cor$km70 = km[[8]]$cluster #add to the coordinates
cor$km80 = km[[9]]$cluster #add to the coordinates
cor$km90 = km[[10]]$cluster #add to the coordinates
cor$km100 = km[[11]]$cluster #add to the coordinates
cor$km200 = km[[12]]$cluster #add to the coordinates
cor$km300 = km[[13]]$cluster #add to the coordinates
cor$km400 = km[[14]]$cluster #add to the coordinates
cor$km500 = km[[15]]$cluster #add to the coordinates

#create rasters from the cluster files
kms = rast(x = cor,type = 'xyz',crs = crs(r))

#check it out
plot(kms)

#save off so it can just be reloaded again
writeRaster(x = kms,filename = './output/clusts.tif',overwrite = T)

# already run ######################################
kms = rast(x = './output/clusts.tif') 
kms
#just for plotting of k means ##############################################################
#aggregate for better plotting
kmag = terra::aggregate(x = kms, fact = 3,fun = 'modal',na.rm=T)

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 35, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(kms))

kmdf = as.data.frame(kmag,xy=T)
pal = Polychrome::alphabet.colors(n = 20)
pal = c(pal,'gray10','gray30','gray60','gray80')
pal_function = colorRampPalette(colors = pal)

kmdf$group = as.factor(kmdf$km20)
num_colors = nlevels(kmdf$group)
diamond_color_colors = pal_function(num_colors)

png(filename = './figures/cluster_map.png',width = 4,height = 3.2,units = 'in',res = 2000)
ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
#  layer_spatial(kmag$km20)+
#  scale_fill_gradientn(na.value = NA,'Cluster',colors = pal)+
  geom_raster(data = kmdf,aes(x,y,fill=factor(km20)))+
  scale_fill_manual('Cluster',values = diamond_color_colors)+
  scale_x_continuous(limits = c(-5093909,4539289))+
  scale_y_continuous(limits = c(-3523458,4375097))+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(x = 0.05,units = 'in'))
dev.off()
