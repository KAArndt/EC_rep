#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

library(terra)
library(data.table)
library(foreach)
library(doParallel)
library(doSNOW)

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')

#aggregate the raster to save dataspace
r = terra::aggregate(x = r,fact = 5,fun = 'mean',cores=10,na.rm=T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#create data frame from PCAs
df = as.data.frame(x = r,xy = T,na.rm = T)
#xy = df[,c(1,2)]
#df = df[,-c(1,2)]

#################################################################################
#calculate the euclidean distance for the whole data set 
#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(df)
pca.towers = data.table(tower.data[,c('pc1','pc2','pc3','pc4')])

#pre-populating the whole matrix makes computation time much faster *DO NOT USE A DATAFRAME
rm(r)
rm(df)

#initialize the euclid
euclid = vector(length = nrow(pca.dt))

#setup parallel backend to use many processors
{orig = Sys.time() #start the clock for timing the process
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores[1]-2) #assign X less than total cores to leave some processing for other tasks
registerDoSNOW(cl) #register the cores

#run the ED calculations in parallel (~3 minutes with 18 cores)
euci = foreach (j = 1:nrow(pca.towers),.verbose = T,.combine = cbind) %dopar% {
   for (i in 1:nrow(pca.dt))  {
      euclid[i] = sqrt((pca.dt$PC1[i]-pca.towers$pc1[j])^2 +
                       (pca.dt$PC2[i]-pca.towers$pc2[j])^2 +
                       (pca.dt$PC3[i]-pca.towers$pc3[j])^2 +
                       (pca.dt$PC4[i]-pca.towers$pc4[j])^2)}
  euclid} #report out the loops above to be included
stopCluster(cl) #stop the clusters
Sys.time() - orig} #stop the clock

#save the euclidean distance
colnames(euci) = tower.data$site

#save the file, rds saves alot of space
saveRDS(object = euci,file = './data/euci_5km.rds')
