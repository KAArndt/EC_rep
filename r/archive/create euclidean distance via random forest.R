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
library(kit)

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')

#aggregate the raster to save dataspace
#r = terra::aggregate(x = r,fact = 2,fun = 'mean',cores=10,na.rm=T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')

#create data frame from PCAs
sr = spatSample(x = r,size = 1000000,method = "regular",xy = T)
sr = sr[complete.cases(sr$PC1),]

#################################################################################
#calculate the euclidean distance for the whole data set 
#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(sr)
pca.towers = data.table(tower.data[,c('pc1','pc2','pc3','pc4','pc5')])

#initialize the euclid
euclid = vector(length = nrow(pca.dt))

#setup parallel backend to use many processors
{orig = Sys.time() #start the clock for timing the process
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores[1]-2) #assign X less than total cores to leave some processing for other tasks
registerDoSNOW(cl) #register the cores

#run the ED calculations in parallel
euci = foreach (j = 1:nrow(pca.towers),.verbose = T,.combine = cbind) %dopar% {
   for (i in 1:nrow(pca.dt))  {
      euclid[i] = sqrt((pca.dt$PC1[i]-pca.towers$pc1[j])^2 +
                       (pca.dt$PC2[i]-pca.towers$pc2[j])^2 +
                       (pca.dt$PC3[i]-pca.towers$pc3[j])^2 +
                       (pca.dt$PC4[i]-pca.towers$pc4[j])^2 +
                       (pca.dt$PC5[i]-pca.towers$pc5[j])^2)}
  euclid} #report out the loops above to be included
stopCluster(cl) #stop the clusters
Sys.time() - orig} #stop the clock

#################################################################
#### first go to base image #####################################
#################################################################
pca.towers1 = tower.data
pca.towers1[,c('site','Activity')]

#find columns which are active sites
net = which(pca.towers1$Activity == 'active')

#create some subsets of the euclidean distance tables for easier calculations
euci.net = euci[,c(net)]
#rm(euci)

#calculate based on the mean of the x lowest + site of interest
num = 2 #how many closest towers you want

#calculate the base network
base.dist = numeric(length = nrow(euci.net))
for (i in 1:nrow(euci.net)) {
  base.dist[i]    = mean(euci.net[i,topn(vec = euci.net[i,],n = num,decreasing = F,hasna = F)])
}

pca.dt$base = base.dist
sample = sample(c(TRUE, FALSE), nrow(pca.dt), replace=TRUE, prob=c(0.1,0.9))
train  = pca.dt[sample, ]

#create random forest model
library(randomForest)
#train a random forest model, Keep forest is needed for predict, 
#reformulate needed to tell it what the response variable is in predict
rf = randomForest(reformulate(response = 'base',names(train)[3:7]),ntree = 100,keep.forest = T,data = train)

eucdist = predict(object = r,model = rf)

plot(eucdist$lyr1,range = c(0,3))

#save the euclidean distance map
writeRaster(x = eucdist,file = './out/base.tif')
