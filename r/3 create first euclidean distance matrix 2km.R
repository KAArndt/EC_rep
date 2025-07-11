
library(terra)
library(data.table)
library(foreach)
library(doParallel)
library(doSNOW)

#load in extracted site data from extraction codes, can be base or upgraded since active, methane etc doesn't matter here
tower.data = fread(file = './data/pca.towers.base.csv')
r = rast('./spatial_data/pca_2km.tif')

#create data frame from PCAs
df = as.data.frame(x = r,xy = T,na.rm = T)

#################################################################################
#calculate the euclidean distance for the whole data set 
#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(df)
pca.towers = data.table(tower.data[,c('pc1','pc2','pc3','pc4')])

#initialize the euclid
euclid = vector(length = nrow(pca.dt))

#setup parallel back end to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
registerDoSNOW(cl) #register the cores

#run the ED calculations in parallel (~16 minutes with 30 cores on VM)
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
saveRDS(object = euci,file = './euclidean_distance_matrix/euci_2km.rds')
