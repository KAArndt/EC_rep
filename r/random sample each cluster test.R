
library(data.table)
#library(raster)
#library(svMisc)
library(ggplot2)
library(terra)
#library(seegSDM)
library(dplyr)
library(foreach)
library(doParallel)
library(doSNOW)
library(kit)

#function to filter to 4 of every value from each cluster ########################################
filter_df_by_vector <- function(df, column, values_to_filter, n = 4) {
  
  # Ensure the specified column exists in the data frame
  if (!column %in% names(df)) {
    stop("The specified column does not exist in the data frame.")
  }
  
  # Initialize an empty data frame to store the result
  result_df <- data.frame()
  
  # Loop through each unique value in the `values_to_filter` vector
  for (val in unique(values_to_filter)) {
    
    # Get all rows from the data frame where the column equals the current value
    subset_df <- df[df[[column]] == val, ]
    
    # Check if there are enough rows to sample from
    if (nrow(subset_df) >= n) {
      
      # Randomly sample 'n' rows from the subset
      sampled_rows <- subset_df[sample(nrow(subset_df), n, replace = FALSE), ]
      
      # Append the sampled rows to the result data frame
      result_df <- rbind(result_df, sampled_rows)
      
    } else {
      
      # If there aren't enough rows, print a warning and skip
      warning(paste("Not enough rows for value:", val, ". Found:", nrow(subset_df), ". Expected:", n, ". Skipping this value."))
    }
  }
  
  return(result_df)
}
###################################################################################

#gh_install_packages("SEEG-Oxford/seegSDM")
#devtools::install_github('SEEG-Oxford/seegSDM')

#load in sites
#tower.data = fread(file = './data/pca.towers.base.csv')
#active = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#set just the coordinates for the extract
#xy.tower = active[,c(58,59)]

#clusters #########################################################################
#load in the stack created in the other files
clust = rast('./output/clusts.tif')
clust = clust$km40
names(clust) = 'cluster'

#load in the pca spatial data
r = rast('./spatial_data/pca_2km.tif')
r = crop(x = r,y = ext(clust))
spat = c(clust,r)

#make a data.frame
spat.df = as.data.frame(x = spat,xy=T)



#create random samples with 1 tower in each cluster
samp1 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ] #randomly sample 1000 points
samp1 = samp1[!duplicated(samp1$cluster),]                        #only take data where cluster is unique (1 per cluster)
samp1 = samp1[complete.cases(samp1$cluster),]                     #remove any NAs

samp2 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp2 = samp2[!duplicated(samp2$cluster),]
samp2 = samp2[complete.cases(samp2$cluster),]

samp3 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp3 = samp3[!duplicated(samp3$cluster),]
samp3 = samp3[complete.cases(samp3$cluster),]

samp4 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp4 = samp4[!duplicated(samp4$cluster),]
samp4 = samp4[complete.cases(samp4$cluster),]

samp5 = spat.df[sample(nrow(spat.df), size = 1000,replace = F), ]
samp5 = samp5[!duplicated(samp5$cluster),]
samp5 = samp5[complete.cases(samp5$cluster),]

#plot to check they are all unique
plot(clust)
points(samp1$x,samp1$y,col='red',pch=16)
points(samp2$x,samp2$y,col='blue',pch=16)
points(samp3$x,samp3$y,col='green',pch=16)
points(samp4$x,samp4$y,col='purple',pch=16)
points(samp5$x,samp5$y,col='yellow',pch=16)

#process representation for each random set

#sample 1 ######################################################################
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp1),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp1$PC1[j])^2 +
                         (spat.df$PC2[i]-samp1$PC2[j])^2 +
                         (spat.df$PC3[i]-samp1$PC3[j])^2 +
                         (spat.df$PC4[i]-samp1$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
num = 2
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er1.1 = rast(x = basedf,type = 'xyz',crs = crs(r))
##########################################################################################

#sample 2
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp2),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp2$PC1[j])^2 +
                         (spat.df$PC2[i]-samp2$PC2[j])^2 +
                         (spat.df$PC3[i]-samp2$PC3[j])^2 +
                         (spat.df$PC4[i]-samp2$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er1.2 = rast(x = basedf,type = 'xyz',crs = crs(r))

##########################################################################################

#sample 3
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp3),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp3$PC1[j])^2 +
                         (spat.df$PC2[i]-samp3$PC2[j])^2 +
                         (spat.df$PC3[i]-samp3$PC3[j])^2 +
                         (spat.df$PC4[i]-samp3$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er1.3 = rast(x = basedf,type = 'xyz',crs = crs(r))


##########################################################################################

#sample 4
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp4),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp4$PC1[j])^2 +
                         (spat.df$PC2[i]-samp4$PC2[j])^2 +
                         (spat.df$PC3[i]-samp4$PC3[j])^2 +
                         (spat.df$PC4[i]-samp4$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er1.4 = rast(x = basedf,type = 'xyz',crs = crs(r))

##########################################################################################

#sample 5
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(samp5),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-samp5$PC1[j])^2 +
                         (spat.df$PC2[i]-samp5$PC2[j])^2 +
                         (spat.df$PC3[i]-samp5$PC3[j])^2 +
                         (spat.df$PC4[i]-samp5$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er1.5 = rast(x = basedf,type = 'xyz',crs = crs(r))
##############################################################################################
base = rast('./output/improved_network/improved_base_2km.tif')

#combine all into 1
ers = c(er1.1,er1.2,er1.3,er1.4,er1.5)


hist(ers-base)
summary(ers)
plot(ers-base)

writeRaster(x = ers,'./output/er1_rasters.tif')

# 4 per cluster #####################################################################################
newdf1 = filter_df_by_vector(df = spat.df,column = 'cluster',values_to_filter = 1:40,n = 4)
newdf2 = filter_df_by_vector(df = spat.df,column = 'cluster',values_to_filter = 1:40,n = 4)
newdf3 = filter_df_by_vector(df = spat.df,column = 'cluster',values_to_filter = 1:40,n = 4)
newdf4 = filter_df_by_vector(df = spat.df,column = 'cluster',values_to_filter = 1:40,n = 4)
newdf5 = filter_df_by_vector(df = spat.df,column = 'cluster',values_to_filter = 1:40,n = 4)

#plot to check they are all unique
plot(clust)
points(newdf1$x,newdf1$y,col='red',pch=16)
points(newdf2$x,newdf2$y,col='blue',pch=16)
points(newdf3$x,newdf3$y,col='green',pch=16)
points(newdf4$x,newdf4$y,col='purple',pch=16)
points(newdf4$x,newdf4$y,col='orange',pch=16)


#process representation for each random set

#sample 1 ######################################################################
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(newdf1),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-newdf1$PC1[j])^2 +
                         (spat.df$PC2[i]-newdf1$PC2[j])^2 +
                         (spat.df$PC3[i]-newdf1$PC3[j])^2 +
                         (spat.df$PC4[i]-newdf1$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
num = 2
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er4.1 = rast(x = basedf,type = 'xyz',crs = crs(r))
##########################################################################################

#sample 2
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(newdf2),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-newdf2$PC1[j])^2 +
                         (spat.df$PC2[i]-newdf2$PC2[j])^2 +
                         (spat.df$PC3[i]-newdf2$PC3[j])^2 +
                         (spat.df$PC4[i]-newdf2$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er4.2 = rast(x = basedf,type = 'xyz',crs = crs(r))

##########################################################################################

#sample 3
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(newdf3),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-newdf3$PC1[j])^2 +
                         (spat.df$PC2[i]-newdf3$PC2[j])^2 +
                         (spat.df$PC3[i]-newdf3$PC3[j])^2 +
                         (spat.df$PC4[i]-newdf3$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er4.3 = rast(x = basedf,type = 'xyz',crs = crs(r))


##########################################################################################

#sample 4
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(newdf4),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-newdf4$PC1[j])^2 +
                         (spat.df$PC2[i]-newdf4$PC2[j])^2 +
                         (spat.df$PC3[i]-newdf4$PC3[j])^2 +
                         (spat.df$PC4[i]-newdf4$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er4.4 = rast(x = basedf,type = 'xyz',crs = crs(r))

##########################################################################################

#sample 5
#initialize the euclid
euclid = vector(length = nrow(spat.df))
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores-1) #assign number of cores
{orig = Sys.time() #start the clock for timing the process
  registerDoSNOW(cl) #register the cores
  
  #run the ED calculations in parallel (~16 minutes with 30 cores on VM)
  euci = foreach (j = 1:nrow(newdf5),.verbose = T,.combine = cbind) %dopar% {
    for (i in 1:nrow(spat.df))  {
      euclid[i] = sqrt((spat.df$PC1[i]-newdf5$PC1[j])^2 +
                         (spat.df$PC2[i]-newdf5$PC2[j])^2 +
                         (spat.df$PC3[i]-newdf5$PC3[j])^2 +
                         (spat.df$PC4[i]-newdf5$PC4[j])^2)}
    euclid} #report out the loops above to be included
  stopCluster(cl) #stop the clusters
  Sys.time() - orig} #stop the clock

#calculate the base network, parallel processing is much slower here
base.dist = numeric(length = nrow(euci))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci)) {
    base.dist[i] = mean(euci[i,topn(vec = euci[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
basedf = data.frame(spat.df$x,spat.df$y,base.dist)
er4.5 = rast(x = basedf,type = 'xyz',crs = crs(r))




