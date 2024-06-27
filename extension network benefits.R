rm(list = ls())

library(readr)

#load back in
euci = read_rds('./data/euci.rds')

#load in the stack created in the other file
r = rast('./data/input data/pca.tif')
df = as.data.frame(x = r,na.rm = T)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')
pca.towers1 = tower.data

#find columns which are active sites
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

#calculate the euc distance of the network and one extension site
#setup parallel backend to use many processors
cores = detectCores()        #detect the number of cores
cl = makeCluster(cores[1]-1) #assign X less than total cores to leave some processing for other tasks
{orig = Sys.time() #start the clock for timing the process
registerDoSNOW(cl) #register the cores
  
#run the ED calculations in parallel
eucis = foreach (j = 1:ncol(euci.ext),.verbose = T,.combine = cbind) %dopar% {
  #create a temp matrix with the base distances and the site of interest
  temp.euci = cbind(euci.net[,1:ncol(euci.net)],euci.ext[,j]) 
    for (i in 1:nrow(pca.dt))  {
      dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])}
      dist} #report out the loops above to be included
stopCluster(cl) #stop the clusters
Sys.time() - orig} #stop the clock




orig = Sys.time()
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
Sys.time() - orig
