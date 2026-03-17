rm(list=setdiff(ls(), c("euci",'df','r','tower.data')))

library(data.table)
library(readr)
library(terra)
library(kit)
library(dplyr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/final.tower.data.csv')

#write.csv(x = tower.data,file = './data/improved_pca.towersv2.csv')
#load back in euclidean distance matrix
euci = read_rds('./euclidean_distance_matrix/euci_2km.rds')

#load in the other spatial data
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,xy = T,na.rm = T)

num = 2

##########################################################################
# BASE
net = which(tower.data$active.2024 == 'active')
tower.data$site[net]
euci.net = euci[,c(net)]

#calculate the base network, parallel processing is much slower here
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
base.22 = rast('./output/base_network/base_2km.tif')
base.22.towers = subset(tower.data,tower.data$active.2022 == 'active')

#find difference
base.towers = tower.data[net,]
diff_df1 = setdiff(base.towers, base.22.towers)

towers = vect(x = base.towers,geom=c("x", "y"), crs=crs(r))
towers.22 = vect(x = diff_df1,geom=c('x','y'),crs=crs(r))

#hist(base)
plot(base,range=c(0,4.5))
points(towers,col='red')
points(towers.22,col='green')

b1 = as.data.frame(base)
b2 = as.data.frame(base.22)
summary(b1$base.dist-b2$base.dist)

plot(base-base.22)
points(towers,col='green')
points(towers.22,col='red')

#save the base here
writeRaster(x = base,filename = './output/improved_network/improved_base_2km.tif',overwrite = T)
#writeRaster(x = base,filename = './output/improved_network/improved_base_1km.tif',overwrite = T)

#######################################################################################
##################     METHANE
net.methane = which(tower.data$active.2024 == 'active' & tower.data$methane.2024 == 'methane')
euci.net.methane = euci[,c(net.methane)]

#calculate the base network, parallel processing is much slower here
methane.dist = numeric(length = nrow(euci.net.methane))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.methane)) {
    methane.dist[i] = mean(euci.net.methane[i,topn(vec = euci.net.methane[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
methanedf = data.frame(df$x,df$y,methane.dist)
methane = rast(x = methanedf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
methane.22 = rast('./output/base_network/methane_2km.tif')
methane.22.towers = subset(tower.data,tower.data$active.2022 == 'active' & tower.data$methane.2022 == 'methane')

#project the towers d#project the towers d#project the towers database
methane.towers = tower.data[net.methane,]
towers = vect(x = methane.towers,geom=c("x", "y"), crs=crs(r))

#find difference
diff_df1 = setdiff(methane.towers, methane.22.towers)
diff_df1$site
towers.22 = vect(x = diff_df1,geom=c('x','y'),crs=crs(r))

hist(methane)
plot(methane,range=c(0,4.5))
points(towers,col='red')
points(towers.22,col='green')

plot(methane - methane.22)
points(towers.22,col='red')


#save the base here
writeRaster(x = methane,filename = './output/improved_network/improved_methane_2km.tif',overwrite = T)
#writeRaster(x = methane,filename = './output/improved_network/improved_methane_1km.tif',overwrite = T)

##########################################################################################
################ Annual
net.annual = which(tower.data$active.2024 == 'active' & tower.data$Season_Activity.2024 == 'All year')
tower.data$site[net.annual]
euci.net.annual = euci[,c(net.annual)]

#calculate the base network, parallel processing is much slower here
annual.dist = numeric(length = nrow(euci.net.annual))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.annual)) {
    annual.dist[i] = mean(euci.net.annual[i,topn(vec = euci.net.annual[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
annualdf = data.frame(df$x,df$y,annual.dist)
annual = rast(x = annualdf,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers dataannual
annual.towers = tower.data[net.annual,]
towers = vect(x = annual.towers,geom=c("x", "y"), crs=crs(r))

#project the towers d#project the towers d#project the towers database
annual.22 = rast('./output/base_network/annual_2km.tif')
annual.22.towers = subset(tower.data,tower.data$active.2022 == 'active' & tower.data$Season_Activity.2022 == 'All year')

#find difference
diff_df1 = setdiff(annual.towers, annual.22.towers)
diff_df1$site
towers.22 = vect(x = diff_df1,geom=c('x','y'),crs=crs(r))

hist(annual)
plot(annual,range=c(0,4.5))
points(towers,col='red')
points(towers.22,col='green')

plot(annual - annual.22)
points(towers.22,col='red')

#save the annual here
writeRaster(x = annual,filename = './output/improved_network/improved_annual_2km.tif',overwrite = T)
#writeRaster(x = annual,filename = './output/improved_network/improved_annual_1km.tif',overwrite = T)

################################################################################
# Annual Methane
net.annual.methane = which(tower.data$annualmethane2024 == 'annualmethane')
euci.net.annual.methane = euci[,c(net.annual.methane)]

#calculate the base network, parallel processing is much slower here
annual.methane.dist = numeric(length = nrow(euci.net.annual.methane))
{orig = Sys.time() #start the clock for timing the process
  for (i in 1:nrow(euci.net.annual.methane)) {
    annual.methane.dist[i] = mean(euci.net.annual.methane[i,topn(vec = euci.net.annual.methane[i,],n = num,decreasing = F,hasna = F)])
  }
  Sys.time() - orig} #stop the clock

#make the base image
annual.methane.df = data.frame(df$x,df$y,annual.methane.dist)
annual.methane = rast(x = annual.methane.df,type = 'xyz',crs = crs(r))

#project the towers d#project the towers d#project the towers database
annual.methane.towers = tower.data[net.annual.methane,]
towers = vect(x = annual.methane.towers,geom=c("x", "y"), crs=crs(r))

#project the towers d#project the towers d#project the towers database
annualmethane.22 = rast('./output/base_network/annual_methane_2km.tif')
annualmethane.22.towers = subset(tower.data,tower.data$annualmethane2022 == 'annualmethane')

#find difference
diff_df1 = setdiff(annual.methane.towers, annualmethane.22.towers)
diff_df1$site
towers.22 = vect(x = diff_df1,geom=c('x','y'),crs=crs(r))


hist(annual.methane)
plot(annual.methane,range=c(0,4.5))
points(towers,col='red')
points(towers.22,col='green')

plot(annual.methane - annualmethane.22)
points(towers.22,col='red')

#save the base here
writeRaster(x = annual.methane,filename = './output/improved_network/improved_annual_methane_2km.tif',overwrite = T)
#writeRaster(x = annual.methane,filename = './output/improved_network/improved_annual_methane_1km.tif',overwrite = T)
