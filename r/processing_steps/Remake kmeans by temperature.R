library(terra)
library(dplyr)

#clusters
clust = rast('./output/clusts_2km.tif')
clust = clust$km40

#environmental data
r = rast('./spatial_data/spatial_repro.tif')
r = r$MeanTemp
r = aggregate(x = r,fact=2,na.rm=T,fun='mean')
r = crop(x = r,y = clust)

#merge all into one stack
all = c(clust,r)

#extract all as a data frame
df = as.data.frame(x = all,xy=T)
df = df[complete.cases(df$km40),]

#make the cluster a character for plotting
df$km40 = as.character(df$km40)

#re-order clusters from 1-40 based on mean temp
aves = df %>%
  group_by(km40) %>%
  summarise(temp = mean(MeanTemp))

aves = aves[order(aves$temp),]
aves$newkm = seq(1:40)
aves = aves[,-2]

df = merge(df,aves,by = 'km40')
df$newkm = as.character(df$newkm)

km40 = rast(x = df[,c(2,3,5)],type = 'xyz',crs=crs(clust))

plot(km40)

writeRaster(x = km40,filename = './spatial_data/km40.tif')


