library(terra)
library(ggplot2)


#load in all the spatial data
#rep distances
gsco2 = rast('./output/improved_network/improved_base_2km.tif')
gsch4 = rast('./output/improved_network/improved_methane_2km.tif')
anco2 = rast('./output/improved_network/improved_annual_2km.tif')
anch4 = rast('./output/improved_network/improved_annual_methane_2km.tif')

#clusters
clust = rast('./output/clusts.tif')
clust = clust$km40

#pca results
pca = rast('./spatial_data/pca_2km.tif')
pca = crop(x = pca,y = clust)

#environmental data
r = rast('./spatial_data/spatial_repro_2km.tif')
r = crop(x = r,y = clust)

#merge all into one stack
all = c(clust,pca,r,gsco2,gsch4,anco2,anch4)

#aggregate to make plottig and playing with data more manageable
ag = aggregate(x = all,fact = 10,fun = mean, verbose=T,cores = 6)

#extract all as a data frame
df = as.data.frame(x = ag,xy=T)

eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')
eco = crop(x = eco,y = c(-180,180,40,90))
eco = subset(eco,eco$BIOME_NAME == 'Tundra' | eco$BIOME_NAME == 'Boreal Forests/Taiga')

eco = project(x = eco,y = crs(ag))
plot(eco)

#play with data
ggplot(data = df)+
    geom_point(aes(MeanTemp,Precip,colour = ))

