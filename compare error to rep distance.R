#clear environment and memory
rm(list = ls())
gc()

#libraries needed
library(terra)
library(ggplot2)

#load in the base and error rasters
base = rast(x = './output/base_2km.tif')
nee.error = rast(x = './data/input data/NEE_mean_sd_across_years_use_for_uncertainty.tif')
nee.rel.error = rast(x = './data/input data/NEE_mean_sd_across_years_use_for_uncertainty_relative_to_mean.tif')

#check out the resolution, crs, etc
base
nee.error
nee.rel.error
nee = c(nee.error,nee.rel.error)
names(nee) = c('sd','relsd')

#project the nee error to the same as the base image for comparison
nee = project(x = nee,y = base,method = 'bilinear')

#stack both images on top of eachother
r = c(base,nee)
r = aggregate(x = r,FUN = mean,na.rm=T,fact = 3)
r

#make a data frame from the above and remove where there's NAs
df = as.data.frame(r)
df = df[complete.cases(df$base),]
df = df[complete.cases(df$sd),]

hist(df$base.dist)
hist(df$sd)

#plot to expore relationship
ggplot(data = df)+
  geom_hex(aes(base.dist,sd),bins=150)+
  scale_fill_viridis_c()

ggplot(data = df)+
  geom_hex(aes(base.dist,relsd),bins=150)+
  scale_fill_viridis_c()+
  scale_y_continuous(limits = c(0,200))

hist(df$relsd,xlim = c(0,20),breaks=1000)

plot(r$base.dist,range = c(0,4))
plot(r$sd,range = c(0,40))
plot(r$relsd,range = c(1,100))

