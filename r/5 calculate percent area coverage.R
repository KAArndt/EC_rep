rm(list = ls())

library(terra)
library(dplyr)

base    = rast('./output/base_2kmv2.tif')
methane = rast('./output/methane_2kmv2.tif')
annual  = rast('./output/annual_2kmv2.tif')
anmeth  = rast('./output/annual_methane_2kmv2.tif')

clust = rast('./output/clusts.tif')
clust = clust$km40

names(methane) = 'methane'
names(annual)  = 'annual'
names(anmeth)  = 'anmeth'

all = c(base,methane,annual,anmeth,clust)

df = as.data.frame(x = all)
summary(df)

er1 = 1.67
er4 = 1.54

df$base.er1 = ifelse(df$base.dist <= er1,1,0)
df$base.er4 = ifelse(df$base.dist <= er4,1,0)

df$methane.er1 = ifelse(df$methane <= er1,1,0)
df$methane.er4 = ifelse(df$methane <= er4,1,0)

df$annual.er1 = ifelse(df$annual <= er1,1,0)
df$annual.er4 = ifelse(df$annual <= er4,1,0)

df$anmeth.er1 = ifelse(df$anmeth <= er1,1,0)
df$anmeth.er4 = ifelse(df$anmeth <= er4,1,0)

df$all = 1

summary = df %>%
  summarise(base.er1 = sum(base.er1)/sum(all),
            base.er4 = sum(base.er4)/sum(all),
            methane.er1 = sum(methane.er1)/sum(all),
            methane.er4 = sum(methane.er4)/sum(all),
            annual.er1 = sum(annual.er1)/sum(all),
            annual.er4 = sum(annual.er4)/sum(all),
            anmeth.er1 = sum(anmeth.er1)/sum(all),
            anmeth.er4 = sum(anmeth.er4)/sum(all))



