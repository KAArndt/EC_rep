
library(data.table)
library(ggplot2)
library(terra)
library(dplyr)
library(plotrix)

#load in tower data
df = fread('./data/pca.towers.upgraded.csv')

#load in PCA data
#pca = rast('./spatial_data/pca_2km.tif')

#load in environmental data
r = rast('./spatial_data/spatial_repro.tif')
r = aggregate(x = r,fact = 2,fun = 'mean',na.rm=T)

cluster = rast('./output/clusts.tif')

#stack together
stack = c(r,cluster)

#make a data frame
sdf = as.data.frame(stack,xy=T)

active = subset(df,df$active == 'active')
ch4    = subset(df,df$active == 'active' & df$methane == 'methane')
active = subset(df,df$active == 'active')
active = subset(df,df$active == 'active')


ggplot()+
  geom_hex(data = sdf,aes(MeanTemp,Precip),bins=100)+
  scale_fill_viridis_c()+
  geom_point(data = active,aes(MeanTemp,Precip,color = methane,pch = Season_Activity))+
  scale_color_manual(values = c('red','green'))


ggplot()+
  geom_hex(data = sdf,aes(ndvimax,OCSTHA_M_100cm_1km_ll),bins=100)+
  scale_fill_viridis_c()+
  geom_point(data = active,aes(NDVImax,CStock,color = methane,pch = Season_Activity))+
  scale_color_manual(values = c('red','green'))



ggplot()+
  geom_hex(data = sdf,aes(mirsaug,ndwimin),bins=100)+
  scale_fill_viridis_c()+
  geom_point(data = active,aes(SWIRaug,NDWImin,color = methane,pch = Season_Activity))+
  scale_color_manual(values = c('red','green'))



ggplot(data = sdf)+theme_bw()+geom_hline(yintercept = 0)+
  geom_boxplot(aes(factor(km40),MeanTemp),fill='gray')+
  scale_x_discrete(expression("Cluster #"))+
  scale_y_continuous(expression("Mean Annual Temperature ("*degree*C*")"))

ggplot(data = sdf)+theme_bw()+
  geom_boxplot(aes(factor(km40),ndvisum),fill='gray')+
  scale_x_discrete(expression("Cluster #"))+
  scale_y_continuous(expression("NDVI"))

ggplot(data = sdf)+theme_bw()+geom_hline(yintercept = 0)+
  geom_boxplot(aes(factor(km40),TempAnnualRange),fill='gray')+
  scale_x_discrete(expression("Cluster #"))+
  scale_y_continuous(expression("Annual Temperature Range ("*degree*C*")"))

ggplot(data = sdf)+theme_bw()+geom_hline(yintercept = 0)+
  geom_boxplot(aes(factor(km40),OCSTHA_M_100cm_1km_ll),fill='gray')+
  scale_x_discrete(expression("Cluster #"))+
  scale_y_continuous(expression("Soil Carbon Stock"))


kmeans.table = sdf %>%
  group_by(km40) %>%
  summarise_all(.funs = c(median,sd,mean,std.error))
  




