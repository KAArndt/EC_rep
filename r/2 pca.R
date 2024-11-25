#########################################################################
#   Code for determining the environmental data space of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())

#library(raster)
#library(svMisc)
#library(MASS)
library(ggplot2)
library(ggnewscale)
#library(ggspatial)
#library(plotrix)
library(terra)
library(plyr)
library(data.table)
#library(kit)
#library(ggthemes)
#library(sf)
#library(ggfortify)

#load in the stack created in the other file
r = rast('./data/input data/spatial_repro.tif')

#load in extracted site data from extraction codes
tower.data = fread(file = './data/extracted_tower_data.csv')

#cut down raster data to remove NAs
sr = spatSample(x = r,size = 500000,method = "regular")

sr = sr[complete.cases(sr$MeanTemp),]

tower.data[,c("WarmestQuarter",                                 
      "ColdestQuarter",                                   
      "WettestMonth",                 
      "DriestMonth",                         
      "WettestQuarter",                            
      "DriestQuarter",                          
      "PrecipWarmestQuarter",                          
      "PrecipColdestQuarter",                                  
      "MaxTempWarmestMonth",                                  
      "MinTempColdestMonth",                                   
      "MeanTempWettestQuarter",                                  
      "MeanTempDriestQuarter")] = NULL

srt = rbind.fill(sr,tower.data)
names(srt)

#change these for plotting
names(srt)[1:20] = c("MeanTemp","Precip","PrecipSeasonality","MeanDiurnalRange",
                     "Isothermality","TempSeasonality","TempAnnualRange",
                     "NDVImax","NDVIsum","EVImax","NDWImin","SWIRaug",
                     "BulkDens","pH","CStock",'CDensity',"Sand","Silt","Clay",                             
                     "Permafrost")


pca = prcomp(srt[,c(1:20)],center = T,scale = T)

srt$pc1 = pca$x[,1]
srt$pc2 = pca$x[,2]
srt$pc3 = pca$x[,3]
srt$pc4 = pca$x[,4]

pca.r = subset(srt,is.na(srt$site))
pca.t = subset(srt,complete.cases(srt$site))
pca.ta = subset(pca.t,pca.t$Activity == 'active')
pca.ex = subset(pca.t,pca.t$Activity != 'active' | is.na(pca.t$Activity))

PCAloadings = data.frame(Variables = rownames(pca$rotation), pca$rotation)
summary(pca)
pca$rotation

#PC 1 & 2
ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc1,y = pc2),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc1,y = pc2,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc1,y = pc2,fill='Extension Site'),pch=21,size=2)+
#  geom_label(data = pca.ta,aes(x = pc1,y = pc2,col='Active Site',label=site),pch=21,size=2)+
#  geom_label(data = pca.ex,aes(x = pc1,y = pc2,col='Extension Site',label=site),pch=21,size=2)+
  scale_fill_manual('',values = c('green','white'))+
  scale_x_continuous('PC1 (34.79%)')+
  scale_y_continuous("PC2 (20.62%)")
  
ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc1,y = pc2),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc1,y = pc2,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc1,y = pc2,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('cyan','green'))+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC1*20),yend = (PC2*20)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red")+
  geom_label(data = PCAloadings,aes(PC1*22,PC2*22,label=Variables))+
  scale_x_continuous('PC1 (34.79%)')+
  scale_y_continuous("PC2 (20.62%)")

#PC 3 & 4
ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc3,y = pc4),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc3,y = pc4,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc3,y = pc4,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('green','white'))+
  scale_x_continuous('PC3 (10.26%)')+
  scale_y_continuous("PC4 (8.46%)")

ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc3,y = pc4),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc3,y = pc4,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc3,y = pc4,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('cyan','green'))+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC3*15),yend = (PC4*15)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red")+
  geom_label(data = PCAloadings,aes(PC3*16,PC4*16,label=Variables))+
  scale_x_continuous('PC3 (10.26%)')+
  scale_y_continuous("PC4 (8.46%)")

#PC 5 & 6
ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc5,y = pc6),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc5,y = pc6,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc5,y = pc6,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('green','white'))+
  scale_x_continuous('PC5 (10.26%)')+
  scale_y_continuous("PC4 (8.46%)")

ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc5,y = pc6),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc5,y = pc6,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ex,aes(x = pc5,y = pc6,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('cyan','green'))+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC5*15),yend = (PC6*15)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red")+
  geom_label(data = PCAloadings,aes(PC5*16,PC6*16,label=Variables))+
  scale_x_continuous('PC5 (10.26%)')+
  scale_y_continuous("PC6 (8.46%)")

#names for raster to match
names(r) = c("MeanTemp","Precip","PrecipSeasonality","MeanDiurnalRange",
             "Isothermality","TempSeasonality","TempAnnualRange",
             "NDVImax","NDVIsum","EVImax","NDWImin","SWIRaug",
             "BulkDens","pH","CStock",'CDensity',"Sand","Silt","Clay",                             
             "Permafrost")

p = predict(r, pca,index = 1:4)
plot(p)

writeRaster(x = p,filename = './data/input data/pca.tif',overwrite = T)
write.csv(x = pca.t,file = './data/pca.towersv2.csv',row.names = F)
