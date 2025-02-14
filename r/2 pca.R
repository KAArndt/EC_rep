
library(terra)
library(data.table)
library(plyr)
library(ggplot2)
library(ggnewscale)

#load in the stack created in the other file
r = rast('./spatial_data/spatial_repro_extended.tif')

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
pca.ta = subset(pca.t,pca.t$active == 'active')
pca.ex = subset(pca.t,pca.t$active != 'active' | is.na(pca.t$active))

PCAloadings = data.frame(Variables = rownames(pca$rotation), pca$rotation)
summary(pca)
pca$rotation

#names for raster to match
names(r) = c("MeanTemp","Precip","PrecipSeasonality","MeanDiurnalRange",
             "Isothermality","TempSeasonality","TempAnnualRange",
             "NDVImax","NDVIsum","EVImax","NDWImin","SWIRaug",
             "BulkDens","pH","CStock",'CDensity',"Sand","Silt","Clay",                             
             "Permafrost")

p = predict(r, pca,index = 1:4)
plot(p)

writeRaster(x = p,filename = './spatial_data/pca.tif',overwrite = T)

pca.original = pca.t

pca.original$active = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active)
pca.original$active = ifelse(tower.data$site == 'Council (NGEE Arctic)','inactive',tower.data$active)
pca.original$Season_Activity = ifelse(tower.data$site == 'Resolute Bay','All year',tower.data$Season_Activity)

write.csv(x = pca.original,file = './data/pca.towers.base.csv',row.names = F)
          
pca.upgraded = pca.t

#change tower sites we increased to all year coverage
pca.upgraded$Season_Activity = ifelse(tower.data$site == "Lutose" |
                                      tower.data$site == "Scotty Creek Landscape" |
                                      tower.data$site == "Steen River" |
                                      tower.data$site == "Scotty Creek Bog" |
                                      tower.data$site == "Smith Creek",
                                    'All year',tower.data$Season_Activity)
write.csv(x = pca.upgraded,file = './data/pca.towers.upgraded.csv',row.names = F)

#PCA plots #############################################################
pca.ice = subset(pca.ex,pca.ex$Country == 'Iceland')
pca.cf = subset(pca.ex,pca.ex$site == 'CEF cluster')
pca.mon = subset(pca.ex,pca.ex$Country == 'Mongolia')

#PC 1 & 2
ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc1,y = pc2),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc1,y = pc2,fill='Active Site'),pch=21,size=2)+
  #  geom_point(data = pca.ex,aes(x = pc1,y = pc2,fill='Extension Site'),pch=21,size=2)+
  geom_point(data = pca.ice,aes(x = pc1,y = pc2,fill='Iceland'),pch=21,size=2)+
  geom_point(data = pca.cf,aes(x = pc1,y = pc2,fill='Quebec'),pch=21,size=2)+
  geom_label(data = pca.mon,aes(x = pc1,y = pc2,col='Mongolia',label=site),pch=21,size=2)+
  #  geom_label(data = pca.ta,aes(x = pc1,y = pc2,col='Active Site',label=site),pch=21,size=2)+
  #  geom_label(data = pca.ex,aes(x = pc1,y = pc2,col='Extension Site',label=site),pch=21,size=2)+
  scale_fill_manual('',values = c('green','red','orange','pink'))+
  scale_x_continuous('PC1 (34.79%)')+
  scale_y_continuous("PC2 (20.62%)")

ggplot()+theme_bw()+
  geom_hex(data = pca.r,aes(x = pc1,y = pc2),bins = 150)+
  scale_fill_viridis_c(option = "A")+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = pc1,y = pc2,fill='Active Site'),pch=21,size=2)+
  #  geom_point(data = pca.ex,aes(x = pc1,y = pc2,fill='Extension Site'),pch=21,size=2)+
  geom_point(data = pca.cf,aes(x = pc1,y = pc2,fill='Quebec'),pch=21,size=2)+
  geom_point(data = pca.mon,aes(x = pc1,y = pc2,fill='Mongolia'),pch=21,size=2)+
  scale_fill_manual('',values = c('green','red','orange','pink'))+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC1*20),yend = (PC2*20)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red")+
  geom_label(data = PCAloadings,aes(PC1*22,PC2*22,label=Variables))+
  scale_x_continuous('PC1 (34.79%)')+
  scale_y_continuous("PC2 (20.62%)")

######################################################################################
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
  #  geom_point(data = pca.ex,aes(x = pc3,y = pc4,fill='Extension Site'),pch=21,size=2)+
  scale_fill_manual('',values = c('green','red','orange'))+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC3*15),yend = (PC4*15)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red")+
  geom_label(data = PCAloadings,aes(PC3*16,PC4*16,label=Variables))+
  scale_x_continuous('PC3 (10.26%)')+
  scale_y_continuous("PC4 (8.46%)")


#parameter plots
ggplot(data = pca.r)+theme_bw()+
  geom_hex(aes(MeanTemp,Precip),bins=150)+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = MeanTemp,y = Precip,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ice,aes(x = MeanTemp,y = Precip,fill='Iceland'),pch=21,size=2)+
  geom_point(data = pca.cf,aes(x = MeanTemp,y = Precip,fill='Quebec'),pch=21,size=2)+
  geom_point(data = pca.mon,aes(x = MeanTemp,y = Precip,fill='Mongolia'),pch=21,size=2)+
  scale_fill_manual('',values = c('cyan','red','orange','pink'))
  
pca.r$NDVIsum
ggplot(data = pca.r)+theme_bw()+
  geom_hex(aes(NDVIsum,MeanDiurnalRange),bins=150)+
  new_scale_fill()+
  geom_point(data = pca.ta,aes(x = NDVIsum,y = MeanDiurnalRange,fill='Active Site'),pch=21,size=2)+
  geom_point(data = pca.ice,aes(x = NDVIsum,y = MeanDiurnalRange,fill='Iceland'),pch=21,size=2)+
  geom_point(data = pca.cf,aes(x = NDVIsum,y = MeanDiurnalRange,fill='Quebec'),pch=21,size=2)+
  geom_point(data = pca.mon,aes(x = NDVIsum,y = MeanDiurnalRange,fill='Mongolia'),pch=21,size=2)+
  scale_fill_manual('',values = c('cyan','red','orange','pink'))
