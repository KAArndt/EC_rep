
library(terra)
library(data.table)
library(plyr)
library(ggplot2)
library(ggnewscale)
library(cowplot)

#re-run PCA for loadings ###################################################################
#load in the stack created in the other file
r = rast('./spatial_data/spatial_repro.tif')

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

write.csv(x = PCAloadings,file = './output/pcaloadings.csv')
summary(pca)
pca$rotation

################################################################################################
#PCA raster figure
# png('./figures/Figure S1 PCA raster.png',width = 5,height = 4,units = 'in',res = 1500)
# plot(r)
# dev.off()

#plots of PCA Clouds #################################################
#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.base.csv')
active = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(r,xy = T,na.rm=T)

#PC 1 & 2
pc12 = ggplot()+theme_bw()+
  geom_hex(data = df,aes(x = PC1,y = PC2),bins = 100)+
  scale_fill_viridis_c(option = "D")+
  new_scale_fill()+
  geom_point(data = active,aes(x = pc1,y = pc2,fill=Country),pch=21,cex=0.8)+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC1*20),yend = (PC2*20)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red",lwd=0.15)+
  geom_label(data = PCAloadings,aes(PC1*22,PC2*22,label=Variables),cex = 1.2)+
  scale_x_continuous('PC1 (34%)')+
  scale_y_continuous("PC2 (20%)")+
  theme(text = element_text(size = 6),
        legend.key.size = unit(x = 0.1,units = 'in'))

######################################################################################
#PC 3 & 4
pc34 = ggplot()+theme_bw()+
  geom_hex(data = df,aes(x = PC3,y = PC4),bins = 100)+
  scale_fill_viridis_c(option = "D")+
  new_scale_fill()+
  geom_point(data = active,aes(x = pc3,y = pc4,fill=Country),pch=21,cex=0.8)+
  geom_segment(data = PCAloadings, 
               aes(x = 0, y = 0, xend = (PC3*15),yend = (PC4*15)),
               arrow = arrow(length = unit(1/2, "picas")),color = "red",lwd=0.15)+
  geom_label(data = PCAloadings,aes(PC3*17,PC4*17,label=Variables),cex = 1.2)+
  scale_x_continuous('PC3 (12%)')+
  scale_y_continuous("PC4 (8%)")+
  theme(text = element_text(size = 6),
        legend.key.size = unit(x = 0.1,units = 'in'))


png(filename = './figures/Figure sXX PCA cloud.png',width = 4,height = 6,units = 'in',res = 1500)
plot_grid(pc12,pc34,labels = c('a','b'),label_size = 6,nrow = 2)
dev.off()
