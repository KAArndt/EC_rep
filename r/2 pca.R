
library(terra)
library(data.table)
library(plyr)
library(ggplot2)
library(ggnewscale)

#load in the stack created in the other file
r = rast('./spatial_data/spatial_repro.tif')

#load in extracted site data from extraction codes
tower.data = fread(file = './data/extracted_tower_data.csv')

#cut down raster data to remove NAs
sr = spatSample(x = r,size = 500000,method = "regular")
sr = sr[complete.cases(sr),]

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

write.csv(x = PCAloadings,file = './data/pcaloadings.csv')

#names for raster to match
names(r) = c("MeanTemp","Precip","PrecipSeasonality","MeanDiurnalRange",
             "Isothermality","TempSeasonality","TempAnnualRange",
             "NDVImax","NDVIsum","EVImax","NDWImin","SWIRaug",
             "BulkDens","pH","CStock",'CDensity',"Sand","Silt","Clay",
             "Permafrost")

p = predict(r, pca,index = 1:4)
plot(p)

#p = rast('./spatial_data/pca.tif')
writeRaster(x = p,filename = './spatial_data/pca.tif',overwrite = T)

p2 = aggregate(x = p,fact = 2,fun = mean,na.rm = T)
writeRaster(x = p2,filename = './spatial_data/pca_2km.tif',overwrite = T)

#save off csv with PCA results
write.csv(x = pca.t,file = './data/pca.towers.csv',row.names = F)
