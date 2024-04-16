rm(list = ls())

#reverse engineer the things

library(data.table)
library(ggplot2)
library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')

red = fread('./output/active/meanreduction.csv')
tdat = fread('./data/extracted_tower_data.csv')


act = subset(tdat,tdat$active == 'active')

ggplot(data = act)+
  geom_label(aes(ColdestQuarter,ndwimin,col=OCSTHA_M_100cm_1km_ll,label=sitename),
             size=3.5,fill='transparent',label.size = NA)+
  scale_color_viridis_c(limits = c(0,1500))
  

merge = merge(red,tdat,by = 'sitename')

ggplot(data = merge)+
  geom_label(aes(MeanTemp,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(WarmestQuarter,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(ColdestQuarter,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')+
  scale_y_continuous('Rep Improvement')+
  scale_x_continuous(expression('Coldest Quarter ('*degree*'C)'))

ggplot(data = merge)+
  geom_label(aes(Precip,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$MeanDiurnalRange,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$Isothermality,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$TempSeasonality,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$MaxTempWarmestMonth,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$MinTempColdestMonth,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$TempAnnualRange,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$gdd,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$fdd,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$ndvisum,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$ndwimin,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$band7,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$ph0_100,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$OCSTHA_M_100cm_1km_ll,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$sand_100_agg,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$silt_100_agg,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$clay_100_agg,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')

ggplot(data = merge)+
  geom_label(aes(merge$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH,means*-1,label=sitename),
             label.size = NA,fill = 'transparent')


library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(plyr)
library(data.table)
library(kit)

base = rast("./summean_v5_base.tif")

#load in the stack created in the other file
clim = rast('./data/input data/climate_resample.tif')
sat  = rast('./data/input data/sat_data.tif')
soil = rast('./data/input data/soils.tif')

sat = resample(x = sat,y = base)
clim = resample(x = clim,y = base)
soil = resample(x = soil,y = base)

names(sat) = c('gdd','fdd','ndvisum','ndwimin','band7')

r = c(clim,sat,soil)
r = subset(x = r,subset = c(1:4,12:17,20:30))

r[is.na(base)] = NA

#remove the above that we don't need anymore to save memory needed in later steps
rm(clim,soil,sat)

r = stack(r)
plot(r$MeanTemp)
plot(r$WarmestQuarter)
plot(r$ColdestQuarter)
plot(r$Precip,zlim=c(0,1200))
plot(r$MeanDiurnalRange)
plot(r$Isothermality)
plot(r$TempSeasonality)
plot(r$MaxTempWarmestMonth)
plot(r$MinTempColdestMonth)
plot(r$TempAnnualRange)
plot(r$gdd)
plot(r$fdd)
plot(r$ndvisum)
plot(r$ndwimin,zlim=c(-0.4,0))
plot(r$band7,zlim=c(0,0.2))
plot(r$ph0_100)
plot(r$OCSTHA_M_100cm_1km_ll,zlim=c(0,1000))
plot(r$sand_100_agg,zlim=c(0,100))
plot(r$silt_100_agg,zlim=c(0,100))
plot(r$clay_100_agg)
plot(r$UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH)

