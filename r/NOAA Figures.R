
library(svMisc)
library(maps)
library(ggplot2)
library(ggspatial)
library(terra)
#library(kit)
library(sf)
library(viridis)
library(data.table)
library(readr)
library(cowplot)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.upgraded.csv')

#create base image
#load in the stack created in the other file
r = rast('./spatial_data/pca_2km.tif')
df = as.data.frame(x = r,xy = T,na.rm = T)

#universal plotting elements
pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')

#world map for plotting
sf_use_s2(FALSE) #need to run this before next line
countries = rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_crop(y = st_bbox(c(xmin = -180, ymin = 44, xmax = 180, ymax = 90))) %>%
  smoothr::densify(max_distance = 1) %>%
  st_transform(crs(r))

# PCA MAP FIGURE ###############################
plot(r)

#Single Site Rep ###############################################################
euci = read_rds(file = './euclidean_distance_matrix/euci_2kmv2.rds')
ykdb = euci[,358]
ykdf = data.frame(df$x,df$y,ykdb)
ykd = rast(x = ykdf,type = 'xyz',crs = crs(r))
ykd.ag = aggregate(x = ykd,fact = 4,fun = mean,na.rm = T)

ykdt = tower.data[358,]
hist(ykd.ag)

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ykd.ag$ykdb)+
  scale_fill_gradientn('Representation',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,6),
                       breaks = c(0,3,6),
                       labels = c('Good','Moderate','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = ykdt,aes(x,y),fill='red',col='black',pch=21,size=3)+
  geom_label(data = ykdt,aes(x+500000,y+500000,label=Site_ID),col='red',size=6)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.5,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')

#plot of 4 conditions ############################################################ ############################
base = rast('./output/base_network/base_2kmv2_mean.tif')
ch4  = rast('./output/base_network/methane_2kmv2_mean.tif')
year = rast('./output/base_network/annual_2kmv2_mean.tif')
ach4 = rast('./output/base_network/annual_methane_2kmv2_mean.tif')

#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
ch4.ag  = aggregate(x = ch4,fact = 4,fun = mean,na.rm = T)
year.ag = aggregate(x = year,fact = 4,fun = mean,na.rm = T)
ach4.ag = aggregate(x = ach4,fact = 4,fun = mean,na.rm = T)

#tower categories
active  = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)

#plot the figure
p1 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,
           label = expression(bold(Summer~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representation',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Mod.','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.25,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

p2 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,
           label = expression(bold(Summer~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ch4.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = methane,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

p3 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,
           label = expression(bold(Annual~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(year.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

p4 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,
           label = expression(bold(Annual~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(ach4.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual_methane,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

plot_grid(p1,p2,p3,p4)


#single site improvement ########################################################
res = rast('./output/difs/Pond Inlet (PP)_dif.tif')
res.ag = aggregate(x = res,fact = 4,fun = mean,na.rm = T)
rest = subset(tower.data,tower.data$site == 'Pond Inlet (PP)')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(res.ag)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-1.5,0),
                       breaks = c(-1.5,-1.5/2,0),
                       labels = c('High','Med.','Low'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = rest,aes(x,y),col='black',fill='red',size=3,pch=21)+
  geom_label(data = rest,aes(x+500000,y-500000,label=Site_ID),col='red',size=4)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.4,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')

#############################################################################################
# mean improvements chart
bars = fread('./output/meanreduction.csv')
top = subset(bars,bars$means < mean(bars$means))

png(filename = './figures/initial_bar_AGU.png',width = 8,height = 4,units = 'in',res = 2000)
ggplot(data = bars)+theme_bw()+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.07),'Mean Rep. Improvement')+
  scale_x_discrete('')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_blank(),
        legend.key.size = unit(0.01,units = 'in'),
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y =  element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.6,0.6),
        panel.grid.major.x = element_blank(),
        legend.direction = 'horizontal')
dev.off()


ggplot(data = bars)+theme_bw()+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.07),'Mean Rep. Improvement')+
  scale_x_discrete('')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_text(angle = 80,hjust = 1,size = 7),
        legend.key.size = unit(0.01,units = 'in'),
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y =  element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.6,0.6),
        panel.grid.major.x = element_blank(),
        legend.direction = 'horizontal')

#######################################################################################
#smaller bar plot
bars = fread('./output/meanreduction2.csv')
top = subset(bars,bars$means < mean(bars$means))

png(filename = './figures/final_bar_AGU.png',width = 8,height = 4,units = 'in',res = 2000)
ggplot(data = top)+theme_bw()+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1,fill=country),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.07),'Mean Rep. Improvement')+
  scale_x_discrete('')+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text.x = element_blank(),
        legend.key.size = unit(0.01,units = 'in'),
        legend.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y =  element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.6,0.6),
        panel.grid.major.x = element_blank(),
        legend.direction = 'horizontal')
dev.off()



#############################################################################################
# base with new sites
base = rast('./output/base_2kmv2.tif')
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)

new = subset(tower.data,
             tower.data$site == 'Pond Inlet (PP)' | 
             tower.data$site == 'Resolute Bay' |
             tower.data$site == 'Churchill Fen' |
             tower.data$site == 'Iqaluit (PP)' |
             tower.data$site == 'Kangiqsuallujjuaq' |
             tower.data$site == 'Scotty Creek Landscape' |
       #      tower.data$site == 'Scotty Creek Bog' |
             tower.data$site == 'Council (Permafrost Pathways)')
        #     tower.data$site == 'Lutose' |
        #     tower.data$site == 'Steen River' |
       #      tower.data$site == 'Cambridge Bay, Victoria Island, mesic' |
       #      tower.data$site == 'Cambridge Bay, Victoria Island, wetland')
         #    tower.data$site == 'Smith Creek' |
        #     tower.data$site == 'Steen River' |
         #    tower.data$site == 'Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Burned 2015' |
          #   tower.data$site == 'Yukon-Kuskokwim Delta, Izaviknek-Kingaglia uplands, Unburned')

ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Mod.','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = new,aes(x,y),col='black',pch=21,fill='red',size=3)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')

### Improved Site #############################################################################################
imp = rast('./output/improve_2kmv2.tif')
imp.ag = aggregate(x = imp,fact = 4,fun = mean,na.rm = T)

old = subset(tower.data,complete.cases(tower.data$`2022 list`) & tower.data$active == 'active' & tower.data$Start_CO2 < 2022)
new = subset(tower.data,tower.data$active == 'active')


ggplot()+theme_map()+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(imp.ag)+
  scale_fill_gradientn('Representativeness',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+  
  new_scale("fill") +
  geom_point(data = new,aes(x,y),col='black',pch=21,fill='cyan',size=3)+
  geom_point(data = old,aes(x,y),col='black',pch=21,fill='red',size=3)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.1,0.05),
        legend.title.position = 'top')


# 4 towers with improvement
#plot of 4 conditions ############################################################ ############################
base   = rast('./output/base_2kmv2.tif')
base.i = rast('./output/improve_2kmv2.tif')

meth   = rast('./output/methane_2kmv2.tif')
meth.i = rast('./output/impch4_2kmv2.tif')

annu   = rast('./output/annual_2kmv2.tif')
annu.i = rast('./output/impanu_2kmv2.tif')

anme   = rast('./output/annual_methane_2kmv2.tif')
anme.i = rast('./output/imp_an_methane_2kmv2.tif')

#create an aggregate for the plot
base.ag = aggregate(x = base,fact = 4,fun = mean,na.rm = T)
meth.ag  = aggregate(x = meth,fact = 4,fun = mean,na.rm = T)
annu.ag = aggregate(x = annu,fact = 4,fun = mean,na.rm = T)
anme.ag = aggregate(x = anme,fact = 4,fun = mean,na.rm = T)

base.i.ag = aggregate(x = base.i,fact = 4,fun = mean,na.rm = T)
meth.i.ag  = aggregate(x = meth.i,fact = 4,fun = mean,na.rm = T)
annu.i.ag = aggregate(x = annu.i,fact = 4,fun = mean,na.rm = T)
anme.i.ag = aggregate(x = anme.i,fact = 4,fun = mean,na.rm = T)

#tower categories
active  = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022)
methane = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022 & tower.data$methane == 'methane')
annual  = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022 & tower.data$Season_Activity == 'All year')
annual_methane = subset(tower.data,tower.data$active == 'active' & tower.data$Start_CO2 < 2022 & tower.data$methane == 'methane' & tower.data$Season_Activity == 'All year')

active.i  = subset(tower.data,tower.data$active == 'active')
methane.i = subset(tower.data,tower.data$active == 'active' & tower.data$methane == 'methane')
annual.i  = subset(tower.data,tower.data$active == 'active' & tower.data$Season_Activity == 'All year')
annual_methane.i = subset(tower.data,tower.data$active == 'active' & tower.data$methane == 'methane' & tower.data$Season_Activity == 'All year')

dif.base = base.i.ag - base.ag
dif.meth = meth.i.ag - meth.ag
dif.annu = annu.i.ag - annu.ag
dif.anme = anme.i.ag - anme.ag

hist(dif.base)
hist(dif.meth)
hist(dif.annu)
hist(dif.anme)

#plot the figure
p1 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Summer~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.base)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-1.5,0),
                       breaks = c(-1.5,-1.5/2,0),
                       labels = c('Good','Moderate','None'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active.i,aes(x,y),col='black',fill='cyan',size=2,pch=21)+
  geom_point(data = active,aes(x,y),col='black',fill='red',size=2,pch=21)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.3,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')


p2 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Summer~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.meth)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-1.5,0),
                       breaks = c(-1.5,-1.5/2,0),
                       labels = c('Good','Moderate','None'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = methane.i,aes(x,y),col='black',fill='cyan',size=2,pch=21)+
  geom_point(data = methane,aes(x,y),col='black',fill='red',size=2,pch=21)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')


p3 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Annual~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.annu)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-1.5,0),
                       breaks = c(-1.5,-1.5/2,0),
                       labels = c('Good','Moderate','None'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual.i,aes(x,y),col='black',fill='cyan',size=2,pch=21)+
  geom_point(data = annual,aes(x,y),col='black',fill='red',size=2,pch=21)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')


p4 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Annual~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(dif.anme)+
  scale_fill_gradientn('Improvement',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(-1.5,0),
                       breaks = c(-1.5,-1.5/2,0),
                       labels = c('Good','Moderate','None'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual_methane.i,aes(x,y),col='black',fill='cyan',size=2,pch=21)+
  geom_point(data = annual_methane,aes(x,y),col='black',fill='red',size=2,pch=21)+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

plot_grid(p1,p2,p3,p4)


#final figure network ########################################################
base.i = rast('./output/improve_2kmv2.tif')
meth.i = rast('./output/impch4_2kmv2.tif')
annu.i = rast('./output/impanu_2kmv2.tif')
anme.i = rast('./output/imp_an_methane_2kmv2.tif')

#create an aggregate for the plot
base.i.ag = aggregate(x = base.i,fact = 4,fun = mean,na.rm = T)
meth.i.ag  = aggregate(x = meth.i,fact = 4,fun = mean,na.rm = T)
annu.i.ag = aggregate(x = annu.i,fact = 4,fun = mean,na.rm = T)
anme.i.ag = aggregate(x = anme.i,fact = 4,fun = mean,na.rm = T)

#tower categories
active.i  = subset(tower.data,tower.data$active == 'active')
methane.i = subset(tower.data,tower.data$active == 'active' & tower.data$methane == 'methane')
annual.i  = subset(tower.data,tower.data$active == 'active' & tower.data$Season_Activity == 'All year')
annual_methane.i = subset(tower.data,tower.data$active == 'active' & tower.data$methane == 'methane' & tower.data$Season_Activity == 'All year')


#plot the figure
p1 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Summer~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(base.i.ag)+
  scale_fill_gradientn('Representation',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Mod.','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = active.i,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        legend.key.height = unit(x = 0.1,units = 'in'),
        legend.key.width = unit(x = 0.25,units = 'in'),
        legend.direction = 'horizontal',
        legend.position = c(0.05,0.05),
        legend.title.position = 'top')

p1
p2 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Summer~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(meth.i.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = methane.i,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

p3 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Annual~CO[2])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(annu.i.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual.i,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

p4 = ggplot()+theme_map()+
  annotate(geom = 'text',x = -5093909+2000000,4375097-400000,label = expression(bold(Annual~CH[4])))+
  geom_sf(data = countries,fill='gray',col='gray40')+
  layer_spatial(anme.i.ag)+
  scale_fill_gradientn('Euc. Dist.',
                       na.value = 'transparent',
                       colours = pal,
                       limits = c(0,1.67*2),
                       breaks = c(0,1.67,1.67*2),
                       labels = c('Good','Cutoff','Poor'),
                       oob = scales::squish)+
  new_scale("fill") +
  geom_point(data = annual_methane.i,aes(x,y),col='black',size=2,pch=21,fill='red')+
  scale_x_continuous(limits = c(-5093909,4542996))+
  scale_y_continuous(limits = c(-3687122,4374170))+
  theme(text = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = 'none')

plot_grid(p1,p2,p3,p4)
