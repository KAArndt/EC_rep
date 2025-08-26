
library(terra)
library(dplyr)
library(data.table)

#load in landcover dataset
lc = rast('./spatial_data/HybridLandCover_1km.tif')
r  = rast('./spatial_data/mir_aug_10yrmean.tif')

#count number of towers
base.towers = fread('./data/pca.towers.base.csv')
new.towers  = fread('./data/pca.towers.upgraded.csv')

#Add variables for projected coordinates
td = vect(geom = c("Longitude","Latitude"),x = new.towers,crs = crs(r))
td = project(x = td,y = crs(lc))
crd = data.frame(crds(td))

new.towers$x = crd$x
new.towers$y = crd$y

#filter to the base towers
base.t           = subset(base.towers,base.towers$active == 'active' & base.towers$Start_CO2 < 2022)
methane.t        = subset(base.towers,base.towers$active == 'active' & base.towers$Start_CO2 < 2022 
                          & base.towers$methane == 'methane')
annual.t         = subset(base.towers,base.towers$active == 'active' & base.towers$Start_CO2 < 2022 
                          & base.towers$Season_Activity == 'All year')
annual.methane.t = subset(base.towers,base.towers$active == 'active' & base.towers$Start_CO2 < 2022 
                          & base.towers$Season_Activity == 'All year' & base.towers$methane == 'methane')

#filter the new tower sites
base.t.new                = subset(new.towers,new.towers$active == 'active')
methane.t.new        = subset(new.towers,new.towers$active == 'active' & new.towers$methane == 'methane')
annual.t.new         = subset(new.towers,new.towers$active == 'active' & new.towers$Season_Activity == 'All year')
annual.methane.t.new = subset(new.towers,new.towers$active == 'active' & new.towers$Season_Activity == 'All year' & new.towers$methane == 'methane')

#create a data set of just new sites to the base
new = merge(base.t,base.t.new,'site',all=T)
new = new[is.na(MeanTemp.x),]

lc.new = extract(x = lc,y = new[,c('x.y','y.y')])
new$lc = lc.new$HybridLandCover_1km

new[,c('site','lc')]

#new methane sites
new = merge(methane.t,methane.t.new,'site',all=T)
new = new[is.na(MeanTemp.x),]

lc.new = extract(x = lc,y = new[,c('x.y','y.y')])
new$lc = lc.new$HybridLandCover_1km

new[,c('site','lc')]

#new year round sites
new = merge(annual.t,annual.t.new,'site',all=T)
new = new[is.na(MeanTemp.x),]

lc.new = extract(x = lc,y = new[,c('x.y','y.y')])
new$lc = lc.new$HybridLandCover_1km

new[,c('site','lc')]


#new year round methane sites
new = merge(annual.methane.t,annual.methane.t.new,'site',all=T)
new = new[is.na(MeanTemp.x),]

lc.new = extract(x = lc,y = new[,c('x.y','y.y')])
new$lc = lc.new$HybridLandCover_1km

new[,c('site','lc')]
