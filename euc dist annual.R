#########################################################################
#   Code for determining the environmental dataspace of the possible arctic sites
#  created by K Arndt July 2022
##################################################################################
rm(list = ls())

library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(plotrix)
library(terra)
library(data.table)

#load in the stacks created in the netcdf and climate files, including the base image for subsetting to only the region of interest
base = rast('./base_2_towers.tif')
clim = rast('./data/input data/climate_resample.tif')
sat  = rast('./data/input data/sat_data.tif')

sat = project(x = sat,y = clim)
names(sat) = c('gdd','fdd','ndvimax','ndvimean','ndwimin','lstmean')

#reproject base for purposes of overlaying here
rebase = project(x = base,y = clim)

r = c(clim,sat,rebase)
r = subset(x = r,subset = c(1,4,11,13:15,17:19))

r[r$ndwimin > 0.1] = NA

#remove the above that we don't need anymore to save memory needed in later steps
rm(clim,rebase,sat)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/extracted_ext_annual_data.csv')
#ext.data   = fread(file = './data/extracted_ext_ch4_data.csv')
tower.data$class[262] = 'existing'
tower.data$class[263] = 'existing'


#add the type for identifier
#tower.data$type = 'base'
#ext.data$type = 'ext'

#cut down raster data to remove NAs
df = as.data.frame(r,xy = T)
df$cell = seq(length.out = length(df$base_3_towers)) #add an ID column for matching up data

#run a PCA on the full data set
library(plyr)

#add the tower sites to the main data from the raster
pca.input = rbind.fill(df,tower.data)

#run the PCA
pca = prcomp(x = pca.input[,c(3,4,6:10)],center = T,scale. = T)
summary(pca)
pcadf = data.frame(pca$x[,c(1:3)])

pca.input$pc1 = pcadf$PC1
pca.input$pc2 = pcadf$PC2
pca.input$pc3 = pcadf$PC3

#td = merge(tower.data,df2,by = 'cells',all.x = T)
pca.base   = subset(pca.input,pca.input$class == 'annual')
pca.ext    = subset(pca.input,pca.input$class == 'ext.' | pca.input$class == 'existing')
pca.pixels = subset(pca.input,is.na(pca.input$class))

ggplot()+theme_bw()+
  geom_hex(data = pca.pixels,aes(pc1,pc2),bins=150)+
  scale_fill_viridis_c()+
  geom_label(data = pca.base,aes(pc1,pc2,label = sitename),size=2,fill='transparent',col='green2',label.size = NA)+
  geom_label(data = pca.ext,aes(pc1,pc2,label = sitename),size=2,fill='transparent',col='red',label.size = NA)

#use this to check out drivers of the PCA
#library(ggfortify)
#autoplot(pca,loadings = T,loadings.label = T)

#remove some unnecessary data again
rm(pca)

#################################################################################
#calculate the euclidean distance for the whole data set 

#convert data.frames to data.tables which process faster in loops
pca.dt     = data.table(pca.pixels)
pca.towers = data.table(rbind(pca.base,pca.ext))

#pre-populating the whole matrix makes computation time much faster *DO NOT USE A DATAFRAME
euci = matrix(nrow = nrow(pca.dt),ncol = nrow(pca.towers))

orig = Sys.time()
for (j in 1:nrow(pca.towers)) {  #j is the tower data frame
  for (i in 1:nrow(pca.dt)) { #i is the full data frame
euci[i,j] = sqrt((pca.dt$pc1[i]-pca.towers$pc1[j])^2 + 
                 (pca.dt$pc2[i]-pca.towers$pc2[j])^2 + 
                 (pca.dt$pc3[i]-pca.towers$pc3[j])^2)
  }
 progress(j,nrow(pca.towers))
}
Sys.time() - orig

#create some subsets of the euclidean distance tables for easier calculations
euci.ext  = euci[,nrow(pca.base)+1:nrow(pca.ext)]
euci.base = euci[,1:nrow(pca.base)]

rm(euci)
#colnames(euci.ext) = pca.ext$sitename
#colnames(euci.base) = pca.base$sitename

#calculate based on the mean of the x lowest + site of interest
num = 3 #how many closest towers you want

#again premaking vectors and matrices of the right length greatly speeds up comp time
dist = numeric(length = nrow(df)) 
wch  = matrix(nrow = nrow(df),ncol = num)
eucis = matrix(nrow = nrow(df),ncol = ncol(euci.ext))
temp.euci = matrix(nrow = nrow(df),ncol = ncol(euci.base)+1)

library(kit)

orig = Sys.time()
for (j in 1:ncol(euci.ext)) {
temp.euci = cbind(euci.base[,1:ncol(euci.base)],euci.ext[,j]) #create a temp matrix with the base distances and the site of interest
for (i in 1:nrow(df)) {
 # wch[i,1:num] = which(frank(temp.euci[i,], ties.method='first') <= num) #find which columns are the n lowest
  #wch[i,1:num] = topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F) #find which columns are the n lowest
  #dist[i]    = mean(temp.euci[i,wch[i,]])
  dist[i]    = mean(temp.euci[i,topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F)])
  
}
eucis[,j] = dist
progress(j,ncol(euci.ext))
}
Sys.time() - orig


#calculate the base network
base.dist = numeric(length = nrow(euci.base))
orig = Sys.time()
  for (i in 1:nrow(df)) {
    #wch[i,1:num] = topn(vec = temp.euci[i,],n = num,decreasing = F,hasna = F) #find which columns are the n lowest
    base.dist[i]    = mean(euci.base[i,topn(vec = euci.base[i,],n = num,decreasing = F,hasna = F)])
  }
Sys.time() - orig

#create base image
base = rasterFromXYZ(xyz = cbind(df$x,df$y,base.dist),crs = crs(r))

#save the base here
writeRaster(x = base,filename = './base_3_towers_scc_removed.tif',overwrite = T)

#plot and save
library(viridis)
pal = viridis(n = 8,direction = -1,option = 'A')

#projection
proj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

#reproject into steridean
base.ster = projectRaster(from = base,crs = proj)

#create a dataframe and reproject the tower locations for plotting
towers = SpatialPointsDataFrame(coords = pca.base[,c(1,2)],data = pca.base,proj4string = crs(base))
towers = spTransform(x = towers,CRSobj = proj)
towers.st = as.data.frame(towers)

#world map at a coarse resolution, spatial polygons dataframe
library(rworldmap)
library(cleangeo)
worldmap = getMap(resolution = 'coarse')
worldmap = clgeo_Clean(worldmap)

wm = crop(x = worldmap,y = extent(-180,180,45,90))
wm = spTransform(x = wm,CRSobj = proj)
wm = crop(x = wm,y = extent(c(-4780235,4580235,-3880235,4580235)))

hist(base.ster$base.dist)

#plot
bp = ggplot()+theme_bw()+ggtitle('CH4 Base Network')+
  geom_polygon(data = wm,aes(x = long,y=lat,group=group),
               fill = 'gray80',lwd = 0.25)+
  layer_spatial(base.ster$base.dist)+
  geom_point(data = towers.st,aes(x.1,y.1),col='green',pch=17)+
  scale_fill_gradientn('ED',
                       na.value = 'transparent',
                       colours = pal,
                       #    trans = 'log',
                       limits = c(0,3),
                       oob = scales::squish)+
  scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
  scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
        legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))

bp

#png(filename = './output/base_ch4.png',width = 5,height = 5,units = 'in',res = 2000)
#bp
#dev.off()

#now create rasters our of the euclidean distances calculated for extension sites
dist.rasts = list()
tempdf = data.table()
#convert into geotiffs
for (i in 1:ncol(eucis)) {
  tempdf = cbind(df[,c(1,2)],eucis[,i])
  dist.rasts[[i]] = rasterFromXYZ(xyz = tempdf,crs = crs(r))
  progress(i,ncol(eucis))
}

#create a path of file names
path = paste('./output/base_scc_ext/',pca.ext$sitename,'.tif',sep = '')
#save off rasters
for (i in 1:length(dist.rasts)) {
  writeRaster(x = dist.rasts[[i]],filename = path[i],overwrite=T)
  progress(i,length(dist.rasts))
}


#####################################################################################
#calculate differences
difs = list()
for (i in 1:length(dist.rasts)) {
  difs[[i]] = dist.rasts[[i]] - base$base.dist
  progress(i,length(dist.rasts))
}

#save off difference maps
path = paste('./output/base_scc_dif/',pca.ext$sitename,'_dif.tif',sep = '')
#save off rasters
for (i in 1:length(difs)) {
  writeRaster(x = difs[[i]],filename = path[i],overwrite=T)
  progress(i,length(difs))
}

#reproject these into steridean
dif.ster = list()
for (i in 1:length(difs)) {
  dif.ster[[i]] = projectRaster(from = difs[[i]],crs = proj)
  progress(i,length(difs)) 
}

#create a dataframe and reproject the tower locations for plotting
towers.ext = SpatialPointsDataFrame(coords = pca.ext[,c(1,2)],data = pca.ext,proj4string = crs(base))
towers.ext = spTransform(x = towers.ext,CRSobj = proj)
towers.st = as.data.frame(towers.ext)

#plot
path = paste('./output/dif_scc_plots/',pca.ext$sitename,'_dif.png',sep = '')

for (i in 1:length(dif.ster)) {
  png(filename = path[i],width = 5,height = 5,units = 'in',res = 1000)
plots =   ggplot()+theme_bw()+ggtitle(pca.ext$sitename[i])+
    geom_polygon(data = wm,aes(x = long,y=lat,group=group),
                 fill = 'gray80',lwd = 0.25)+
    layer_spatial(dif.ster[[i]])+
    geom_point(data = towers.st,aes(x.1[i],y.1[i]),col='green',pch=17)+
    scale_fill_gradientn('ED',
                         na.value = 'transparent',
                         colours = pal,
                         #    trans = 'log',
                         limits = c(-.3,0),
                         oob = scales::squish)+
    scale_x_continuous(limits = c(-4780235,4580235),expand = c(0,0))+
    scale_y_continuous(limits = c(-3880235,4580235),expand = c(0,0))+
  theme(text = element_text(size = 8),
          legend.text = element_text(size = 8),
        title = element_text(size = 10),
        axis.title = element_text(size = 8),
        legend.key.width = unit(x = 0.1,units = 'in'))
print(plots)
  dev.off()
  progress(i,length(dif.ster))
}

#calculate mean improvements
means = numeric(length = length(difs))

for (i in 1:length(difs)) {
  means[i] = cellStats(x = difs[[i]],stat = mean)
}

pca.ext$means = means

pca.ext$sitename[173] = 'filler'
pca.ext$sitename[174] = 'filler2'
pca.ext$sitename[178] = 'filler3'

hist(pca.ext$means)
ggplot(data = pca.ext)+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,.02),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  theme(axis.text = element_text(angle = 90))


png(filename = './output/barplot_scc_reduction.png',width = 12,height = 8,units = 'in',res = 2000)
ggplot(data = pca.ext)+theme_bw()+
  geom_bar(aes(reorder(sitename, -means*-1),means*-1),stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.025),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  theme(axis.text = element_text(angle = 90,size = 6),
        axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 8))
dev.off()

#median
median(pca.ext$means)

subs = subset(pca.ext,pca.ext$means < median(pca.ext$means))

png(filename = './output/barplot_ch4_reduction_top50.png',width = 8,height = 8,units = 'in',res = 2000)
ggplot(data = subs,aes(reorder(sitename, -means*-1),means*-1))+theme_bw()+
  geom_bar(stat = 'identity')+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.022),'Mean ED Reduction')+
  scale_x_discrete('Site')+
  theme(axis.text = element_text(angle = 90,size = 6),
        axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 8))
dev.off()

