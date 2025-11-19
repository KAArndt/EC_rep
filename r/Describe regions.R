library(terra)
library(ggplot2)
library(dplyr)
library(cowplot)

#load in all the spatial data
#rep distances
gsco2 = rast('./output/base_network/base_2km.tif')
gsch4 = rast('./output/base_network/methane_2km.tif')
anco2 = rast('./output/base_network/annual_2km.tif')
anch4 = rast('./output/base_network/annual_methane_2km.tif')

igsco2 = rast('./output/improved_network/improved_base_2km.tif')
igsch4 = rast('./output/improved_network/improved_methane_2km.tif')
ianco2 = rast('./output/improved_network/improved_annual_2km.tif')
ianch4 = rast('./output/improved_network/improved_annual_methane_2km.tif')

#clusters
clust = rast('./output/clusts.tif')
clust = clust$km40

#pca results
pca = rast('./spatial_data/pca_2km.tif')
pca = crop(x = pca,y = clust)

#environmental data
r = rast('./spatial_data/spatial_repro_2km.tif')
r = crop(x = r,y = clust)

#merge all into one stack
all = c(pca,r,gsco2,gsch4,anco2,anch4)

#aggregate to make plottig and playing with data more manageable
ag    = aggregate(x = all,fact = 10,fun = 'mean',cores = 6,na.rm = T)
ag.km = aggregate(x = clust,fact = 10,fun = 'modal',na.rm = T)

ag = c(ag,ag.km)

#extract all as a data frame
df = as.data.frame(x = ag,xy=T)
df = df[complete.cases(df$base.dist),]

#ecoregions
# eco = vect('./spatial_data/Ecoregions2017/Ecoregions2017.shp')
# eco = crop(x = eco,y = c(-180,180,40,90))
# eco = subset(eco,eco$BIOME_NAME == 'Tundra' | eco$BIOME_NAME == 'Boreal Forests/Taiga')
# eco = project(x = eco,y = crs(ag))

#make the cluster a character for plotting
df$km40 = as.character(df$km40)

df$km40 = ordered(x = df$km40,c(
'1', '2' ,'3' ,'4' ,'5' ,'6' ,'7' ,'8' ,'9' ,'10',
'11','12','13','14','15','16','17','18','19','20',
'21','22','23','24','25','26','27','28','29','30',
'31','32','33','34','35','36','37','38','39','40'))

#calculate summary statistics
stats = df %>%
  group_by(km40) %>%
  summarise_all(list(mean))

dists = stats[,c(1,28,29,30,31)]
names(dists) = c('km40','base','methane','annual','annualmethane')

df = merge(df,dists,by = 'km40',all=T)

#color pallette
pal = hcl.colors(n = 9,palette = 'Vik')
pal = pal[-c(4,6)]
#pal = c('#FEEDB9','#E88D7A','#72509A','#8AABD6','#F2F7FB')
plot(clust)
plot(clust,range = c(38.5,41.5))

#play with data
ggplot(data = df)+theme_bw()+geom_hline(yintercept = 0)+
  geom_violin(aes(x = km40,y = MeanTemp,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = Precip,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = MeanDiurnalRange,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = Isothermality,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = TempAnnualRange,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = ndvi_sum,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = ndvi_max,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = evi,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = soc0_100,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = OCSTHA_M_100cm_1km_ll,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = ndwi,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = mir,fill = base))+
  scale_fill_gradientn(colors = pal)

ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = clay_100_agg,fill = base))+
  scale_fill_gradientn(colors = pal)

#final plots ####################################################################
temp = ggplot(data = df)+theme_bw()+geom_hline(yintercept = 0)+
  geom_violin(aes(x = km40,y = MeanTemp,fill = base))+
  scale_fill_gradientn(colors = pal)+
  scale_y_continuous(expression('Mean Temp.'~'('*degree*C*")"))+
  scale_x_discrete('')+
  theme(text = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        legend.position = 'none')

temp_range = ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = TempAnnualRange,fill = base))+
  scale_fill_gradientn(colors = pal)+
  scale_y_continuous(expression('Temp. Range'~'('*degree*C*")"))+
  scale_x_discrete('')+
  theme(text = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        legend.position = 'none')

ndvi = ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = ndvi_sum,fill = base))+
  scale_fill_gradientn(colors = pal)+
  scale_y_continuous(expression('NDVI Sum (unitless)'))+
  scale_x_discrete('')+
  theme(text = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        legend.position = 'none')

soc = ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = soc0_100,fill = base))+
  scale_fill_gradientn(colors = pal)+
  scale_y_continuous(expression('SOC ('*kg~m^-3*")"))+
  scale_x_discrete('')+
  theme(text = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        legend.position = 'none')

mir = ggplot(data = df)+theme_bw()+
  geom_violin(aes(x = km40,y = mir,fill = base))+
  scale_fill_gradientn(colors = pal)+
  scale_y_continuous('SWIR (unitless)')+
  scale_x_discrete('Cluster Number')+
  theme(text = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        legend.position = 'none')

leg = get_legend(ggplot(data = df)+theme_bw()+
                   geom_violin(aes(x = km40,y = mir,fill = base))+
                   scale_fill_gradientn('Rep.',
                                        colours = pal,
                                        limits = c(0,1.56*2),
                                        breaks = c(0,1.56,1.56*2),
                                        labels = c('Good','Cutoff','Poor'),
                                        oob = scales::squish)+
                   theme(legend.text = element_text(size = 7),
                         legend.title = element_text(size = 7),
                         legend.key.height = unit(0.5,units = 'in'),
                         legend.key.width = unit(0.075,units = 'in')))

g = plot_grid(temp,temp_range,ndvi,soc,mir,nrow = 5,labels = c('a','b','c','d','e'),label_size = 10,align = 'hv')

png(filename = './figures/figure xx cluster simmilarity metrics.png',width = 7,height = 8,units = 'in',res = 1800)
plot_grid(g,leg,nrow = 1,rel_widths = c(0.92,0.08))
dev.off()

####################################################################################




mod = lm(base.dist ~ mir,data = stats)
summary(mod)

mod = lm(base.dist ~ PC2,data = stats)
summary(mod)

mod = lm(base.dist ~ PC3,data = stats)
summary(mod)

mod = lm(base.dist ~ PC4,data = stats)
summary(mod)

hist(mod$residuals)
summary(lm(base.dist ~ PRecip,data = stats))
summary(lm(base.dist ~ MeanTemp,data = stats))
summary(lm(base.dist ~ MeanTemp,data = stats))
summary(lm(base.dist ~ MeanTemp,data = stats))
summary(lm(base.dist ~ MeanTemp,data = stats))


ggplot(data = stats,aes(stats$mir,base.dist))+
  geom_point()+
  geom_smooth(method = 'lm')


model <- glm(base.dist ~ mir,family=binomial(link='logit'),data=stats)


ggplot(data = df)+
  geom_point(aes(MeanTemp,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(Precip,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(TempSeasonality,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(TempAnnualRange,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(ndwi,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(ndvi_sum,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(ndvi_max,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(evi,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(soc0_100,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(OCSTHA_M_100cm_1km_ll,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(PC1,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(PC2,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(PC3,base.dist),alpha=0.25)

ggplot(data = df)+
  geom_point(aes(PC4,base.dist),alpha=0.25)



ggplot(data = df)+
  geom_violin(aes(x = km40,y = MeanTemp))