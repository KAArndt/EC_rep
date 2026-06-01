
library(data.table)
library(terra)
library(dplyr)

#load in clusters
clust = rast('./output/clusts_2km.tif')
clust = clust$km40

#load in landcovers
lc = rast('./spatial_data/HybridLandCover_1km.tif')
eco = vect(x = './spatial_data/Ecoregions2017/Ecoregions2017.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$BIOME_NAME == 'Rock and Ice' | 
               eco$BIOME_NAME == 'Tundra' |
               eco$BIOME_NAME == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

towers = fread('./data/final.tower.data.csv')
names(towers)[21] = 'sitename'
td = vect(geom = c("x","y"),x = towers,crs = crs(clust))
td = project(x = td,y = crs(lc))
crd = data.frame(crds(td))
towers$x = crd$x
towers$y = crd$y
lce = extract(x = lc,y = towers[,c('x','y')])
towers$lc = lce$HybridLandCover_1km

ecoe = extract(x = eco,y = towers[,c('Longitude','Latitude')])
towers$eco = ecoe$ECO_NAME

#the cutoff values from the previous exercises (step 4, mean)
er1 = 2.11
er4 = 1.77

#regular new sites ###########################################################################################
df1 = fread('./data/reductions/meanreduction_remaining.csv')
df2 = fread('./data/reductions/meanreduction_remaining_1.csv')
df3 = fread('./data/reductions/meanreduction_remaining_2.csv')
df4 = fread('./data/reductions/meanreduction_remaining_3.csv')
df5 = fread('./data/reductions/meanreduction_remaining_4.csv')
df6 = fread('./data/reductions/meanreduction_remaining_5.csv')

df1 = subset(df1,df1$means == min(df1$means))
df2 = subset(df2,df2$means == min(df2$means))
df3 = subset(df3,df3$means == min(df3$means))
df4 = subset(df4,df4$means == min(df4$means))
df5 = subset(df5,df5$means == min(df5$means))
df6 = subset(df6,df6$means == min(df6$means))

top = rbind(df1,df2,df3,df4,df5,df6,fill=T)
top = merge(x = top,y = towers,all.x = T,by = 'sitename')
top = top[order(top$means),]

top$sitename
top$eco
top$lc

#load in raster files
imp  = rast('./output/improved_network/improved_base_2km.tif')
imp1 = rast('./output/improved_network/next_five_sites/improved_base_2km_1.tif')
imp2 = rast('./output/improved_network/next_five_sites/improved_base_2km_2.tif')
imp3 = rast('./output/improved_network/next_five_sites/improved_base_2km_3.tif')
imp4 = rast('./output/improved_network/next_five_sites/improved_base_2km_4.tif')
imp5 = rast('./output/improved_network/next_five_sites/improved_base_2km_5.tif')

# dif01 = imp - imp1
# dif12 = imp1 - imp2
# dif23 = imp2 - imp3
# dif34 = imp3 - imp4
# dif45 = imp4 - imp5
# 
# plot(dif01)
# plot(dif12)
# plot(dif23)
# plot(dif34)
# plot(dif45)

#merge all together
all = c(imp,imp1,imp2,imp3,imp4,imp5,clust)

names(all) = c('improved','step1','step2','step3','step4','step5','cluster')
df = as.data.frame(x = all)
summary(df)

#set 1 for meets cutoff and 0 for does not
df$improved.er1 = ifelse(df$improved <= er1,1,0)
df$improved.er4 = ifelse(df$improved <= er4,1,0)

df$step1.er1 = ifelse(df$step1 <= er1,1,0)
df$step1.er4 = ifelse(df$step1 <= er4,1,0)

df$step2.er1 = ifelse(df$step2 <= er1,1,0)
df$step2.er4 = ifelse(df$step2 <= er4,1,0)

df$step3.er1 = ifelse(df$step3 <= er1,1,0)
df$step3.er4 = ifelse(df$step3 <= er4,1,0)

df$step4.er1 = ifelse(df$step4 <= er1,1,0)
df$step4.er4 = ifelse(df$step4 <= er4,1,0)

df$step5.er1 = ifelse(df$step5 <= er1,1,0)
df$step5.er4 = ifelse(df$step5 <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.step1 = df$improved - df$step1
df$change.step2 = df$step1    - df$step2
df$change.step3 = df$step2    - df$step3
df$change.step4 = df$step3    - df$step4
df$change.step5 = df$step4    - df$step5

df$change.count.step1 = ifelse(df$change.step1 > 0 ,1,0)
df$change.count.step2 = ifelse(df$change.step2 > 0 ,1,0)
df$change.count.step3 = ifelse(df$change.step3 > 0 ,1,0)
df$change.count.step4 = ifelse(df$change.step4 > 0 ,1,0)
df$change.count.step5 = ifelse(df$change.step5 > 0 ,1,0)

df = df[complete.cases(df$improved),]
#calculate improvements
summary = df %>%
  summarise(improved.er1 = sum(improved.er1)/sum(all),
            step1.er1    = sum(step1.er1)/sum(all),
            step2.er1    = sum(step2.er1)/sum(all),
            step3.er1    = sum(step3.er1)/sum(all),
            step4.er1    = sum(step4.er1)/sum(all),
            step5.er1    = sum(step5.er1)/sum(all),
            
            improved.er4 = sum(improved.er4)/sum(all),
            step1.er4    = sum(step1.er4)/sum(all),
            step2.er4    = sum(step2.er4)/sum(all),
            step3.er4    = sum(step3.er4)/sum(all),
            step4.er4    = sum(step4.er4)/sum(all),
            step5.er4    = sum(step5.er4)/sum(all),

            change.step1   = sum(change.count.step1)/sum(all),
            change.step2   = sum(change.count.step2)/sum(all),
            change.step3   = sum(change.count.step3)/sum(all),
            change.step4   = sum(change.count.step4)/sum(all),
            change.step5   = sum(change.count.step5)/sum(all),
            
            total.step1    = (sum(improved) - sum(step1))/sum(improved),
            total.step2    = (sum(step1) - sum(step2))/sum(step1),
            total.step3    = (sum(step2) - sum(step3))/sum(step2),
            total.step4    = (sum(step3) - sum(step4))/sum(step3),
            total.step5    = (sum(step4) - sum(step5))/sum(step4),
  )

summary
write.csv(x = summary,file = './data/improvements_base_network_next5.csv')

summary
gser1 = c(summary$step1.er1,
        summary$step2.er1,
        summary$step3.er1,
        summary$step4.er1,
        summary$step5.er1)

gser4 = c(summary$step1.er4,
        summary$step2.er4,
        summary$step3.er4,
        summary$step4.er4,
        summary$step5.er4)

net = c(summary$total.step1,
          summary$total.step2,
          summary$total.step3,
          summary$total.step4,
          summary$total.step5)

some = c(summary$change.step1,
        summary$change.step2,
        summary$change.step3,
        summary$change.step4,
        summary$change.step5)

#rearrange
finalgs = top
finalgs = finalgs[-6,c('sitename','eco','lc')]
finalgs$er1 = gser1
finalgs$er4 = gser4
finalgs$net = net
finalgs$some = some
finalgs$scenario = "Growing Season CO2"

finalgs

#annual new sites ###########################################################################################
df1 = fread('./data/reductions/meanreduction_annual.csv')
df2 = fread('./data/reductions/meanreduction_remaining_annual_1.csv')
df3 = fread('./data/reductions/meanreduction_remaining_annual_2.csv')
df4 = fread('./data/reductions/meanreduction_remaining_annual_3.csv')
df5 = fread('./data/reductions/meanreduction_remaining_annual_4.csv')
df6 = fread('./data/reductions/meanreduction_remaining_annual_5.csv')

df1 = subset(df1,df1$means == min(df1$means))
df2 = subset(df2,df2$means == min(df2$means))
df3 = subset(df3,df3$means == min(df3$means))
df4 = subset(df4,df4$means == min(df4$means))
df5 = subset(df5,df5$means == min(df5$means))
df6 = subset(df6,df6$means == min(df6$means))

top = rbind(df1,df2,df3,df4,df5,df6,fill=T)
top = merge(x = top,y = towers,all.x = T,by = 'sitename')
top = top[order(top$means),]
top$sitename
top$lc
top$eco

#load in raster files
imp  = rast('./output/improved_network/improved_annual_2km.tif')
imp1 = rast('./output/improved_network/next_five_sites/improved_annual_2km_1.tif')
imp2 = rast('./output/improved_network/next_five_sites/improved_annual_2km_2.tif')
imp3 = rast('./output/improved_network/next_five_sites/improved_annual_2km_3.tif')
imp4 = rast('./output/improved_network/next_five_sites/improved_annual_2km_4.tif')
imp5 = rast('./output/improved_network/next_five_sites/improved_annual_2km_5.tif')

# dif01 = imp - imp1
# dif12 = imp1 - imp2
# dif23 = imp2 - imp3
# dif34 = imp3 - imp4
# dif45 = imp4 - imp5
# 
# plot(dif01)
# plot(dif12)
# plot(dif23)
# plot(dif34)
# plot(dif45)

#merge all together
all = c(imp,imp1,imp2,imp3,imp4,imp5,clust)

names(all) = c('improved','step1','step2','step3','step4','step5','cluster')
df = as.data.frame(x = all)
summary(df)

#set 1 for meets cutoff and 0 for does not
df$improved.er1 = ifelse(df$improved <= er1,1,0)
df$improved.er4 = ifelse(df$improved <= er4,1,0)

df$step1.er1 = ifelse(df$step1 <= er1,1,0)
df$step1.er4 = ifelse(df$step1 <= er4,1,0)

df$step2.er1 = ifelse(df$step2 <= er1,1,0)
df$step2.er4 = ifelse(df$step2 <= er4,1,0)

df$step3.er1 = ifelse(df$step3 <= er1,1,0)
df$step3.er4 = ifelse(df$step3 <= er4,1,0)

df$step4.er1 = ifelse(df$step4 <= er1,1,0)
df$step4.er4 = ifelse(df$step4 <= er4,1,0)

df$step5.er1 = ifelse(df$step5 <= er1,1,0)
df$step5.er4 = ifelse(df$step5 <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.step1 = df$improved - df$step1
df$change.step2 = df$step1    - df$step2
df$change.step3 = df$step2    - df$step3
df$change.step4 = df$step3    - df$step4
df$change.step5 = df$step4    - df$step5

df$change.count.step1 = ifelse(df$change.step1 > 0 ,1,0)
df$change.count.step2 = ifelse(df$change.step2 > 0 ,1,0)
df$change.count.step3 = ifelse(df$change.step3 > 0 ,1,0)
df$change.count.step4 = ifelse(df$change.step4 > 0 ,1,0)
df$change.count.step5 = ifelse(df$change.step5 > 0 ,1,0)

df = df[complete.cases(df$improved),]
#calculate improvements
summary = df %>%
  summarise(improved.er1 = sum(improved.er1)/sum(all),
            step1.er1    = sum(step1.er1)/sum(all),
            step2.er1    = sum(step2.er1)/sum(all),
            step3.er1    = sum(step3.er1)/sum(all),
            step4.er1    = sum(step4.er1)/sum(all),
            step5.er1    = sum(step5.er1)/sum(all),
            
            improved.er4 = sum(improved.er4)/sum(all),
            step1.er4    = sum(step1.er4)/sum(all),
            step2.er4    = sum(step2.er4)/sum(all),
            step3.er4    = sum(step3.er4)/sum(all),
            step4.er4    = sum(step4.er4)/sum(all),
            step5.er4    = sum(step5.er4)/sum(all),
            
            change.step1   = sum(change.count.step1)/sum(all),
            change.step2   = sum(change.count.step2)/sum(all),
            change.step3   = sum(change.count.step3)/sum(all),
            change.step4   = sum(change.count.step4)/sum(all),
            change.step5   = sum(change.count.step5)/sum(all),
            
            total.step1    = (sum(improved) - sum(step1))/sum(improved),
            total.step2    = (sum(step1) - sum(step2))/sum(step1),
            total.step3    = (sum(step2) - sum(step3))/sum(step2),
            total.step4    = (sum(step3) - sum(step4))/sum(step3),
            total.step5    = (sum(step4) - sum(step5))/sum(step4),
  )

write.csv(x = summary,file = './output/improvements_annual_network_next5.csv')


#rearrange
annualer1 = c(summary$step1.er1,
          summary$step2.er1,
          summary$step3.er1,
          summary$step4.er1,
          summary$step5.er1)

annualer4 = c(summary$step1.er4,
          summary$step2.er4,
          summary$step3.er4,
          summary$step4.er4,
          summary$step5.er4)

net = c(summary$total.step1,
        summary$total.step2,
        summary$total.step3,
        summary$total.step4,
        summary$total.step5)

some = c(summary$change.step1,
         summary$change.step2,
         summary$change.step3,
         summary$change.step4,
         summary$change.step5)

#rearrange
finalannual = top
finalannual = finalannual[-6,c('sitename','eco','lc')]
finalannual$er1 = annualer1
finalannual$er4 = annualer4
finalannual$net = net
finalannual$some = some
finalannual$scenario = "Annual CO2"

finalannual

##############################################################################################


#methane new sites ###########################################################################################
df1 = fread('./data/reductions/meanreduction_methane.csv')
df2 = fread('./data/reductions/meanreduction_remaining_methane_1.csv')
df3 = fread('./data/reductions/meanreduction_remaining_methane_2.csv')
df4 = fread('./data/reductions/meanreduction_remaining_methane_3.csv')
df5 = fread('./data/reductions/meanreduction_remaining_methane_4.csv')
df6 = fread('./data/reductions/meanreduction_remaining_methane_5.csv')

df1 = subset(df1,df1$means == min(df1$means))
df2 = subset(df2,df2$means == min(df2$means))
df3 = subset(df3,df3$means == min(df3$means))
df4 = subset(df4,df4$means == min(df4$means))
df5 = subset(df5,df5$means == min(df5$means))
df6 = subset(df6,df6$means == min(df6$means))

top = rbind(df1,df2,df3,df4,df5,df6,fill=T)
top = merge(x = top,y = towers,all.x = T,by = 'sitename')
top = top[order(top$means),]
top$sitename
top$lc
top$eco


#load in raster files
imp  = rast('./output/improved_network/improved_methane_2km.tif')
imp1 = rast('./output/improved_network/next_five_sites/improved_methane_2km_1.tif')
imp2 = rast('./output/improved_network/next_five_sites/improved_methane_2km_2.tif')
imp3 = rast('./output/improved_network/next_five_sites/improved_methane_2km_3.tif')
imp4 = rast('./output/improved_network/next_five_sites/improved_methane_2km_4.tif')
imp5 = rast('./output/improved_network/next_five_sites/improved_methane_2km_5.tif')

dif01 = imp - imp1
dif12 = imp1 - imp2
dif23 = imp2 - imp3
dif34 = imp3 - imp4
dif45 = imp4 - imp5

plot(dif01)
plot(dif12)
plot(dif23)
plot(dif34)
plot(dif45)

#merge all together
all = c(imp,imp1,imp2,imp3,imp4,imp5,clust)

names(all) = c('improved','step1','step2','step3','step4','step5','cluster')
df = as.data.frame(x = all)
summary(df)
df = df[complete.cases(df$improved),]

#set 1 for meets cutoff and 0 for does not
df$improved.er1 = ifelse(df$improved <= er1,1,0)
df$improved.er4 = ifelse(df$improved <= er4,1,0)

df$step1.er1 = ifelse(df$step1 <= er1,1,0)
df$step1.er4 = ifelse(df$step1 <= er4,1,0)

df$step2.er1 = ifelse(df$step2 <= er1,1,0)
df$step2.er4 = ifelse(df$step2 <= er4,1,0)

df$step3.er1 = ifelse(df$step3 <= er1,1,0)
df$step3.er4 = ifelse(df$step3 <= er4,1,0)

df$step4.er1 = ifelse(df$step4 <= er1,1,0)
df$step4.er4 = ifelse(df$step4 <= er4,1,0)

df$step5.er1 = ifelse(df$step5 <= er1,1,0)
df$step5.er4 = ifelse(df$step5 <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.step1 = df$improved - df$step1
df$change.step2 = df$step1    - df$step2
df$change.step3 = df$step2    - df$step3
df$change.step4 = df$step3    - df$step4
df$change.step5 = df$step4    - df$step5

df$change.count.step1 = ifelse(df$change.step1 > 0 ,1,0)
df$change.count.step2 = ifelse(df$change.step2 > 0 ,1,0)
df$change.count.step3 = ifelse(df$change.step3 > 0 ,1,0)
df$change.count.step4 = ifelse(df$change.step4 > 0 ,1,0)
df$change.count.step5 = ifelse(df$change.step5 > 0 ,1,0)

#calculate improvements
summary = df %>%
  summarise(improved.er1 = sum(improved.er1)/sum(all),
            step1.er1    = sum(step1.er1)/sum(all),
            step2.er1    = sum(step2.er1)/sum(all),
            step3.er1    = sum(step3.er1)/sum(all),
            step4.er1    = sum(step4.er1)/sum(all),
            step5.er1    = sum(step5.er1)/sum(all),
            
            improved.er4 = sum(improved.er4)/sum(all),
            step1.er4    = sum(step1.er4)/sum(all),
            step2.er4    = sum(step2.er4)/sum(all),
            step3.er4    = sum(step3.er4)/sum(all),
            step4.er4    = sum(step4.er4)/sum(all),
            step5.er4    = sum(step5.er4)/sum(all),
            
            change.step1   = sum(change.count.step1)/sum(all),
            change.step2   = sum(change.count.step2)/sum(all),
            change.step3   = sum(change.count.step3)/sum(all),
            change.step4   = sum(change.count.step4)/sum(all),
            change.step5   = sum(change.count.step5)/sum(all),
            
            total.step1    = (sum(improved) - sum(step1))/sum(improved),
            total.step2    = (sum(step1) - sum(step2))/sum(step1),
            total.step3    = (sum(step2) - sum(step3))/sum(step2),
            total.step4    = (sum(step3) - sum(step4))/sum(step3),
            total.step5    = (sum(step4) - sum(step5))/sum(step4),
  )

write.csv(x = summary,file = './output/improvements_methane_network_next5.csv')

#rearrange
methaneer1 = c(summary$step1.er1,
              summary$step2.er1,
              summary$step3.er1,
              summary$step4.er1,
              summary$step5.er1)

methaneer4 = c(summary$step1.er4,
              summary$step2.er4,
              summary$step3.er4,
              summary$step4.er4,
              summary$step5.er4)

net = c(summary$total.step1,
        summary$total.step2,
        summary$total.step3,
        summary$total.step4,
        summary$total.step5)

some = c(summary$change.step1,
         summary$change.step2,
         summary$change.step3,
         summary$change.step4,
         summary$change.step5)

#rearrange
finalmethane = top
finalmethane = finalmethane[-6,c('sitename','eco','lc')]
finalmethane$er1 = methaneer1
finalmethane$er4 = methaneer4
finalmethane$net = net
finalmethane$some = some
finalmethane$scenario = "Growing Season CH4"

finalmethane
##############################################################################################

#methane new sites ###########################################################################################
df1 = fread('./data/reductions/meanreduction_annual_methane.csv')
df2 = fread('./data/reductions/meanreduction_remaining_annual_methane_1.csv')
df3 = fread('./data/reductions/meanreduction_remaining_annual_methane_2.csv')
df4 = fread('./data/reductions/meanreduction_remaining_annual_methane_3.csv')
df5 = fread('./data/reductions/meanreduction_remaining_annual_methane_4.csv')
df6 = fread('./data/reductions/meanreduction_remaining_annual_methane_5.csv')

df1 = subset(df1,df1$means == min(df1$means))
df2 = subset(df2,df2$means == min(df2$means))
df3 = subset(df3,df3$means == min(df3$means))
df4 = subset(df4,df4$means == min(df4$means))
df5 = subset(df5,df5$means == min(df5$means))
df6 = subset(df6,df6$means == min(df6$means))

top = rbind(df1,df2,df3,df4,df5,df6,fill=T)
top = merge(x = top,y = towers,all.x = T,by = 'sitename')
top = top[order(top$means),]
top$sitename
top$lc
top$eco

#load in raster files
imp  = rast('./output/improved_network/improved_annual_methane_2km.tif')
imp1 = rast('./output/improved_network/next_five_sites/improved_annual_methane_2km_1.tif')
imp2 = rast('./output/improved_network/next_five_sites/improved_annual_methane_2km_2.tif')
imp3 = rast('./output/improved_network/next_five_sites/improved_annual_methane_2km_3.tif')
imp4 = rast('./output/improved_network/next_five_sites/improved_annual_methane_2km_4.tif')
imp5 = rast('./output/improved_network/next_five_sites/improved_annual_methane_2km_5.tif')

dif01 = imp - imp1
dif12 = imp1 - imp2
dif23 = imp2 - imp3
dif34 = imp3 - imp4
dif45 = imp4 - imp5

plot(dif01)
plot(dif12)
plot(dif23)
plot(dif34)
plot(dif45)

#merge all together
all = c(imp,imp1,imp2,imp3,imp4,imp5,clust)

names(all) = c('improved','step1','step2','step3','step4','step5','cluster')
df = as.data.frame(x = all)
summary(df)
df = df[complete.cases(df$improved),]

#set 1 for meets cutoff and 0 for does not
df$improved.er1 = ifelse(df$improved <= er1,1,0)
df$improved.er4 = ifelse(df$improved <= er4,1,0)

df$step1.er1 = ifelse(df$step1 <= er1,1,0)
df$step1.er4 = ifelse(df$step1 <= er4,1,0)

df$step2.er1 = ifelse(df$step2 <= er1,1,0)
df$step2.er4 = ifelse(df$step2 <= er4,1,0)

df$step3.er1 = ifelse(df$step3 <= er1,1,0)
df$step3.er4 = ifelse(df$step3 <= er4,1,0)

df$step4.er1 = ifelse(df$step4 <= er1,1,0)
df$step4.er4 = ifelse(df$step4 <= er4,1,0)

df$step5.er1 = ifelse(df$step5 <= er1,1,0)
df$step5.er4 = ifelse(df$step5 <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.step1 = df$improved - df$step1
df$change.step2 = df$step1    - df$step2
df$change.step3 = df$step2    - df$step3
df$change.step4 = df$step3    - df$step4
df$change.step5 = df$step4    - df$step5

df$change.count.step1 = ifelse(df$change.step1 > 0 ,1,0)
df$change.count.step2 = ifelse(df$change.step2 > 0 ,1,0)
df$change.count.step3 = ifelse(df$change.step3 > 0 ,1,0)
df$change.count.step4 = ifelse(df$change.step4 > 0 ,1,0)
df$change.count.step5 = ifelse(df$change.step5 > 0 ,1,0)

#calculate improvements
summary = df %>%
  summarise(improved.er1 = sum(improved.er1)/sum(all),
            step1.er1    = sum(step1.er1)/sum(all),
            step2.er1    = sum(step2.er1)/sum(all),
            step3.er1    = sum(step3.er1)/sum(all),
            step4.er1    = sum(step4.er1)/sum(all),
            step5.er1    = sum(step5.er1)/sum(all),
            
            improved.er4 = sum(improved.er4)/sum(all),
            step1.er4    = sum(step1.er4)/sum(all),
            step2.er4    = sum(step2.er4)/sum(all),
            step3.er4    = sum(step3.er4)/sum(all),
            step4.er4    = sum(step4.er4)/sum(all),
            step5.er4    = sum(step5.er4)/sum(all),
            
            change.step1   = sum(change.count.step1)/sum(all),
            change.step2   = sum(change.count.step2)/sum(all),
            change.step3   = sum(change.count.step3)/sum(all),
            change.step4   = sum(change.count.step4)/sum(all),
            change.step5   = sum(change.count.step5)/sum(all),
            
            total.step1    = (sum(improved) - sum(step1))/sum(improved),
            total.step2    = (sum(step1) - sum(step2))/sum(step1),
            total.step3    = (sum(step2) - sum(step3))/sum(step2),
            total.step4    = (sum(step3) - sum(step4))/sum(step3),
            total.step5    = (sum(step4) - sum(step5))/sum(step4),
  )

write.csv(x = summary,file = './output/improvements_annual_methane_network_next5.csv')

#rearrange
annualmethaneer1 = c(summary$step1.er1,
               summary$step2.er1,
               summary$step3.er1,
               summary$step4.er1,
               summary$step5.er1)

annualmethaneer4 = c(summary$step1.er4,
               summary$step2.er4,
               summary$step3.er4,
               summary$step4.er4,
               summary$step5.er4)

net = c(summary$total.step1,
        summary$total.step2,
        summary$total.step3,
        summary$total.step4,
        summary$total.step5)

some = c(summary$change.step1,
         summary$change.step2,
         summary$change.step3,
         summary$change.step4,
         summary$change.step5)

#rearrange
finalannualmethane = top
finalannualmethane = finalannualmethane[-6,c('sitename','eco','lc')]
finalannualmethane$er1 = annualmethaneer1
finalannualmethane$er4 = annualmethaneer4
finalannualmethane$net = net
finalannualmethane$some = some
finalannualmethane$scenario = "Year-round CH4"

finalannualmethane

##############################################################################################
#### combine all for table 4 ####

fin = rbind(finalgs,finalmethane,finalannual,finalannualmethane)

write.csv(x = fin,file = './figures/table 4.csv',row.names = F)

