rm(list = ls())
gc()

library(terra)
library(dplyr)

#load in the base network and improvement map
base   = rast('./output/base_2kmv2.tif')
base.i = rast('./output/improve_2kmv2.tif')

meth   = rast('./output/methane_2kmv2.tif')
meth.i = rast('./output/impch4_2kmv2.tif')

annu   = rast('./output/annual_2kmv2.tif')
annu.i = rast('./output/impanu_2kmv2.tif')

anme   = rast('./output/annual_methane_2kmv2.tif')
anme.i = rast('./output/imp_an_methane_2kmv2.tif')

#load in clusters
clust = rast('./output/clusts.tif')
clust = clust$km40

#merge all together
all = c(base,base.i,meth,meth.i,annu,annu.i,anme,anme.i,clust)

names(all) = c('base','base.i','meth','meth.i','annu','annu.i','anme','anme.i','clust')
df = as.data.frame(x = all)
summary(df)

#the cutoff values from the previous exercises (step 4)
er1 = 1.67
er4 = 1.56

#set 1 for meets cutoff and 0 for does not
df$base.er1 = ifelse(df$base <= er1,1,0)
df$base.er4 = ifelse(df$base <= er4,1,0)

df$base.i.er1 = ifelse(df$base.i <= er1,1,0)
df$base.i.er4 = ifelse(df$base.i <= er4,1,0)

#methane
df$meth.er1 = ifelse(df$meth <= er1,1,0)
df$meth.er4 = ifelse(df$meth <= er4,1,0)

df$meth.i.er1 = ifelse(df$meth.i <= er1,1,0)
df$meth.i.er4 = ifelse(df$meth.i <= er4,1,0)

#annual
df$annu.er1 = ifelse(df$annu <= er1,1,0)
df$annu.er4 = ifelse(df$annu <= er4,1,0)

df$annu.i.er1 = ifelse(df$annu.i <= er1,1,0)
df$annu.i.er4 = ifelse(df$annu.i <= er4,1,0)

#annual methane
df$anme.er1 = ifelse(df$anme <= er1,1,0)
df$anme.er4 = ifelse(df$anme <= er4,1,0)

df$anme.i.er1 = ifelse(df$anme.i <= er1,1,0)
df$anme.i.er4 = ifelse(df$anme.i <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.base = df$base - df$base.i
df$change.meth = df$meth - df$meth.i
df$change.annu = df$annu - df$annu.i
df$change.anme = df$anme - df$anme.i

df$change.count.base = ifelse(df$change.base > 0 ,1,0)
df$change.count.meth = ifelse(df$change.meth > 0 ,1,0)
df$change.count.annu = ifelse(df$change.annu > 0 ,1,0)
df$change.count.anme = ifelse(df$change.anme > 0 ,1,0)

#calculate improvements
summary = df %>%
  summarise(base.er1 = sum(base.er1)/sum(all),
            base.i.er1  = sum(base.i.er1)/sum(all),
            base.er4 = sum(base.er4)/sum(all),
            base.i.er4  = sum(base.i.er4)/sum(all),
            
            meth.er1 = sum(meth.er1)/sum(all),
            meth.i.er1  = sum(meth.i.er1)/sum(all),
            meth.er4 = sum(meth.er4)/sum(all),
            meth.i.er4  = sum(meth.i.er4)/sum(all),
            
            annu.er1 = sum(annu.er1)/sum(all),
            annu.i.er1  = sum(annu.i.er1)/sum(all),
            annu.er4 = sum(annu.er4)/sum(all),
            annu.i.er4  = sum(annu.i.er4)/sum(all),
            
            anme.er1 = sum(anme.er1)/sum(all),
            anme.i.er1  = sum(anme.i.er1)/sum(all),
            anme.er4 = sum(anme.er4)/sum(all),
            anme.i.er4  = sum(anme.i.er4)/sum(all),
            
            change.base   = sum(change.count.base)/sum(all),
            change.meth   = sum(change.count.meth)/sum(all),
            change.annu   = sum(change.count.annu)/sum(all),
            change.anme   = sum(change.count.anme)/sum(all),

            total.base    = (sum(base) - sum(base.i))/sum(base),
            total.meth    = (sum(meth) - sum(meth.i))/sum(meth),
            total.annu    = (sum(annu) - sum(annu.i))/sum(annu),
            total.anme    = (sum(anme) - sum(anme.i))/sum(anme),
            )

write.csv(x = summary,file = './output/improvements.csv')
summary
# % area well covered area
pi = sum(df$base.i.er1) - sum(df$base.er1)
pi*3.43

# % area with some increase
sum(df$all)*3.43*.37


sum(df$change)
sum(df$base.dist)
sum(df$improve)/sum(df$base.dist)

all$dif = all$improve - all$base.dist

plot(all$dif)


