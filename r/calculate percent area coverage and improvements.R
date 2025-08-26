
library(terra)
library(dplyr)
library(data.table)

#load in the base network and improvement map
base     = rast('./output/base_network/base_2km.tif')
base.i   = rast('./output/improved_network/improved_base_2km.tif')
base.abc = rast('./output/abc_network/abc_base_2km.tif')

meth     = rast('./output/base_network/methane_2km.tif')
meth.i   = rast('./output/improved_network/improved_methane_2km.tif')
meth.abc = rast('./output/abc_network/abc_methane_2km.tif')

annu     = rast('./output/base_network/annual_2km.tif')
annu.i   = rast('./output/improved_network/improved_annual_2km.tif')
annu.abc = rast('./output/abc_network/abc_annual_2km.tif')

anme     = rast('./output/base_network/annual_methane_2km.tif')
anme.i   = rast('./output/improved_network/improved_annual_methane_2km.tif')
anme.abc = rast('./output/abc_network/abc_annual_methane_2km.tif')

#load in clusters
clust = rast('./output/clusts.tif')
clust = clust$km40

#merge all together
all = c(base,base.i,base.abc,meth,meth.i,meth.abc,annu,annu.i,annu.abc,anme,anme.i,anme.abc,clust)

names(all) = c('base','base.i','base.abc','meth','meth.i','meth.abc','annu','annu.i','annu.abc','anme','anme.i','anme.abc','clust')
df = as.data.frame(x = all,na.rm=T)
summary(df)

#the cutoff values from the previous exercises (step 4, mean)
er1 = 1.96
er4 = 1.56

#set 1 for meets cutoff and 0 for does not
df$base.er1 = ifelse(df$base <= er1,1,0)
df$base.er4 = ifelse(df$base <= er4,1,0)

df$base.i.er1 = ifelse(df$base.i <= er1,1,0)
df$base.i.er4 = ifelse(df$base.i <= er4,1,0)

df$base.abc.er1 = ifelse(df$base.abc <= er1,1,0)
df$base.abc.er4 = ifelse(df$base.abc <= er4,1,0)

#methane
df$meth.er1 = ifelse(df$meth <= er1,1,0)
df$meth.er4 = ifelse(df$meth <= er4,1,0)

df$meth.i.er1 = ifelse(df$meth.i <= er1,1,0)
df$meth.i.er4 = ifelse(df$meth.i <= er4,1,0)

df$meth.abc.er1 = ifelse(df$meth.abc <= er1,1,0)
df$meth.abc.er4 = ifelse(df$meth.abc <= er4,1,0)

#annual
df$annu.er1 = ifelse(df$annu <= er1,1,0)
df$annu.er4 = ifelse(df$annu <= er4,1,0)

df$annu.i.er1 = ifelse(df$annu.i <= er1,1,0)
df$annu.i.er4 = ifelse(df$annu.i <= er4,1,0)

df$annu.abc.er1 = ifelse(df$annu.abc <= er1,1,0)
df$annu.abc.er4 = ifelse(df$annu.abc <= er4,1,0)

#annual methane
df$anme.er1 = ifelse(df$anme <= er1,1,0)
df$anme.er4 = ifelse(df$anme <= er4,1,0)

df$anme.i.er1 = ifelse(df$anme.i <= er1,1,0)
df$anme.i.er4 = ifelse(df$anme.i <= er4,1,0)

df$anme.abc.er1 = ifelse(df$anme.abc <= er1,1,0)
df$anme.abc.er4 = ifelse(df$anme.abc <= er4,1,0)

df$all = 1

#set a count for each pixel that experienced some change
df$change.base = df$base - df$base.i
df$change.meth = df$meth - df$meth.i
df$change.annu = df$annu - df$annu.i
df$change.anme = df$anme - df$anme.i

df$change.base.abc = df$base.abc - df$base.i
df$change.meth.abc = df$meth.abc - df$meth.i
df$change.annu.abc = df$annu.abc - df$annu.i
df$change.anme.abc = df$anme.abc - df$anme.i

df$change.count.base = ifelse(df$change.base > 0 ,1,0)
df$change.count.meth = ifelse(df$change.meth > 0 ,1,0)
df$change.count.annu = ifelse(df$change.annu > 0 ,1,0)
df$change.count.anme = ifelse(df$change.anme > 0 ,1,0)

df$change.count.base.abc = ifelse(df$change.base.abc > 0 ,1,0)
df$change.count.meth.abc = ifelse(df$change.meth.abc > 0 ,1,0)
df$change.count.annu.abc = ifelse(df$change.annu.abc > 0 ,1,0)
df$change.count.anme.abc = ifelse(df$change.anme.abc > 0 ,1,0)

#calculate improvements
summary = df %>%
  summarise(base.er1    = sum(base.er1)/sum(all),
            base.i.er1  = sum(base.i.er1)/sum(all),
            base.abc.er1  = sum(base.abc.er1)/sum(all),
            
            base.er4    = sum(base.er4)/sum(all),
            base.i.er4  = sum(base.i.er4)/sum(all),
            base.abc.er4  = sum(base.abc.er4)/sum(all),
            
            meth.er1    = sum(meth.er1)/sum(all),
            meth.i.er1  = sum(meth.i.er1)/sum(all),
            meth.abc.er1  = sum(meth.abc.er1)/sum(all),
            
            meth.er4    = sum(meth.er4)/sum(all),
            meth.i.er4  = sum(meth.i.er4)/sum(all),
            meth.abc.er4  = sum(meth.abc.er4)/sum(all),
            
            annu.er1    = sum(annu.er1)/sum(all),
            annu.i.er1  = sum(annu.i.er1)/sum(all),
            annu.abc.er1  = sum(annu.abc.er1)/sum(all),
            
            annu.er4    = sum(annu.er4)/sum(all),
            annu.i.er4  = sum(annu.i.er4)/sum(all),
            annu.abc.er4  = sum(annu.abc.er4)/sum(all),
            
            anme.er1    = sum(anme.er1)/sum(all),
            anme.i.er1  = sum(anme.i.er1)/sum(all),
            anme.abc.er1  = sum(anme.abc.er1)/sum(all),
            
            anme.er4    = sum(anme.er4)/sum(all),
            anme.i.er4  = sum(anme.i.er4)/sum(all),
            anme.abc.er4  = sum(anme.abc.er4)/sum(all),
            
            change.base   = sum(change.count.base)/sum(all),
            change.meth   = sum(change.count.meth)/sum(all),
            change.annu   = sum(change.count.annu)/sum(all),
            change.anme   = sum(change.count.anme)/sum(all),

            change.base.abc   = sum(change.count.base.abc)/sum(all),
            change.meth.abc   = sum(change.count.meth.abc)/sum(all),
            change.annu.abc   = sum(change.count.annu.abc)/sum(all),
            change.anme.abc   = sum(change.count.anme.abc)/sum(all),
            
            total.base    = (sum(base) - sum(base.i))/sum(base),
            total.meth    = (sum(meth) - sum(meth.i))/sum(meth),
            total.annu    = (sum(annu) - sum(annu.i))/sum(annu),
            total.anme    = (sum(anme) - sum(anme.i))/sum(anme),
            
            total.base.abc     = (sum(base.abc) - sum(base.i))/sum(base.abc),
            total.meth.abc     = (sum(meth.abc) - sum(meth.i))/sum(base.abc),
            total.annu.abc     = (sum(annu.abc) - sum(annu.i))/sum(base.abc),
            total.anme.abc     = (sum(anme.abc) - sum(anme.i))/sum(base.abc),
            )

write.csv(x = summary,file = './output/improvements.csv')
summary
