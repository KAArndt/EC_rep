rm(list = ls())

library(terra)
library(dplyr)

base = rast('./output/base_2km.tif')
improve = rast('./output/improve_2km.tif')
clust = rast('./output/clusts.tif')
clust = clust$km100

names(improve) = 'improve'

all = c(base,improve,clust)

df = as.data.frame(x = all)
summary(df$base.dist)

er1 = 1.64
er4 = 1.51

df$base.er1 = ifelse(df$base.dist <= er1,1,0)
df$base.er4 = ifelse(df$base.dist <= er4,1,0)

df$new.er1 = ifelse(df$improve <= er1,1,0)
df$new.er4 = ifelse(df$improve <= er4,1,0)

df$all = 1

df$change = df$base.dist - df$improve
df$change.count = ifelse(df$change > 0 ,1,0)
sum(df$change)


summary = df %>%
  summarise(base.er1 = sum(base.er1)/sum(all),
            new.er1  = sum(new.er1)/sum(all),
            base.er4 = sum(base.er4)/sum(all),
            new.er4  = sum(new.er4)/sum(all),
            change   = sum(change.count)/sum(all),
            total    = (sum(base.dist) - sum(improve))/sum(base.dist)
            )


summary(df$improve)
summary(df$base.dist)


mean(df$improve)
mean(df$base.dist)

sum(df$change)
sum(df$base.dist)
sum(df$improve)/sum(df$base.dist)

all$dif = all$improve - all$base.dist

plot(all$dif)


