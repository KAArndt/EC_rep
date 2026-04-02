
library(data.table)
df = fread('./data/final.tower.data.csv')

#total active towers
a22 = subset(df,df$active.2022 == 'active')
a24 = subset(df,df$active.2024 == 'active')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total methane towers
a22 = subset(df,df$active.2022 == 'active' & df$methane.2022 == 'methane')
a24 = subset(df,df$active.2024 == 'active' & df$methane.2024 == 'methane')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total annual towers
a22 = subset(df,df$active.2022 == 'active' & df$Season_Activity.2022 == 'All year')
a24 = subset(df,df$active.2024 == 'active' & df$Season_Activity.2024 == 'All year')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total annual CH4 towers
a22 = subset(df,df$active.2022 == 'active' & df$annualmethane2022 == 'annualmethane')
a24 = subset(df,df$active.2024 == 'active' & df$annualmethane2024 == 'annualmethane')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

