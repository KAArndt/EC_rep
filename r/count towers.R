
library(data.table)
d22 = fread('./data/pca.towers.base.csv')
d24 = fread('./data/pca.towers.upgraded.csv')
d24$active = ifelse(d24$site == 'Lutose Rich Fen', 'inactive',d24$active)

#total active towers
a22 = subset(d22,d22$active == 'active' & d22$Start_CO2 < 2022)
a24 = subset(d24,d24$active == 'active')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total methane towers
a22 = subset(d22,d22$active == 'active' & d22$Start_CO2 < 2022 & d22$methane == 'methane')
a24 = subset(d24,d24$active == 'active' & d24$methane == 'methane')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total annual towers
a22 = subset(d22,d22$active == 'active' & d22$Start_CO2 < 2022 & d22$Season_Activity == 'All year')
a24 = subset(d24,d24$active == 'active' & d24$Season_Activity == 'All year')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site

#total annual CH4 towers
a22 = subset(d22,d22$active == 'active' & d22$Start_CO2 < 2022 & d22$Season_Activity == 'All year' & d22$methane == 'methane')
a24 = subset(d24,d24$active == 'active' & d24$Season_Activity == 'All year' & d24$methane == 'methane')

m = merge(a22,a24,all = T,by='site')
m = subset(m,is.na(m$MeanTemp.x))

m$site
