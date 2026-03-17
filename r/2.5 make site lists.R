
library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)

#load in extracted site data from extraction codes
tower.data = fread(file = './data/pca.towers.csv')


#active sites 2022 ########################################################
tower.data$active.2022 = ifelse(tower.data$End_CO2 >= 2024 & tower.data$Start_CO2 < 2022,'active','inactive')

#custom turn off scotty creek, lutose, and council NGEE
tower.data$active.2022 = ifelse(tower.data$site == 'Scotty Creek Landscape','inactive',tower.data$active.2022)
tower.data$active.2022 = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active.2022)
tower.data$active.2022 = ifelse(tower.data$site == 'Council (NGEE Arctic)','inactive',tower.data$active.2022)

#active 2024 ##############################################################
tower.data$active.2024 = ifelse(tower.data$End_CO2 >= 2024,'active','inactive')

#custom turn off scotty creek, lutose, and council NGEE
#tower.data$active.2024 = ifelse(tower.data$site == 'Scotty Creek Landscape','inactive',tower.data$active.2024)
tower.data$active.2024 = ifelse(tower.data$site == 'Lutose Rich Fen','inactive',tower.data$active.2024)
tower.data$active.2024 = ifelse(tower.data$site == 'Council (NGEE Arctic)','inactive',tower.data$active.2024)

#test
active22 = subset(tower.data,tower.data$active.2022 == 'active')$site
active24 = subset(tower.data,tower.data$active.2024 == 'active')$site

setdiff(active24,active22)

#methane sites 2022 #####################################################################################
tower.data$methane.2022 = ifelse(tower.data$GHG == 'CO2,CH4' | tower.data$GHG == 'CO2,CH4,N2O','methane','nonmethane')
tower.data$methane.2022 = ifelse(tower.data$site == 'Lutose','nonmethane',tower.data$methane.2022)

#methane sites 2024 ##############################################################################
tower.data$methane.2024 = ifelse(tower.data$GHG == 'CO2,CH4' | tower.data$GHG == 'CO2,CH4,N2O','methane','nonmethane')
tower.data$methane.2024 = ifelse(tower.data$site == 'Lutose','nonmethane',tower.data$methane.2022)
tower.data$site

#test
active22 = subset(tower.data,tower.data$active.2022 == 'active' & tower.data$methane.2022 == 'methane')$site
active24 = subset(tower.data,tower.data$active.2024 == 'active' & tower.data$methane.2024 == 'methane')$site

setdiff(active24,active22)

#year-round sites 2022 #####################################################################################
#change tower sites we increased to all year coverage
tower.data$Season_Activity.2022  = ifelse(tower.data$site == "Lutose" |
                                          tower.data$site == "Scotty Creek Landscape" |
                                          tower.data$site == "Steen River" |
                                          tower.data$site == "Scotty Creek Bog" |
                                    #     tower.data$site == "Resolute Bay" |
                                          tower.data$site == "Smith Creek",
                                      'Summer',tower.data$Season_Activity)

tower.data$Season_Activity.2024  = tower.data$Season_Activity

#test
active22 = subset(tower.data,tower.data$active.2022 == 'active' & tower.data$Season_Activity.2022 == 'All year')$site
active24 = subset(tower.data,tower.data$active.2024 == 'active' & tower.data$Season_Activity.2024 == 'All year')$site

setdiff(active24,active22)


#annual methane sites 2022 ##############################################################################
tower.data$annualmethane2022 = ifelse(tower.data$active.2022 == 'active' & tower.data$methane.2022 == 'methane' & tower.data$Season_Activity.2022 == 'All year','annualmethane','no')

tower.data$annualmethane2024 = ifelse(tower.data$active.2024 == 'active' & tower.data$methane.2024 == 'methane' & tower.data$Season_Activity.2024 == 'All year','annualmethane','no')

#test
active22 = subset(tower.data,tower.data$annualmethane2022 == 'annualmethane')$site
active24 = subset(tower.data,tower.data$annualmethane2024 == 'annualmethane')$site

setdiff(active24,active22)


write.csv(x = tower.data,file = './data/final.tower.data.csv',row.names = F)
