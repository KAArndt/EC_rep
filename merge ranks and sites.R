
library(data.table)

ranks = fread('./data/reductions/meanreduction_remaining.csv')
sites = fread('./data/pca.towers.upgraded.csv')

names(ranks)[1] = 'site'

sites = merge(sites,ranks,by = 'site')
names(sites)

sites = sites[,c('site','means','Latitude','Longitude','country','State','Population')]
sites = sites[order(sites$means),]
sites$rank = seq(1,length(sites$site))

write.csv(sites,'./data/next_best_sites.csv')
