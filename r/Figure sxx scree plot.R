
# library(dplyr)
library(ggplot2)
# library(ggspatial)
# library(foreach)
# library(doParallel)
# library(doSNOW)
# library(cowplot)
# library(sf)
# library(viridis)
# library(Polychrome)

km = readRDS(file = './output/clusters.rds')

#error calculations for deciding appropriate amount of clusters
cents = c(5,10,20,30,40,50,60,70,80,90,100,200,300,400) 

error  = km[[1]]$tot.withinss
for (i in 2:length(km)) {
  error = c(error,km[[i]]$tot.withinss)
}

slope = vector(length = length(error)-1)
for (i in 1:length(error)-1) {
  slope[i] = (error[i]-error[i+1])/(cents[i]-cents[i+1])
}


#error kmeans plot
png(filename = './figures/Figure Sxx kmeans_error.png',width = 5,height = 2.5,units = 'in',res = 2000)
ggplot()+theme_bw()+
  geom_line(aes(cents,error))+
  geom_point(aes(cents,error))+
  scale_x_continuous('Clusters',limits = c(0,410),expand = c(0,0),breaks = cents,
                     labels = c('5','','20','','40','','60','','80','','100','200','300','400'))+
  scale_y_continuous('Within-Cluster Sum of Squares Error')+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.minor.x = element_blank())
dev.off()
