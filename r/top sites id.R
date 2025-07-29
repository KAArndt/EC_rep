library(data.table)
library(ggplot2)

#growing season methane
gs1 = fread('./output/reductions/meanreduction_remaining.csv')
gs2 = fread('./output/reductions/meanreduction_remaining_1.csv')
gs3 = fread('./output/reductions/meanreduction_remaining_2.csv')
gs4 = fread('./output/reductions/meanreduction_remaining_3.csv')
gs5 = fread('./output/reductions/meanreduction_remaining_4.csv')

gs1 = subset(gs1,gs1$means == min(gs1$means))
gs2 = subset(gs2,gs2$means == min(gs2$means))
gs3 = subset(gs3,gs3$means == min(gs3$means))
gs4 = subset(gs4,gs4$means == min(gs4$means))
gs5 = subset(gs5,gs5$means == min(gs5$means))

gs = rbind(gs1,gs2,gs3,gs4,gs5)
gs$scenario = 'growing season CO2'

#growing season methane
meth1 = fread('./output/reductions/meanreduction_methane.csv')
meth2 = fread('./output/reductions/meanreduction_remaining_methane_1.csv')
meth3 = fread('./output/reductions/meanreduction_remaining_methane_2.csv')
meth4 = fread('./output/reductions/meanreduction_remaining_methane_3.csv')
meth5 = fread('./output/reductions/meanreduction_remaining_methane_4.csv')

meth1 = subset(meth1,meth1$means == min(meth1$means))
meth2 = subset(meth2,meth2$means == min(meth2$means))
meth3 = subset(meth3,meth3$means == min(meth3$means))
meth4 = subset(meth4,meth4$means == min(meth4$means))
meth5 = subset(meth5,meth5$means == min(meth5$means))

meth = rbind(meth1,meth2,meth3,meth4,meth5,fill=T)
meth$scenario = 'growing season CH4'


#annual co2
anco21 = fread('./output/reductions/meanreduction_annual.csv')
anco22 = fread('./output/reductions/meanreduction_remaining_annual_1.csv')
anco23 = fread('./output/reductions/meanreduction_remaining_annual_2.csv')
anco24 = fread('./output/reductions/meanreduction_remaining_annual_3.csv')
anco25 = fread('./output/reductions/meanreduction_remaining_annual_4.csv')

anco21 = subset(anco21,anco21$means == min(anco21$means))
anco22 = subset(anco22,anco22$means == min(anco22$means))
anco23 = subset(anco23,anco23$means == min(anco23$means))
anco24 = subset(anco24,anco24$means == min(anco24$means))
anco25 = subset(anco25,anco25$means == min(anco25$means))

anco2 = rbind(anco21,anco22,anco23,anco24,anco25,fill=T)
anco2$scenario = 'annual co2'

#annual co2
anch41 = fread('./output/reductions/meanreduction_annual_methane.csv')
anch42 = fread('./output/reductions/meanreduction_remaining_annual_methane_1.csv')
anch43 = fread('./output/reductions/meanreduction_remaining_annual_methane_2.csv')
anch44 = fread('./output/reductions/meanreduction_remaining_annual_methane_3.csv')
anch45 = fread('./output/reductions/meanreduction_remaining_annual_methane_4.csv')

anch41 = subset(anch41,anch41$means == min(anch41$means))
anch42 = subset(anch42,anch42$means == min(anch42$means))
anch43 = subset(anch43,anch43$means == min(anch43$means))
anch44 = subset(anch44,anch44$means == min(anch44$means))
anch45 = subset(anch45,anch45$means == min(anch45$means))

anch4 = rbind(anch41,anch42,anch43,anch44,anch45,fill=T)
anch4$scenario = 'annual ch4'

fin = rbind(gs,meth,anco2,anch4)


  

