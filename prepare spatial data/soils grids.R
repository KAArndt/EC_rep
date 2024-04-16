rm(list = ls())
setwd('C:/Users/karndt.WHRC/Desktop/site.selection/')

#library(raster)
library(svMisc)
library(MASS)
library(ggplot2)
library(ggspatial)
library(terra)
#library(sf)

#base Extrapolation index image from TNC shapefile
eco = vect('./data/input data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp')

#subset to rock and ice and tundra and boreal
eco = subset(eco,eco$WWF_MHTNAM == 'Rock and Ice' | 
                 eco$WWF_MHTNAM == 'Tundra' |
                 eco$WWF_MHTNAM == 'Boreal Forests/Taiga')

#crop to the northern regions
eco = crop(x = eco,y = c(-180, 180, 43, 83.6236))

############################################################
#Newer soil files from soil grids https://soilgrids.org/ ############################################################

#bd
bd00  = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl1_1km_ll.tif')
bd05  = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl2_1km_ll.tif')
bd15  = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl3_1km_ll.tif')
bd30  = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl4_1km_ll.tif')
bd60  = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl5_1km_ll.tif')
bd100 = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl6_1km_ll.tif')
#bd200 = rast('./data/input data/soil grids 2017/bd/BLDFIE_M_sl7_1km_ll.tif')

bd = c(bd00,bd05,bd15,bd30,bd60,bd100)

#resize to the area and size of the base image
bd = crop(x = bd,y = eco)
#bd = mask(x = bd,mask = eco)

#calculating aggregate soil layers
bd$bd_100_agg = 1./100*(5.*(bd$BLDFIE_M_sl1_1km_ll+bd$BLDFIE_M_sl2_1km_ll)/2 +
                        10.*(bd$BLDFIE_M_sl2_1km_ll+bd$BLDFIE_M_sl3_1km_ll)/2 +
                        15.*(bd$BLDFIE_M_sl3_1km_ll+bd$BLDFIE_M_sl4_1km_ll)/2 +
                        30.*(bd$BLDFIE_M_sl4_1km_ll+bd$BLDFIE_M_sl5_1km_ll)/2 +
                        40.*(bd$BLDFIE_M_sl5_1km_ll+bd$BLDFIE_M_sl6_1km_ll)/2)

plot(bd$bd_100_agg/1000)
bd = bd$bd_100_agg/1000

#SOC stock
#soc0_30  = rast('./data/input data/soil grids 2017/soc.stock/OCSTHA_M_30cm_1km_ll.tif')
soc0_100 = rast('./data/input data/soil grids 2017/soc.stock/OCSTHA_M_100cm_1km_ll.tif')
#soc0_200 = rast('./data/input data/soil grids 2017/soc.stock/OCSTHA_M_200cm_1km_ll.tif')

#soc.stock = c(soc0_30,soc0_100,soc0_200)

soc.stock = crop(x = soc0_100,y = eco)
#soc.stock = mask(x = soc.stock,mask = eco)

#plot(soc.stock)
soc = soc.stock

#soc density
# soc00  = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl1_1km_ll.tif')
# soc05  = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl2_1km_ll.tif')
# soc15  = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl3_1km_ll.tif')
# soc30  = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl4_1km_ll.tif')
# soc60  = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl5_1km_ll.tif')
# soc100 = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl6_1km_ll.tif')
# soc200 = rast('./data/input data/soil grids 2017/soc.density/OCDENS_M_sl7_1km_ll.tif')
# 
# soc.dens = c(soc00,soc05,soc15,soc30,soc60,soc100,soc200)
# soc.dens = crop(x = soc.dens,y = extent(base))
# 
# plot(soc.dens)
# 
# #calculating aggregate soil layers
# soc.dens$soc0_100 = 1./100*(5.*(soc.dens$OCDENS_M_sl1_1km_ll+soc.dens$OCDENS_M_sl2_1km_ll)/2 +
#                         10.*(soc.dens$OCDENS_M_sl2_1km_ll+soc.dens$OCDENS_M_sl3_1km_ll)/2 +
#                         15.*(soc.dens$OCDENS_M_sl3_1km_ll+soc.dens$OCDENS_M_sl4_1km_ll)/2 +
#                         30.*(soc.dens$OCDENS_M_sl4_1km_ll+soc.dens$OCDENS_M_sl5_1km_ll)/2 +
#                         40.*(soc.dens$OCDENS_M_sl5_1km_ll+soc.dens$OCDENS_M_sl6_1km_ll)/2)
# 
# plot(soc.dens$soc0_100)

#ph
ph00  = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl1_1km_ll.tif')
ph05  = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl2_1km_ll.tif')
ph15  = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl3_1km_ll.tif')
ph30  = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl4_1km_ll.tif')
ph60  = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl5_1km_ll.tif')
ph100 = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl6_1km_ll.tif')
#ph200 = rast('./data/input data/soil grids 2017/ph/PHIHOX_M_sl7_1km_ll.tif')

ph = c(ph00,ph05,ph15,ph30,ph60,ph100)

ph = crop(x = ph,y = eco)
#ph = mask(x = ph,mask = eco)

ph$ph0_100 = 1./100*(5.*(ph$PHIHOX_M_sl1_1km_ll+ph$PHIHOX_M_sl2_1km_ll)/2 +
                              10.*(ph$PHIHOX_M_sl2_1km_ll+ph$PHIHOX_M_sl3_1km_ll)/2 +
                              15.*(ph$PHIHOX_M_sl3_1km_ll+ph$PHIHOX_M_sl4_1km_ll)/2 +
                              30.*(ph$PHIHOX_M_sl4_1km_ll+ph$PHIHOX_M_sl5_1km_ll)/2 +
                              40.*(ph$PHIHOX_M_sl5_1km_ll+ph$PHIHOX_M_sl6_1km_ll)/2)

#plot(ph$ph0_100/10)
ph = ph$ph0_100/10

#clay
clay00  = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl1_1km_ll.tif')
clay05  = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl2_1km_ll.tif')
clay15  = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl3_1km_ll.tif')
clay30  = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl4_1km_ll.tif')
clay60  = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl5_1km_ll.tif')
clay100 = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl6_1km_ll.tif')
#clay200 = rast('./data/input data/soil grids 2017/clay/CLYPPT_M_sl7_1km_ll.tif')

clay = c(clay00,clay05,clay15,clay30,clay60,clay100)

clay = crop(x = clay,y = eco)
#clay = mask(x = clay,mask = eco)

#calculating aggregate soil layers
clay$clay_100_agg = 1./100*(5.*(clay$CLYPPT_M_sl1_1km_ll+clay$CLYPPT_M_sl2_1km_ll)/2 +
                              10.*(clay$CLYPPT_M_sl2_1km_ll+clay$CLYPPT_M_sl3_1km_ll)/2 +
                              15.*(clay$CLYPPT_M_sl3_1km_ll+clay$CLYPPT_M_sl4_1km_ll)/2 +
                              30.*(clay$CLYPPT_M_sl4_1km_ll+clay$CLYPPT_M_sl5_1km_ll)/2 +
                              40.*(clay$CLYPPT_M_sl5_1km_ll+clay$CLYPPT_M_sl6_1km_ll)/2)

#plot(clay$clay_100_agg)
clay = clay$clay_100_agg

#silt
silt00  = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl1_1km_ll.tif')
silt05  = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl2_1km_ll.tif')
silt15  = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl3_1km_ll.tif')
silt30  = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl4_1km_ll.tif')
silt60  = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl5_1km_ll.tif')
silt100 = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl6_1km_ll.tif')
#silt200 = rast('./data/input data/soil grids 2017/silt/SLTPPT_M_sl7_1km_ll.tif')

silt = c(silt00,silt05,silt15,silt30,silt60,silt100)

silt = crop(x = silt,y = eco)
#silt = mask(x = silt,mask = eco)

#calculating aggregate soil layers
silt$silt_100_agg = 1./100*(5.*(silt$SLTPPT_M_sl1_1km_ll+silt$SLTPPT_M_sl2_1km_ll)/2 +
                              10.*(silt$SLTPPT_M_sl2_1km_ll+silt$SLTPPT_M_sl3_1km_ll)/2 +
                              15.*(silt$SLTPPT_M_sl3_1km_ll+silt$SLTPPT_M_sl4_1km_ll)/2 +
                              30.*(silt$SLTPPT_M_sl4_1km_ll+silt$SLTPPT_M_sl5_1km_ll)/2 +
                              40.*(silt$SLTPPT_M_sl5_1km_ll+silt$SLTPPT_M_sl6_1km_ll)/2)

#plot(silt$silt_100_agg)
silt = silt$silt_100_agg

#sand
sand00  = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl1_1km_ll.tif')
sand05  = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl2_1km_ll.tif')
sand15  = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl3_1km_ll.tif')
sand30  = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl4_1km_ll.tif')
sand60  = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl5_1km_ll.tif')
sand100 = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl6_1km_ll.tif')
#sand200 = rast('./data/input data/soil grids 2017/sand/SNDPPT_M_sl7_1km_ll.tif')

sand = c(sand00,sand05,sand15,sand30,sand60,sand100)

sand = crop(x = sand,y = eco)
#sand = mask(x = sand,mask = eco)

#calculating aggregate soil layers
sand$sand_100_agg = 1./100*(5.*(sand$SNDPPT_M_sl1_1km_ll+sand$SNDPPT_M_sl2_1km_ll)/2 +
                              10.*(sand$SNDPPT_M_sl2_1km_ll+sand$SNDPPT_M_sl3_1km_ll)/2 +
                              15.*(sand$SNDPPT_M_sl3_1km_ll+sand$SNDPPT_M_sl4_1km_ll)/2 +
                              30.*(sand$SNDPPT_M_sl4_1km_ll+sand$SNDPPT_M_sl5_1km_ll)/2 +
                              40.*(sand$SNDPPT_M_sl5_1km_ll+sand$SNDPPT_M_sl6_1km_ll)/2)

#plot(sand$sand_100_agg)
sand = sand$sand_100_agg

#soil temps and permafrost probability ##########################
pp = rast('./data/input data/pfrost/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif')

pp = project(x = pp,y = soc,method = 'near')
#pp = crop(x = pp,y = eco)
#pp = mask(x = pp,mask = eco)

plot(pp)

#combine all
soils = c(bd,ph,soc,sand,silt,clay,pp)

#plot(soils)

writeRaster(x = soils,filename = './data/input data/soils.tif',overwrite=T)
