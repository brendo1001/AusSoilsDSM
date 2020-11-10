library(raster)
library(tictoc)

rasterOptions(progress = 'text', memfrac = 0.5 )

args = commandArgs(trailingOnly=TRUE)

k = as.numeric(args[1])

print(paste0('K = ', k))




inRP <- paste0('M:/work/Ross/New/Auscover/Tiles/', 'lztmre_aus_y20002011_dmaa2_d20050630.vrt')
outR <- paste0('M:/work/Ross/New/Auscover/FinalMosaics/', 'lztmre_aus_y20002011_dmaa2_d20050630')

inRP <= paste0('/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/Tiles/', 'lztmre_aus_y20002011_dmaa2_d20050630.vrt')
outR <- paste0('/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/', 'lztmre_aus_y20002011_dmaa2_d20050630')
inR <- raster(inRP)
writeRaster(inR, outR)

tic()




