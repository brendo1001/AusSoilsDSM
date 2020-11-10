

library(raster)

rasterOptions(progress = 'text')

p <- '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/lztmre_aus_y20002011_dmaa2_d20050630.gri'
r <- raster(p)
writeRaster(r, '/datasets/work/af-tern-mal-deb/work/Ross/New/Auscover/FinalMosaics/lztmre_aus_y20002011_dmaa2_d20050630.tif')
