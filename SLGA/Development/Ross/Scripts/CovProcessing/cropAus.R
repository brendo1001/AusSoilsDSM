library(raster)
rasterOptions(progress = 'text', memfrac = 0.3)

templateR <- raster('M:/work/datasets/national/covariates/mosaics/30m/Masked/mask30.tif')
inR <- raster('M:/work/Ross/New/Auscover/Tiles/alpsbk_aust_y2009_sd5a2/geo_8.tif')


inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC'


#att<-'alpsbk_aust_y2009_se5a2'
#att<-'lztmre_aus_y20002011_dm7a2_d20050630'
#att<-'lztmre_aus_y20002011_dmaa2_d20050630'


  fls <- list.files(path=paste0('M:/work/Ross/New/Auscover/Tiles/', att), recursive = F, pattern = '.tif$', full.names = T )
  print(length(fls))
  csvPath <- paste0('M:/work/Ross/New/Auscover/Tiles', '/', att,'.csv' )
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  o <- system(paste0('"C:/Program Files/QGIS 3.12/bin/gdalbuildvrt.exe" -input_file_list ',csvPath, ' ',  paste0('M:/work/Ross/New/Auscover/Tiles', '/', att,'.vrt' )))

  
  rx <- raster(paste0('M:/work/Ross/New/Auscover/Tiles/', att, '.vrt'))
  crop(rx, templateR, align='out', filename=paste0('M:/work/Ross/New/Auscover/FinalMosaics/', att, '.tif'), overwrite=T)
  
  r1 <- raster('e:/temp/alpsbk_aust_y2009_sd5a2_TEST.tif')
compareRaster(r2, templateR)  
r2 <- alignExtent(templateR, r1)

r2 <- setExtent(r1, templateR)

sextent

hr <- raster('M:/work/Ross/New/HeatMask/ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_max.tif')

r1
templateR





r2 <- raster('M:/work/Ross/FC/FCMosaicsFilled/mask_FC_Mean_PV.tif')




