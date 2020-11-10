library(stringr)


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  inDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC'
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/Ross/FC/FCMosaics'
}else{
  inDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC'
  outDir <- '/datasets/work/af-tern-mal-deb/work/Ross/FC/FCMosaics'
}


# d <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC/Tiffs/0_-19'
# fls <- list.files(path=d, recursive = T, pattern = '.tif', full.names = F )

types <- c('Max_BS', 'Max_NPV', 'Max_PV', 'Mean_BS', 'Mean_NPV', 'Mean_PV', 'Min_BS', 'Min_NPV', 'Min_PV', 'SD_BS', 'SD_NPV', 'SD_PV' )

print('Loading rasters.....')


for (i in 1:length(types)) {
  print(i)
  fil <- types[i]
  fls <- list.files(path=paste0(inDir, '/geoTiles'), recursive = T, pattern = fil, full.names = T )
  print(length(fls))
  csvPath <- paste0(inDir, '/FC_Tiles_', fil,'.csv' )
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  o <- system(paste0('"C:/Program Files/QGIS 3.12/bin/gdalbuildvrt.exe" -input_file_list ',csvPath, ' ', paste0(inDir, '/FC_', fil,'.vrt' )))
}

