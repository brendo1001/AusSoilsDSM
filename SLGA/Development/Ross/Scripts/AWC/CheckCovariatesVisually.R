library(raster)
library(stringr)


#covDir <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/datasets/national/covariates'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'

fls <- list.files(covDir, pattern = '.tif$', full.names = T)
for (i in 1:length(fls)) {
  print(fls[i])
  r <-raster(fls[i])
  f <- paste0(covDir, '/Thumbnails/', str_replace(basename(fls[i]), '.tif', '.png') )
  png(file = f, bg = "transparent")
  plot(r, maxpixels=100000, main=basename(fls[i])) 
  dev.off()
}



