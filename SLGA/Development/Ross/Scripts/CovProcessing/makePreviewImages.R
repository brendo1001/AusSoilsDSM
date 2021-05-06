library(raster)
library(RColorBrewer)
library(png)
library(stringr)


indir1 <- 'M:/work/datasets/national/covariates/mosaics/90m'
indir1 <- 'M:/work/datasets/national/covariates/mosaics/30m'
  
fls1 <-  list.files(indir1, recursive = F, full.names = T, pattern = '*.tif$')

for (i in 1:length(fls1)) {
 
  print(paste0(i, ' of ', length(fls1)))
  fl <- fls1[i]
  outp <- str_replace( basename(fl), '.tif', '.png')
    outfile <- paste0('e:/temp/pngs/', outp)
  if(!file.exists(outfile)){
    png(outfile, width=1400, height=1200)
    r <- raster(fl)
    plot(r, col = rainbow(n=20))
    dev.off()
  }
}


