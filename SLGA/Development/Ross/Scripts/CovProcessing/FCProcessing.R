library(tictoc)
library(raster)
library(httr)
library(XML)
library(xml2)
library(stringr)
library(htmltidy)
library(ncdf4)
library(RCurl)

#http://dapds00.nci.org.au/thredds/catalog.html
#http://dapds00.nci.org.au/thredds/catalog/fk4/datacube/002/FC/FC-percentile/ANNUAL/catalog.html
#https://cmi.ga.gov.au/sites/default/files/2017-12/DEA%20Tiles_1.pdf
#http://cmi.ga.gov.au/sites/default/files/2017-12/DEA_v2_Albers_tiles_Australia.zip


machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  outDir <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Ross/SLGAData/FC'
}else{
  outDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGAData/FC'
}

tic()
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
print(paste0("Processing iteraration = ", k))

#k=50

dirs <- list.dirs(paste0(outDir, '/NC'), full.names = T, recursive = F)

length(dirs)

dir <- dirs[k]

fls <- list.files(dir, full.names = T, pattern='.nc')

downDir <-  paste0(outDir, '/NC/', dir )

print(paste0('Input Directory - ', downDir))
atts<- c('PV_PC_50', 'NPV_PC_50','BS_PC_50')
print(paste0('Loading Data......'))
for (j in 1:length(atts)) {
  
  stk <- stack()
  att <- atts[j]
    for (i in 1:length(fls)) {
        f <- fls[i]
       
        
        res <- try(nc <- ncdf4::nc_open(f))
        if(inherits(res, "try-error"))
        {
          print(paste0('ERROR Loading - ', f ))
          next
        }else{
          print(f)
          md = ncvar_get( nc, att, start=c(1,1,1), count=c(nc$dim$x$len, nc$dim$y$len, 1) )
          r <- raster(as.matrix(t(md)), xmn=min(nc$dim$x$vals), xmx=max(nc$dim$x$vals), ymn=min(nc$dim$y$vals), ymx=max(nc$dim$y$vals))
          stk <- addLayer(stk, r)
        }
      }

    print('Calculating stats.....')
    ravg <- calc(stk, fun=mean, na.rm=T)
    print('Finished calculating the mean')
    rmin <- calc(stk, fun=min, na.rm=T)
    print('Finished calculating the min')
    rmax <- calc(stk, fun=max, na.rm=T)
    print('Finished calculating the max')
    rstd <- calc(stk, fun=sd, na.rm=T)
    print('Finished calculating the standard deviation')

    outDR <- paste0(outDir, '/Tiffs/', basename(dir))
    if(!dir.exists(outDR)){dir.create(outDR, recursive = T)}
    d <- basename(dir)

    print('Saving rasters....')
    print(outDR)

    proj <- '+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
    crs(ravg) <- proj
    writeRaster(ravg, paste0(outDR, '/Mean_', att, '_', d, '.tif'), overwrite=T, datatype='INT1U')
    crs(rmin) <- proj
    writeRaster(rmin, paste0(outDR, '/Min_', att, '_', d, '.tif'), overwrite=T, datatype='INT1U')
    crs(rmax) <- proj
    writeRaster(rmax, paste0(outDR, '/Max_', att, '_', d, '.tif'), overwrite=T, datatype='INT1U')
    crs(rstd) <- proj
    writeRaster(rstd, paste0(outDR, '/SD_', att, '_', d, '.tif'), overwrite=T, datatype='INT1U')
      
}

toc()
print(paste0('Finished Successfully'))


