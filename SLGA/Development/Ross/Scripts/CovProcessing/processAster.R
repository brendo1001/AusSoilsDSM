library(raster)
library(ncdf4)

att <- 'Aus_Mainland_Ferrous_Iron_Index'

url <- paste0('http://dapds00.nci.org.au/thredds/fileServer/wx7/aster/vnir/Aus_Mainland/', att, '_reprojected.nc4')
#download.file(url, 'e:/temp/aster.nc', mode='wb')


f <- 'E:/temp/aster.nc'
nc <- ncdf4::nc_open(f, suppress_dimvals=F, readunlim=F)
#md = ncvar_get( nc, nc$var$Ferrous_Iron_Index, start=c(1,1), count=c(nc$dim$lon$len, nc$dim$lat$len) )



# md = ncvar_get( nc, nc$var$Ferrous_Iron_Index, start=c(40000,40000), count=c(10000, 10000) )
# r <- raster(as.matrix(t(md)), xmn=1, xmx=10000, ymn=1, ymx=10000)
# plot(r)
#r <- raster(as.matrix(t(md)), xmn=min(nc$dim$lon$vals), xmx=max(nc$dim$x$vals), ymn=min(nc$dim$y$vals), ymx=max(nc$dim$y$vals))



#latseq <- seq(1, 104917, 10000)
#lonseq <- seq(1, 148509, 10000)
xmin = nc$dim$lon$vals[1]
xmax = nc$dim$lon$vals[nc$dim$lon$len]
ymin = nc$dim$lat$vals[1]
ymax = nc$dim$lat$vals[nc$dim$lat$len]






rasterOptions( tmpdir='e:/temp/xtemp')
r <- raster(nrows=104917, ncols=148509, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax) 
bs <- blockSize(r, chunksize = length(nc$dim$lon$vals)*1000)
outname <- paste0('e:/temp/Aster_',att, '.tif')
outR<-writeStart(r,filename=outname, overwrite=TRUE,datatype="FLT4S")
itr <- 1
for (i in 1:bs$n) { 
  
  print(paste0(itr,' of ', bs$n))
  md = ncvar_get( nc, nc$var$Ferrous_Iron_Index, start=c(1,bs$row[i]), count=c(nc$dim$lon$len, bs$nrows[i]) )
  #md[md<-1000]<NA
  outR <- writeValues(outR, md, bs$row[i])
  itr = itr+1
  # md=NULL
  # gc()
}


outR<-writeStop(outR)
outR
plot(outR)


