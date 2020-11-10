library(ncdf4)
library(RCurl)

#http://dapds00.nci.org.au/thredds/catalog.html

rootOut <- 'e:/temp/wofsOut'
xv <- seq(-19, 21, 1)
yv <- seq(-10, -43, -1)


k=1
for (i in 1:length(xv)) {
  for (j in 1:length(yv)) {
    print(k)
    
    url <- paste0('http://dapds00.nci.org.au/thredds/fileServer/fk4/datacube/002/WOfS/WOfS_Filt_Stats_25_2_1/netcdf/wofs_filtered_summary_', xv[i], '_',yv[j],'.nc')
    if(url.exists(url)){
      
      download.file(url, destfile = 'e:/temp/x.nc', quiet = T, mode = 'wb')
      
      nc <- ncdf4::nc_open( 'e:/temp/x.nc')
      md = ncvar_get( nc, nc$var$wofs_filtered_summary, start=c(1,1,1), count=c(nc$dim$x$len, nc$dim$y$len, 1) )
      ncr <- raster(as.matrix(t(md)), xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats))
      
      k=k+1
      outfile <- paste0(rootOut, '/r_', k, '.tif')
      writeRaster(ncr, outfile, overwrite=T)
      
      }
  }
}


