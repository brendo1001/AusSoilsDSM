library(raster)


rasters <- stack(c('E:/BEin/blue.tif', 'E:/BEin/green.tif', 'E:/BEin/red.tif'))

writeRaster(rasters, 'e:/temp/BErgb.tif', datatype = 'INT2S', overwrite=TRUE, NAvalue=-9999.0)
tempdir()

tempdir()


write("TMPDIR = e:/rtmp/", file=file.path(Sys.getenv('TMPDIR'), '.Renviron'))
write("R_USER = e:/rtmp/", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
tempdir()

