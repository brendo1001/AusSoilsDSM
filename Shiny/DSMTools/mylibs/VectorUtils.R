library(rgdal)


# sf <- 'C:/Projects/AgDataShop/Data/McClelland/MachineryData/Harvest/2013/Shapefiles/01 - Goldings_WHEAT_2013.shp'
#
# dsf <- 'C:/Temp/shp/05-Rogers_Barley-Spring_2013.shp'
# shp <- readShapeFile(sf)
# deleteShapefile(dsf)
# filenameSHP <- dsf

readShapeFile <- function(filenameSHP)
{
  src <- dirname(filenameSHP)
  lyr <- str_replace_all(basename(filenameSHP), ".shp", "")
  paddockbdys <- readOGR(src, layer=lyr)
}

readKMLFile <- function(filenameSHP)
{
  paddockbdys <- readOGR(filenameSHP)
}

deleteShapefile <- function(filenameSHP){
  lyr <- str_replace_all(basename(filenameSHP), ".shp", "")
  src <- dirname(filenameSHP)
  paths = list.files( paste0(src), pattern = lyr, full.names = T, recursive =T)
  unlink(paths)

}

writeShapeFile <- function(df, filenameSHP)
{
  src <- dirname(filenameSHP)
  lyr <- str_replace_all(basename(filenameSHP), ".shp", "")
  #writeOGR(src, layer=lyr)
  writeOGR(df, src, lyr, driver="ESRI Shapefile", overwrite_layer=TRUE)
}