
## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  basePath <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work'
}else{
  basePath <- '/datasets/work/af-tern-mal-deb/work'
}

root.out<- paste0(basePath, "/datasets/national/covariates/tiles_25m/")
slurm.out<- paste0(basePath, "/datasets/national/covariates/vectors/rcode/slurm_out/")
transList<- readRDS(paste0(basePath, "/datasets/national/covariates/vectors/list_tiles25m.rds"))
length(transList)


srt<- 1
fin<- 5000

# Base grid (dem)
list.files(paste0(basePath, "/datasets/national/covariates/mosaics/PCS/Organisms/"),  pattern="tif$", full.names=FALSE)
files<- list.files(paste0(basePath, "/datasets/national/covariates/mosaics/PCS/Organisms/"),  pattern="tif$",full.names=T)
files

#The baseline raster
base.r<- raster(files[1])
base.r


###
# begin parallel cluster and register it with foreach
#cpus<- 8
#cl<- makeCluster(spec=cpus)
# register with foreach
#registerDoParallel(cl)


# Apply model to each tile
#oper1<- foreach(i=srt:fin, .packages = c("sp", "rgdal")) %dopar% {
for(i in srt:fin){
  print(i)
  
  # select polygon
  pz<-Polygons(list(transList[[i]]),"list")
  SpP = SpatialPolygons(list(pz))
  
  # crop the rasters
  test <- try(crop(base.r, extent(SpP), snap="out"),TRUE)
  if(isTRUE(class(test)=="try-error")){next} else {
    tempD <- data.frame(cellNos = seq(1:ncell(test)))
    vals <- as.data.frame(getValues(test))
    tempD <- cbind(tempD, vals)
    lna<- length(which(is.na(tempD[,2])))}
  if(lna == nrow(tempD)){next} else {
    # make a new directory
    new.dir<- paste0(root.out,i,"/")
    dir.create(new.dir)
    fr <- rasterize(SpP, test)   #rasterise the polygon 
    lr <- mask(x=test, mask=fr) #use the mask 
    lr.reproj <- projectRaster(from = lr, crs = crs(lr), method = "ngb", res = 0.0002777778)
    writeRaster(x = lr.reproj,filename = paste0(new.dir,"cstone.tif"),format = "GTiff",datatype = "FLT4S", overwrite = TRUE )}}


