#TODO Moving window approach to DSM: 20km tiles for 25 grids
#Procedure:
#1. Import a base grid (base grid)
#2. get the bounding box of the grid
#3. set a block size deemed approriate for tiling
#4. Automatic creation of tiles with set size within the bounding box
#5. Offset the tiles and run the automation again (optional)


root<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/vectors/25m_tiles/"
library(raster);library(maptools);library(rgdal);


## Australian Boundary
aus_bound<- readOGR("/datasets/work/af-tern-mal-deb/work/datasets/national/Australia Topo/60803_shp/framework/aus10fgd_l.shp")
plot(aus_bound)


# Base grid (dem)
list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/PCS/Organisms/",  pattern="tif$", full.names=FALSE)
files<- list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/PCS/Organisms/",  pattern="tif$",full.names=T)
files

#The baseline raster
base.r<- raster()
base.r
#plot(base.r)
#bounding box of base raster
min.y<- base.r@extent@ymin 
max.y<- base.r@extent@ymax
min.x<- base.r@extent@xmin
max.x<- base.r@extent@xmax

range.x<- max.x - min.x
range.y<- max.y - min.y

b.box<- c(min.y,max.y,min.x,max.x)
b.box

# Make a polygon of the bounding box
pAll<- Polygon(cbind(c(b.box[3],b.box[4],b.box[4],b.box[3],b.box[3]),c(b.box[2],b.box[2],b.box[1],b.box[1],b.box[2])))
PZ = Polygons(list(pAll), 1)
SpP = SpatialPolygons(list(PZ))
crs(SpP)<- crs(base.r)
SpP
dataf<- data.frame(f=1)
SpD<-SpatialPolygonsDataFrame(SpP, dataf)
plot(SpD, add=T)
writeOGR(SpD, root, "AusContBB_25m", "ESRI Shapefile")



#window parameters
win.size<- 0.2 #size of window (m)
overlap.size<- 0 #amount of window overlap (m)
buff<-0.05 #buffer size for edge


test.win<-window.frame(b.box, win.size,operlap.size,buff,outs=T,nms="t1") # run the first time





#FUNCTION

#The function (its messy but more-or-less works ok)
window.frame<- function(b.box, win.size,operlap.size,buff,outs,nms){
  
  #set up first sqaure 
  x1<- b.box[3]
  x2<- b.box[3]+win.size
  y1<- b.box[1]
  y2<- b.box[1]+win.size
  p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
  transList<- list(p1)
  
  
  while (x2 != b.box[4]) { # 1 column at a time
    
    while (y2!= b.box[2]){ # within each column now row by row
      p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
      transList<- c(transList,p1)
      P1 = Polygons(list(p1), "p1")
      SpP = SpatialPolygons(list(P1))
      plot(SpP, add=TRUE, bg="transparent")
      x1<- x1
      x2<- x2
      y1<- y2 - overlap.size
      if(((b.box[2]-y2)-win.size) > (buff*win.size)) {y2<- y1+win.size} else {y2<- b.box[2]
      p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
      transList<- c(transList,p1)     
      P1 = Polygons(list(p1), "p1")
      SpP = SpatialPolygons(list(P1))
      plot(SpP,add=TRUE,bg="transparent")}} #END ROW by ROW
    y1<- b.box[1]
    y2<- b.box[1]+win.size
    x1<- x2-overlap.size
    if(((b.box[4]-x2)-win.size) > (buff*win.size)) {x2<- x1+win.size} else {x2<- b.box[4]
    p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
    #transList<- c(transList,p1)  
    P1 = Polygons(list(p1), "p1")
    SpP = SpatialPolygons(list(P1))
    plot(SpP,add=TRUE,bg="transparent")}
    while (y2!= b.box[2]){
      p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
      transList<- c(transList,p1)  
      P1 = Polygons(list(p1), "p1")
      SpP = SpatialPolygons(list(P1))
      plot(SpP,add=TRUE,bg="transparent")
      x1<- x1
      x2<- x2
      y1<- y2 - overlap.size
      if(((b.box[2]-y2)-win.size) > (buff*win.size)) {y2<- y1+win.size} else {y2<- b.box[2]
      p1<- Polygon(cbind(c(x1,x2,x2,x1,x1),c(y2,y2,y1,y1,y2)))
      transList<- c(transList,p1)  
      P1 = Polygons(list(p1), "p1")
      SpP = SpatialPolygons(list(P1))
      plot(SpP,add=TRUE,bg="transparent")}}}
  
  transList<- transList[2:length(transList)] # the list that needs to be output
  
  #write a single polygon with all single polygons
  # Make polgons
  result <- vector("list",length(transList)) 
  for (i in 1:length(transList)){
    result[[i]]<-Polygons(list(transList[[i]]), ID=i)}
  SpP = SpatialPolygons(result)
  plot(SpP)
  SpP
  FID<- data.frame(1:length(transList))
  names(FID)<- "FID"
  zz<-SpatialPolygonsDataFrame(SpP,data=FID)
  crs(zz)<- crs(base.r)
  zz
  writeOGR(zz, root, "allTiles_combined_25m", "ESRI Shapefile")
  
  saveRDS(object = transList, file = "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/vectors/list_tiles25m.rds")
  # make individual shapefiles of outputs iof required (optional output)
  if (outs==T){for (i in 1:length(transList)){
    pz<-Polygons(list(transList[[i]]),"list")
    SpP = SpatialPolygons(list(pz))
    zz<-SpatialPolygonsDataFrame(SpP,data=data.frame(i,row.names="list"))
    crs(zz)<- crs(base.r)
    name1<- paste(nms,paste("square",i,sep="_"),sep="_")
    writeOGR(zz, getwd(), name1, "ESRI Shapefile")
    print(i)}} else {print("no outputs selected")}
  return (transList)} #END FUNCTION
