

library(raster)

r <- raster('E:/Auscover/alpsbk_aust_y2009_sd5a2.tif')
plot(r, maxpixels=100000)

Aust <- read_sf('D:/Projects/GIS/AustOutlineGeneralisedshp.shp')

AustProj <- st_transform(Aust, CRS('+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs '))
plot(st_geometry(AustProj),  axes = TRUE)

#x<- ncol(r)/100
xsize <- 2500
ysize <- 2500
cs <- 30
bufm <- cs * 5

ext <- extent(r)
startx <- ext@xmin
xits <- ceiling((ext@xmax - ext@xmin)/(xsize*cs))-1
yits  <- ceiling((ext@ymax - ext@ymin)/(ysize*cs))-1
its <- xits * yits
l = vector("list", length = its)
c=1

for (i in 1:xits) {
  
  endx <-startx + (xsize * cs)
  starty <- ext@ymin

  for (j in 1:yits) {
    
            endy <-starty + (ysize * cs)
            box = matrix(c(startx-bufm,starty-bufm,startx-bufm,endy+bufm,endx+bufm,endy+bufm,endx+bufm,starty-bufm,startx-bufm,starty-bufm),ncol=2, byrow=T)
            p1 <- st_polygon(list(box))
            l[[c]] = p1
            c=c+1
            starty <- endy
            #print(st_touches(p1, AustProj, sparse = FALSE))
            
          
        }
  startx <- endx
}

shp = st_sf(st_sfc(l))
st_crs(shp) <- st_crs(AustProj)
#plot(AustProj, add = T)
plot(shp, add = T)



ts <- st_intersects(shp, AustProj, sparse = FALSE)
ts1<-apply(ts, 1, any)



shp$inter <- ts1
plot(shp)

st_write(shp, 'e:/temp/allsquares.shp', delete_layer=T)
st_write(shp[shp$inter,], 'e:/temp/intersquares.shp', delete_layer=T)


plot(shp[shp$inter,])


p<-shp[shp$inter,]
plot(p[1000,], add=T, col='blue')






