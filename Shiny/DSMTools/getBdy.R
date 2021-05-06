library(raster)
library(rgdal)
library(rgeos)
library(imager)


rp <- 'C:/Projects/Myanmar/BrendanData/Slope.tif'

rp <- 'C:/Projects/Myanmar/MarkData/Convergence_960.tif'

r <- raster(rp)
rr <- r/r + 2
rr[is.na(rr)] <-1
plot(rr)

lns <- rasterToContour(rr)
lines(lns)
writeOGR(c, 'c:/temp', 'Template', driver="ESRI Shapefile")
plot(lns)

lt <- lns@lines[[1]]

x <- sapply(lt@Lines, function(v) return(v@coords[,1]))
xx <- unlist(x)
y <- sapply(lt@Lines, function(v) return(v@coords[,2]))
yy<- unlist(y)



df <- data.frame(xx,yy)
# dfs <- df
# df2 <- df2 <- rbind(df, data.frame(df[1,]))


     
     coordinates(df) <- ~ xx+yy
     hpts <- chull(df@coords)
     hpts <- c(hpts, hpts[1])

library(sf)
outer  <- as.matrix(df@coords[hpts,])
pts = list(outer)
p <- st_polygon(pts)
g = st_sfc(p)
st_write(g, 'c:/temp/bdy2.shp', delete_layer=TRUE)



#########   Full working workflow from here 


rp <- 'C:/Projects/Myanmar/MarkData/Convergence_960.tif'

rp <- 'C:/Projects/Myanmar/BrendanData/Slope.tif'

r <- raster(rp)
# rr <- r/r + 2
# rr[is.na(rr)] <-1
# plot(rr)

rb <- boundaries(r,classes=FALSE)

rb[rb==0] <-NA
plot(rb)
rpts <- rasterToPoints(rb)[,1:2]

rpts2 <- rpts[seq(1, nrow(rpts)-1, 10),]
sdf <- sort_points(as.data.frame(rpts2), y='y', x='x')

sdf2 <- rbind(sdf, data.frame(sdf[1,]))
plot(sdf2)

pts = list(as.matrix(sdf2))
p <- st_polygon(pts)
g = st_sfc(p)
plot(g)
st_write(g, 'c:/temp/bdy2.shp', delete_layer=TRUE)



r <- raster(nrows=10, ncols=10)
adjacent(r, cells=c(1, 55), directions=8, pairs=TRUE) 

a <- adjacent(r, cell = c(1,55,90), directions=4, sorted=TRUE) 
a





install.packages("devtools")
library(devtools)
install_github("skgrange/gissr")
library()

devtools::install_github("skgrange/threadr")




sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE) {
  
  # NA check, if NAs drop them
  if (any(is.na(c(df[, y], df[, x])))) {
    
    # Remove NAs
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    
    # Raise warning
    warning("Missing coordinates were detected and have been removed.", 
            call. = FALSE)
    
    # Check 
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
    
  }
  
  # Get centre (-oid) point of points
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  # d$angle_degrees <- d$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # Return
  df
  
}





r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
plot(r)
pol <- rasterToPolygons(r, fun=function(x){x>6})
plot(pol)
plot(r)
lines(pol)


plot(rb)
rpts <- rasterToPoints(rb)[,1:2]

cells <- cellFromXY(rb, cbind(rpts))

#rc <- rowColFromCell(rb, cells)

plot(rb)

ocells <- numeric(length = length(cells))
c1 <- adjacent(r, cells=c(cells[i]), directions=8, pairs=F)[1]
ocells[1] <- c1

for (i in 2:length(cells)) {
  
 c2 <- adjacent(rb, cells=c(cells[i]), directions=8, pairs=F, id=T)
 
 rowcol <- rowColFromCell(rb, cells[i])
 b <- getValuesBlock(rb, row=rowcol[1,1] -1, nrows=3, col=rowcol[1,2]-1, ncols=3, format='matrix')
 b[2,2] <- NA
 which(b==1)[1]
 #getValuesFocal(rb, row=rowcol[1,1], nrows=1, ngb=3, names=TRUE)
 
 if(ki == 1){
   delta <- c(-1,-1)
  else if(k==2){
     
   }
 }
  
}


library(sf)

ext <- extent(r)
lx <- numeric(5)
ly <- numeric(5)
ly[1]
matrix(1:9, nrow = 3, ncol = 3)


p <- as(ext, 'SpatialPolygons')
SPDF = SpatialPolygonsDataFrame(p, data =  data.frame(N = c("one")))
writeOGR(SPDF, 'c:/temp', 'Template', driver="ESRI Shapefile", overwrite_layer = T)

plot(p)
g = st_sfc(p)
plot(g)
st_write(g, 'c:/temp/bdy2.shp', delete_layer=TRUE)

st_bbox(ext)
