library(raster)

m<- raster('E:/temp/mask30.tif')

colmin <- colFromX(m, 152.8)
colmax <- colFromX(m, 153)
rowmax <- rowFromY(m, -27.31)
rowmin <- rowFromY(m, -27.2)

r <- m
outname <- paste0('e:/temp/mask302.tif')
outR<-writeStart(r,filename=outname, overwrite=TRUE,datatype="INT1U")

for (i in 1:nrow(m)) { 
  print(i)
  vals<-getValues(m, i)
  
  if(i>=rowmin & i<=rowmax){
    vals[colmin:colmax] <- 1
  }
  outR <- writeValues(outR, vals, i)
}

outR<-writeStop(outR)
