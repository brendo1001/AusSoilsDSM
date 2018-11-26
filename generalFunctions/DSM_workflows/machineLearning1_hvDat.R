# Trials with various data mining algorithms: Ranger

## variable selection
## hyperparameter optimisation
## predicting onto raster

## Libraries
#install.packages("devtools") 
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg") #ithir package
library(ithir);library(caret);library(raster);library(rgdal);library(sp);library(MASS)


##########################################################################################################
## Common dataset 
#point data
data(HV_subsoilpH)

# Start afresh 
#round pH data to 2 decimal places
HV_subsoilpH$pH60_100cm<- round(HV_subsoilpH$pH60_100cm, 2)

#remove already intersected data
HV_subsoilpH<- HV_subsoilpH[,1:3]

#add an id column
HV_subsoilpH$id<- seq(1, nrow(HV_subsoilpH), by = 1)

#re-arrange order of columns
HV_subsoilpH<- HV_subsoilpH[,c(4,1,2,3)]

#Change names of coordinate columns
names(HV_subsoilpH)[2:3]<- c("x", "y")


#grids (covariate raster)
data(hunterCovariates_sub)

## make data a sptatial object
coordinates(HV_subsoilpH)<- ~ x + y

# covariate data extract
DSM_data<- extract(hunterCovariates_sub, HV_subsoilpH, sp= 1, method = "simple")
DSM_data<- as.data.frame(DSM_data)
str(DSM_data)

## remove any missing values
which(!complete.cases(DSM_data))
DSM_data<- DSM_data[complete.cases(DSM_data),]

# Calibrarion and validation datasets
## Random holdback
set.seed(123)
training <- sample(nrow(DSM_data), 0.70 * nrow(DSM_data))
DSM_data_c<- DSM_data[training,]
DSM_data_v<- DSM_data[-training,]
###########################################################################################################################


##########################################################################################################################
## Ranger model with defaults
# does 25 bootstrapped samples
ranger.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "ranger", num.trees = 1000)  
summary(ranger.model)
ranger.model

# predict on calibration data
ranger.pred_c<-predict(ranger.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = ranger.pred_c, plot.it = T)

# predict on calibration data
ranger.pred_v<-predict(ranger.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = ranger.pred_v, plot.it = T)

# predict onto grid
ranger.grid<- predict(hunterCovariates_sub, ranger.model)
plot(ranger.grid)


## Ranger model simple fit
# does 25 bootstrapped samples
ranger.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "ranger",trControl =trainControl(method = "oob"), num.trees = 1000)  
summary(ranger.model)
ranger.model

# predict on calibration data
ranger.pred_c<-predict(ranger.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = ranger.pred_c, plot.it = T)

# predict on calibration data
ranger.pred_v<-predict(ranger.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = ranger.pred_v, plot.it = T)

# predict onto grid
ranger.grid<- predict(hunterCovariates_sub, ranger.model)
plot(ranger.grid)



