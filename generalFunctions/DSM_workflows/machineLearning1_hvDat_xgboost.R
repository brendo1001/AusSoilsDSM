# Trials with various data mining algorithms: GBM (stochastic gradient boost)

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
## XGboost model with defaults
# Bootstrapping and optimisation of: 


### Trees
#nrounds: 
#max_depth:
#eta:
#gamma:
#colsample_bytree:
#min_child_weight:
#subsample:
xg.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "xgbTree",nthread =3)  
summary(xg.model)
xg.model


### Linear boost (appear to overfit)
#nrounds
#lambda
#alpha
#eta
xg.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "xgbLinear",nthread =3)  
summary(xg.model)
xg.model

# predict on calibration data
xg.pred_c<-predict(xg.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = xg.pred_c, plot.it = T)

# predict on validation data
xg.pred_v<-predict(xg.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = xg.pred_v, plot.it = T)

# predict onto grid
xg.grid<- predict(hunterCovariates_sub, xg.model)
plot(xg.grid)


## XGBoost model fit: fiddly around with tuning hyperparameters
#
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 2)

# repeated cv with 5 repeats (linear)
xg.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "xgbLinear",trControl = fitControl,nthread=3)  
xg.model
summary(xg.model)


# expand the tuning possibiliites
fitControl <- trainControl(method = "repeatedcv", number=10,
                           repeats = 2)

tgrid <- expand.grid(
  .nrounds = c(50,100,150,250,500), 
  .lambda = seq(0,1,0.25),
  .alpha = seq(0, 1, 0.25),
  .eta = seq(0.1,0.9,0.1))

# Notes this seems to take a while
xg.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "xgbLinear",trControl = fitControl, tuneGrid = tgrid, nthread=4)  
xg.model
summary(xg.model)




### PREDICT
# predict on calibration data
xg.pred_c<-predict(xg.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = xg.pred_c, plot.it = T)

# predict on validation data
xg.pred_v<-predict(xg.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = xg.pred_v, plot.it = T)

# predict onto grid
xg.grid<- predict(hunterCovariates_sub, xg.model)
plot(xg.grid)


### Note that this code has been expemplified using the xbgLinear rather than xbgTree.
### xbgTree has quite a few more hyperparameters 
### xbgLinear does appear to overfit.
### There does not appear to be much difference in terms of the validation between either xbgTree and xgbLinear




