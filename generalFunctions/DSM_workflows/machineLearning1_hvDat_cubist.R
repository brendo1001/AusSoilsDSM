# Trials with various data mining algorithms: Cubist

#stratified k-fold repeated cross-validation

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

# transects
ts<- as.factor(rep(1:80 ,length.out=nrow(HV_subsoilpH)))
ts
ts[order(ts)]


#strata
nrow(HV_subsoilpH)
sum(35,55,62,80,18)
s<- c(rep(1,60), rep(2,80),rep(3,58),rep(4,20),rep(5,18),rep(6,180),rep(7,90))
length(s)




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
## Cubist model with defaults
# does 25 bootstrapped samples
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist")  
summary(cub.model)
cub.model

# predict on calibration data
cub.pred_c<-predict(cub.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = cub.pred_c, plot.it = T)

# predict on calibration data
cub.pred_v<-predict(cub.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = cub.pred_v, plot.it = T)

# predict onto grid
cub.grid<- predict(hunterCovariates_sub, cub.model)
plot(cub.grid)


## Cubist model fit: fiddly around with tuning hyperparameters
#

# Change number of rules. Default is to bootstrap and automatically iterates number of committees and neigbours
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",control = Cubist::cubistControl(rules = 1))  
cub.model
summary(cub.model)

cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",control = Cubist::cubistControl(rules = 10))  
cub.model
summary(cub.model)

# Introduce a grid of the tuning parameters
tgrid <- expand.grid(
  .committees = c(1:5),
  .neighbors = c(0:3)
)

# Use tune grid table but stick with the  default bootstrap sampling
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",tuneGrid = tgrid)  
cub.model
summary(cub.model)

# Use tune grid table but alter the hyperparamters
tgrid <- expand.grid(
  .committees = 1,
  .neighbors = 0)

# leave group out cv, 5 fold and 63% sample size
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",tuneGrid = tgrid, trControl =trainControl(method = "LGOCV", number=5, p=0.63))  
cub.model
summary(cub.model)

# leave group out cv, 5 fold and 63% sample size
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",tuneGrid = tgrid, trControl =trainControl(method = "repeatedcv", number=5, repeats = 10))  
cub.model
summary(cub.model)


# Use tune grid table but alter the hyperparamters
tgrid <- expand.grid(
  .committees = c(1,5,10,20,30,40,50),
  .neighbors = c(0,1,5))

# leave group out cv, 5 fold and 63% sample size
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",tuneGrid = tgrid, trControl =trainControl(method = "LGOCV", number=5, p=0.63))  
cub.model
summary(cub.model)

# leave group out cv, 5 fold and 63% sample size
cub.model<-train(x= DSM_data_c[,5:15], y= DSM_data_c$pH60_100cm,method = "cubist",tuneGrid = tgrid, trControl =trainControl(method = "repeatedcv", number=5, repeats = 10))  
cub.model
summary(cub.model)





### PREDICT

# predict on calibration data
cub.pred_c<-predict(cub.model, DSM_data_c)
goof(observed = DSM_data_c$pH60_100cm, predicted = cub.pred_c, plot.it = T)

# predict on validation data
cub.pred_v<-predict(cub.model, DSM_data_v)
goof(observed = DSM_data_v$pH60_100cm, predicted = cub.pred_v, plot.it = T)

# predict onto grid
ranger.grid<- predict(hunterCovariates_sub, cub.model)
plot(ranger.grid)



