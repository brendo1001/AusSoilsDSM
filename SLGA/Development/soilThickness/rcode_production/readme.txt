# Organisation of R coded workflow for national mapping of soil thickness

## Data Wrangling
* Basic GIS mapping of point data [micelaneous/soilDepth_GIS_file_pointData.R]
* Query the bore log data and assign soil thickness estimates [boreLog_queries.R]


## Model Fitting
* Variogram modelling of soil thickness [residual_variogram_modelling_rangermodels.R] 
* Ranger model fitting of the categorical prediction for distinguishing between deep and not deep soils [rangerModelling_soilDepth_categorical_deep_vs_notdeep.R] 
* Ranger model fitting of the categorical prediction for distinguishing between rock outcrops from not rock outcrops [rangerModelling_soilDepth_categorical_rock_vs_notrock.R] 
* Ranger model fit of soil thickness as a continuous variable [rangerModelling_soilDepth_continuous.R]


## Spatialisation of Models [folder: modelSpatialisation]
* Applying (kriging) globally fitted variogram [residual_modelling_rangermodels_spatialise.R] 
* Applying the categorical model for distinguishing between deep and not deep soils [spatialise_categorical_deep_vs_notdeep_SD_model.R] 
* Applying the categorical model for distinguishing between rock outcrops and not rock outcrops [spatialise_categorical_rock_vs_notrock_outcrop_model.R] 
* Applying the continuous model of soil thickness prediction [spatialise_continuous_SD_model.R]


## Pulling it all together
* Integration of continuous and categorical map outputs to derive products (13 products derived).[spatialise_SD_model__statisticalmoments.R]
* Tile mosaicing of all outputs [folder: tileMosaic contains 13 seperate R scripts for each statistical moment.]




