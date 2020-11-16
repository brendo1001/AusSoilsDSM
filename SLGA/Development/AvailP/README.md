# Available Phosphorus
Methodology used to produce a map of inherent soil Colwell P for Australia which approximates available P

## Summary
The map was made using soil point data and environmental covariates developed by TERN. The soil 
data comes from state and territory agencies and was collated using the TERN Soil Data Federator 
(See TERNDataPrep.R). Soil data was filtered using *disturbance of site* coding if available or *land 
use* data (ACLUMP). A *random forests* (RF) model was fitted using the *Ranger* implementation 
of RF. Prediction uncertainty was determined using a bootstrapping method implemented by 
Malone (2017). 50 simulations were run to determine the 90% prediction interval. The final map was 
predicted using the *predict* function in the *Raster* package. Predictions were made using the 
TERN covariate tiles in batch mode on the DES HPC.

## Workflow
1. Extract all avialable Colwell P data from state agency soil databases.
2. Apply a land use filter to the extracted data to capature just soil data from non-cropping lands.
3. Add in other data not in state agency data bases.
4. Prepare data for spline.
5. Standardise soil data to global soil map standard depths using a spline.
6. Format standardised data for modell fitting by random forest.
7. Select TERN covariates.
8. Fit a random forests model using the Ranger package.
9. Anaylse model fit, repeat above steps if necessary.
10. Map out fitted model using raster interpolation function *predict* using TERN standard tiles
11. Mosaic tile predictions and back transform map

**Installation:** To download all scripts and files in this repository, click green Clone or Download button, select Download ZIP, save zip file to C://Temp, extract files. Always source files from this respository so that you have the latest version. You will need SQL Developer (linked to SALI), RStudio, R and Pyhon installed.

**Usage:** This repository is provided for reviewers of the TERN Available P map to assist with understanding how the product was made.

**Contributing:** If you can see improvements that can be made, please do a pull request or raise an issue.

**Credits:** The Soil Data Federator API used in the first script was created by Ross Searle, TERN Landscapes & CSIRO. The second script contains ideas and code present in Malone BP, Minasny B, McBratney AB (2017) ‘Using R for Digital Soil Mapping.’ (Spinger International Publishing: Switzerland) as well as orginal script by Peter Zund, who is also the author of this repository.

**License:** Free to use with acknowledgement.

## Further detail 

