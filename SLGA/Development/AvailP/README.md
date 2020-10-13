# Available Phosphorus
These methods were used to produce a Colwell P map for Australia
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
Coming
**Usage:**
**Credits:**
## Further detail 
Coming
