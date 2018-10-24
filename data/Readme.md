Common datasets to use
----------------------

The R language is a very rich resource when it comes to doing
statistical analyses. This suits doing digital soil mapping enormously
as the process of creating soil maps largely hinges upon developing some
sort of geospatial model that relates target variables to environmental
data. The *scorpan* model, first described in McBratney, Mendonca
Santos, and Minasny (2003) sets out the basis for doing digital soil
mapping whereby soil spatial variation is modeled as a function of
environmental information including: other soil information, climate,
organisms, relief, parent material, age, and spatial position. Soil
scientists will pick up on the fact that nearly all these variables
coincide with the factors of soil formation that were identified and
described in Jenny (1941). This is true, but the *scorpan* model can be
thought of as the empirical model for what Jenny (1941) conceptually
defined. With the *scorpan* model we can retrieved available spatial
data that correspond to or can be used as proxies for those above
described factors of soil formation. With such data and coupled with
soil observations, we can begin to develop some empirical models about
what the likely variation of soil is across a given spatial domain.

R has a very rich ecosystem of potential models that could be exploited
in the process of doing digital soil mapping (fitting the models bit).
In the `__/rcode/dsm_models__` folder of this repository there are a
number of examples of different models used for the spatial prediction
of soil properties. To make things simple, a common data set is used for
the contexts of predicting continuous soils variable and categorical
soil variables. These data are available in the `ithir` R package.

The R script for downloading, installing, and loading `ithir` is as
follows:

    ## install.packages("devtools")
    ## library(devtools)
    ## install_bitbucket("brendo1001/ithir/pkg") # download and install
    library(ithir) # load

### Continuous variables

For continuous variables the data to use is `data(HV_subsoilpH)`, which
is a collection of 506 soil pH observations from the Hunter Valley
region of NSW. The associated environmental covariates are
`data(hunterCovariates_sub)` which is a stack of raster data coincidong
mainly with information derived from an avaailble 25m digital enlevation
model that has continous coverage in the spatial domain where the soil
observations were collected from.

    # point data
    data(HV_subsoilpH)
    str(HV_subsoilpH)

    ## 'data.frame':    506 obs. of  14 variables:
    ##  $ X                       : num  340386 340345 340559 340483 340734 ...
    ##  $ Y                       : num  6368690 6368491 6369168 6368740 6368964 ...
    ##  $ pH60_100cm              : num  4.47 5.42 6.26 8.03 8.86 ...
    ##  $ Terrain_Ruggedness_Index: num  1.34 1.42 1.64 1.04 1.27 ...
    ##  $ AACN                    : num  1.619 0.281 2.301 1.74 3.114 ...
    ##  $ Landsat_Band1           : int  57 47 59 52 62 53 47 52 53 63 ...
    ##  $ Elevation               : num  103.1 103.7 99.9 101.9 99.8 ...
    ##  $ Hillshading             : num  1.849 1.428 0.934 1.517 1.652 ...
    ##  $ Light_insolation        : num  1689 1701 1722 1688 1735 ...
    ##  $ Mid_Slope_Positon       : num  0.876 0.914 0.844 0.848 0.833 ...
    ##  $ MRVBF                   : num  3.85 3.31 3.66 3.92 3.89 ...
    ##  $ NDVI                    : num  -0.143 -0.386 -0.197 -0.14 -0.15 ...
    ##  $ TWI                     : num  17.5 18.2 18.8 18 17.8 ...
    ##  $ Slope                   : num  1.79 1.42 1.01 1.49 1.83 ...

    #covariate data
    data(hunterCovariates_sub)
    hunterCovariates_sub

    ## class       : RasterStack 
    ## dimensions  : 249, 210, 52290, 11  (nrow, ncol, ncell, nlayers)
    ## resolution  : 25, 25  (x, y)
    ## extent      : 338422.3, 343672.3, 6364203, 6370428  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=utm +zone=56 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs 
    ## names       : Terrain_Ruggedness_Index,        AACN, Landsat_Band1,   Elevation, Hillshading, Light_insolation, Mid_Slope_Positon,       MRVBF,        NDVI,         TWI,       Slope 
    ## min values  :                 0.194371,    0.000000,     26.000000,   72.217499,    0.000677,      1236.662840,          0.000009,    0.000002,   -0.573034,    8.224325,    0.001708 
    ## max values  :                15.945321,  106.665482,    140.000000,  212.632507,   32.440960,      1934.199950,          0.956529,    4.581594,    0.466667,   20.393652,   21.809752

To get a sense of where these data come from, they can be visualised
below in the interactive map made possible through the use of both
`mapview` and `sf` R packages.

    library(mapview);library(sf)

    ## Linking to GEOS 3.6.2, GDAL 2.2.3, proj.4 4.9.3

    # coerce to a spatial object
    HV_subsoilpH <- st_as_sf(
      HV_subsoilpH, 
      coords = c('X', 'Y'),
      crs = "+init=epsg:32756")

    #select the raster to display
    r1<- hunterCovariates_sub[[1]]
    r1

    ## class       : RasterLayer 
    ## dimensions  : 249, 210, 52290  (nrow, ncol, ncell)
    ## resolution  : 25, 25  (x, y)
    ## extent      : 338422.3, 343672.3, 6364203, 6370428  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=utm +zone=56 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs 
    ## data source : in memory
    ## names       : Terrain_Ruggedness_Index 
    ## values      : 0.194371, 15.94532  (min, max)

    # create the map with point data and a raster layer
    # overlaying on 
    mapview(r1,map.types="Esri.WorldImagery", legend=FALSE, use.layer.names=T) + HV_subsoilpH


<img src="https://github.com/brendo1001/AusSoilsDSM/tree/master/data/Readme_files/figure-markdown_strict/map-1.png" width="20cm" height="15cm" style="display: block; margin: auto;" />

### Categorical variables

References
----------

Jenny, H. 1941. *Factors of Soil Formation*. New York: McGraw-Hill.

McBratney, A B, M L Mendonca Santos, and B Minasny. 2003. “On Digital
Soil Mapping.” *Geoderma* 117: 3–52.
