#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing/paraStats.R $SLURM_ARRAY_TASK_ID ETa_MOD_DAY_4dim_3ord_Spatial_Temporal /datasets/work/af-tern-mal-deb/work/Ross/Heat