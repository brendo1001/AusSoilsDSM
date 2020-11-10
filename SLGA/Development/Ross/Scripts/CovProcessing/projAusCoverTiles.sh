#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing/projAusCoverTiles.R $SLURM_ARRAY_TASK_ID alpsbk_aust_y2009_sd5a2