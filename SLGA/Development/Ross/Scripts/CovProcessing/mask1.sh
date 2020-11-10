#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing/mask1.R $SLURM_ARRAY_TASK_ID /datasets/work/af-tern-mal-deb/work/Ross/New/AuscoverResampled /datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Masked