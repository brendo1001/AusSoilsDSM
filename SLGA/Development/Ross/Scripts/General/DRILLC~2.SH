#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/General/drillCovsHPC.R $SLURM_ARRAY_TASK_ID /datasets/work/af-digiscapesm/work/Ross/SLGAData/TileDrill/ASCs.csv o_longitude_GDA94 o_latitude_GDA94 o_asc_ord SID