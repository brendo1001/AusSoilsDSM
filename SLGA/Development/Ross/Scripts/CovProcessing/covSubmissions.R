jobName='mosRFMaps90mTiles'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=1, debugPath=debugPath,arguments='', deleteDebugFiles=T)


jobName='mosLinMaps90mTiles'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='06:00:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=12, debugPath=debugPath, arguments='', deleteDebugFiles=T)

workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/General'
jobName='drillCovsHPC'
args <- paste0('/datasets/work/af-digiscapesm/work/Ross/SLGAData/TileDrill/ASCs.csv o_longitude_GDA94 o_latitude_GDA94 o_asc_ord SID')
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:10:00', memoryGB='8GB', jobStartIteration=1580, jobEndIteration=1580, debugPath=debugPath, arguments=args, deleteDebugFiles=T)

workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing'
jobName='TileProcessing'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:10:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=4009, debugPath=debugPath, deleteDebugFiles=T)

workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing'
jobName='make30mTiles'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:30:00', memoryGB='8GB', jobStartIteration=1, jobEndIteration=4009, limit='', debugPath=debugPath, deleteDebugFiles=T)


jobName='wofsProcessing'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:50:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1246, limit='', debugPath=debugPath, deleteDebugFiles=T)


jobName='testDown'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:05:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=1, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='FCDownload'
args <- ''
# end =927
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='00:30:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=927, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='FCProcessing'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=927, limit='%200', debugPath=debugPath, deleteDebugFiles=T)


jobName='projFC'
args <- ''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:00:00', memoryGB='10GB', jobStartIteration=298, jobEndIteration=298, limit='%100', debugPath=debugPath, deleteDebugFiles=T)


jobName='mosWofs'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='mosFC2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='05:00:00', memoryGB='10GB', jobStartIteration=9, jobEndIteration=12, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='fasterize'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='04:30:00', memoryGB='280GB', jobStartIteration=1, jobEndIteration=1, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='rasterize'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='05:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='test'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=1, limit='', debugPath=debugPath, deleteDebugFiles=T)

jobName='paraStats'
#args <- 'B_MOD_DAY_4dim_3ord_Spatial_Temporal /datasets/work/af-tern-mal-deb/work/Ross/Heat'
args <- 'ETa_MOD_DAY_4dim_3ord_Spatial_Temporal /datasets/work/af-tern-mal-deb/work/Ross/Heat'
#args <- 'A_MOD_DAY_4dim_3ord_Spatial_Temporal /datasets/work/af-tern-mal-deb/work/Ross/Heat'
#args <- 'H_MOD_DAY_4dim_3ord_Spatial_Temporal /datasets/work/af-tern-mal-deb/work/Ross/Heat'
#539
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:00:00', memoryGB='2GB', jobStartIteration=1, jobEndIteration=539, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


jobName='paraStatsSMOS'
args <- 'AU_daily /datasets/work/af-tern-mal-deb/work/Ross'
#1158
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='03:00:00', memoryGB='5GB', jobStartIteration=1, jobEndIteration=1158, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='FillHolesFC'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='10:00:00', memoryGB='1GB', jobStartIteration=1, jobEndIteration=4, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='serialProcessing'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=4, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='serialProcessing2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=16, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='serialProcessing3'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=13, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='serialProcessing4'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=9, jobEndIteration=12, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

jobName='serialProcessing5'
args<-''
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=5, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
args<-''


workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/CovProcessing'
jobName='aus1TileResamp'
args<-''
#1519
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='5:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=1519, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=F)


jobName='aus2TileResamp'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='5:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=1519, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=F)


jobName='FCMask'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='5:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=12, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)

jobName='mask2'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=17, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)

jobName='mosGA'
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='50:00:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=93, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)

jobName='makeTilesFromMosaics'
# args = "Relief_1sec_aspect_1s.tif"           
# args = "Relief_1sec_dem1sv1_0.tif"            
# args = "Relief_1sec_focalrange1000m_1s.tif"  
# args = "Relief_1sec_focalrange300m_1s.tif"    
# args = "Relief_1sec_mrrtf6g-a5_1s.tif"        
# args = "Relief_1sec_mrvbf_int.tif"           
# args = "Relief_1sec_plan_curvature_1s.tif"    
# args = "Relief_1sec_profile_curvature_1s.tif" 
# args = "Relief_1sec_slope_relief.tif"      
# args = "Relief_1sec_slopedeg_1s.tif"          
# args = "Relief_1sec_slopepct1s.tif"           
# args = "Relief_1sec_twi_1s.tif"

# args ="Clim_tdayann.tif"
# args ="Relief_GEOSS_Aus_Landform_Land_surface_forms.tif"
# args ="Relief_Aus_Landform_Topographic_moisture_potential.tif"
# args ="PM_GEOSS_Aus_Lithology_Lithology.tif"
# args ="PM_GEOSS_Aus_Lithology_Weathering_intensity.tif"
# args ="Clim_GEOSS_Aus_Macroclimate_Bioclimatic_zones.tif"
# args ="Veg_GEOSS_Aus_Vegetation_structural_formations.tif"

# args = "Clim_PrescottIndex_01_1s_lzw.tif"
# args = "Relief_tpi_class_1s.tif"
# args = "Relief_tpi_mask_1s.tif"
# args = "Veg_trend_evi_mean.tif"


#args ="Veg_FC_Max_BS.tif"
#args ="Veg_FC_Max_NPV.tif"
#args ="Veg_FC_Max_PV.tif"
#args ="Veg_FC_Mean_BS.tif"
#args ="Veg_FC_Mean_NPV.tif"
#args ="Veg_FC_Min_BS.tif"
#args ="Veg_FC_Min_NPV.tif"
#args ="Veg_FC_Min_PV.tif"
#args ="Veg_FC_Mean_PV.tif"
#args ="Veg_FC_SD_BS.tif"
#args ="Veg_FC_SD_NPV"
#args ="Veg_FC_SD_PV.tif"



#args ="Other_A_MOD_DAY_4dim_3ord_Spatial_Temporal_max.tif"    
#args ="Other_A_MOD_DAY_4dim_3ord_Spatial_Temporal_mean.tif"  
#args ="Other_A_MOD_DAY_4dim_3ord_Spatial_Temporal_med.tif"    
#args ="Other_A_MOD_DAY_4dim_3ord_Spatial_Temporal_min.tif"   
#args ="Other_A_MOD_DAY_4dim_3ord_Spatial_Temporal_sd.tif"     
#args ="Other_Bowen_Ratio_mean.tif"                           
#args ="Other_ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_max.tif"  
#args ="Other_ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_mean.tif"
#args ="Other_ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_med.tif"  
#args ="Other_ETa_MOD_DAY_4dim_3ord_Spatial_Temporal_sd.tif"  
#args ="Other_H_MOD_DAY_4dim_3ord_Spatial_Temporal_max.tif"    
#args ="Other_H_MOD_DAY_4dim_3ord_Spatial_Temporal_mean.tif"  
args ="Other_H_MOD_DAY_4dim_3ord_Spatial_Temporal_med.tif"   #submit this one next 
#args ="Other_H_MOD_DAY_4dim_3ord_Spatial_Temporal_min.tif"   
#args ="Other_H_MOD_DAY_4dim_3ord_Spatial_Temporal_sd.tif"   

# 4009
jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='01:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=401, limit='%200', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

