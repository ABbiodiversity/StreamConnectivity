#
# Title: Network creation and standardization
# Created: September 1st, 2021
# Last Updated: June 27th, 2024
# Author: Brandon Allen
# Objectives: Create the network file required for calculating stream connectivity
# Keywords: Notes, Network Creation
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
####################
# Network Creation #
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(foreign)
library(parallel)

# Source data cleaning scripts
source("src/network-creation_functions.R")

# Create the appropriate linear feature subsets using the three HUC scales of interest
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

# It is more efficient to implement parallel processes
# across watersheds than it is years (more watersheds than years)

# Based on the analysis HUC scale, identify the watershed codes
# Identify the watersheds within the focal HUC scale
huc.scale <- 6
if(huc.scale == 6) {
        
        huc.layer <- paste0("data/base/gis/watersheds/boundary/HUC_",
                            8,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
        
} else {
        
        huc.layer <- paste0("data/base/gis/watersheds/boundary/HUC_",
                            huc.unit,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
}

watershed.ids <- unique(as.character(watershed.ids[, paste0("HUC_", huc.scale)]))

# Define the cores and objects required for for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series",
                            "huc.layer", "network_extraction"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        library(sf)
        
        # Initialize arcpy
        py_discover_config() # We need version 3.9
        py_config() # Double check it is version 3.9
        
        # Set python 
        use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

start.time <- Sys.time()
# Loop through each available HFI inventory
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(network_extraction(watershed.geodatabase = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                                                                 hfi, "/gis/", huc, ".gdb"),
                                                                  huc.scale = huc.scale,
                                                                  huc.unit = huc,
                                                                  hfi.year = hfi,
                                                                  dam.layer = paste0(getwd(), "/data/base/gis/dams/Alberta_Dams_3400_2021-05-11.shp"), 
                                                                  mineable.boundary = paste0(getwd(), "/data/base/gis/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp"),
                                                                  Slope = paste0(getwd(), "/data/base/gis/topographic/Slope_LiDAR.tif"),
                                                                  DEM = paste0(getwd(), "/data/base/gis/topographic/ALOS/DEM.tif"), 
                                                                  MAP = paste0(getwd(), "/data/base/gis/climate/MAP.tif"), 
                                                                  Eref = paste0(getwd(), "/data/base/gis/climate/Eref.tif"),
                                                                  AT.bridges = paste0(getwd(), "/data/base/gis/bridges/alberta-transportation/Bridges-20m-3400_2019.shp"), 
                                                                  NP.rail.bridges = paste0(getwd(), "/data/base/gis/bridges/access-layer/railway-bridges-np-20m_2020.shp"),
                                                                  NP.road.bridges = paste0(getwd(), "/data/base/gis/bridges/access-layer/road-bridges-np-20m_2020.shp"),
                                                                  arcpy = arcpy), error = function(e) e)
        )

stopCluster(core.input)

end.time <- Sys.time()
end.time - start.time

# Clear memory
rm(list=ls())
gc()

