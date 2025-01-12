# ---
# title: "Stream standardization"
# author: "Brandon Allen"
# created: "2025-01-10"
# inputs: ["0_data/external/strahler-stream-order/SSO0 through SSO20.shp"]
# outputs: ["0_data/processed/stream-network/stream_network_standardized.shp"]
# notes: 
#   "This script standardized stream network layer provided by EPA. This layer is functionally,
#    equivalent to the stream network available through the Watercourse Crossing Program.
#    This is an earlier version that was broken up into smaller watersheds."
# ---

# 1.0 Clear memory ----
rm(list=ls())
gc()

# 1.1 Load libraries and source functions ----
library(reticulate)
source("1_code/r-scripts/stream-standardization_functions.R")

# 1.2 Initialize reticulate to communicate with ArcPro
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python version
use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# 1.3 Define output file name and workspace ----
file.name <- "0_data/processed/stream-network/stream_network_standardized.shp"
workspace <- "0_data/external/strahler-stream-order/"

# 1.4 List of shapefiles required for creating the provincial inventory ----
stream.layer.list <- list(column_id = list(SSO0 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO1 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO2 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO3 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO4 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO5 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO6 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO7 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO8 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO9 = c("FID", "Shape", "Environm_4", 
                                                    "Shape_STLe", "Environ_16"),
                                           SSO10 = c("FID", "Shape", "Environm_4", 
                                                     "Shape_STLe", "Environ_16"),
                                           SSO11 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO12 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO13 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO14 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO15 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),                                           
                                           SSO11 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO16 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO17 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO18 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO19 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15"),
                                           SSO20 = c("FID", "Shape", "Environm_3", 
                                                     "Shape_STLe", "Environ_15")), 
                          stream_layer = list(SSO0 = "0_data/external/strahler-stream-order/SSO0.shp",
                                              SSO1 = "0_data/external/strahler-stream-order/SSO1.shp",
                                              SSO2 = "0_data/external/strahler-stream-order/SSO2.shp",
                                              SSO3 = "0_data/external/strahler-stream-order/SSO3.shp",
                                              SSO4 = "0_data/external/strahler-stream-order/SSO4.shp",
                                              SSO5 = "0_data/external/strahler-stream-order/SSO5.shp",
                                              SSO6 = "0_data/external/strahler-stream-order/SSO6.shp",
                                              SSO7 = "0_data/external/strahler-stream-order/SSO7.shp",
                                              SSO8 = "0_data/external/strahler-stream-order/SSO8.shp",
                                              SSO9 = "0_data/external/strahler-stream-order/SSO9.shp",
                                              SSO10 = "0_data/external/strahler-stream-order/SSO10.shp",
                                              SSO11 = "0_data/external/strahler-stream-order/SSO11.shp",
                                              SSO12 = "0_data/external/strahler-stream-order/SSO12.shp",
                                              SSO13 = "0_data/external/strahler-stream-order/SSO13.shp",
                                              SSO14 = "0_data/external/strahler-stream-order/SSO14.shp",
                                              SSO15 = "0_data/external/strahler-stream-order/SSO15.shp",
                                              SSO16 = "0_data/external/strahler-stream-order/SSO16.shp",
                                              SSO17 = "0_data/external/strahler-stream-order/SSO17.shp",
                                              SSO18 = "0_data/external/strahler-stream-order/SSO18.shp",
                                              SSO19 = "0_data/external/strahler-stream-order/SSO19.shp",
                                              SSO20 = "0_data/external/strahler-stream-order/SSO20.shp"))

# 2.0 Stream standardization ----
# Run the stream standardization if the stream layer has not already been created
if(!file.exists(file.name)) {
        
        stream_standardization(stream.list = stream.layer.list, 
                               workspace = workspace,
                               file.name = file.name,
                               arcpy = arcpy)
        
}

# Clear memory
rm(list=ls())
gc()