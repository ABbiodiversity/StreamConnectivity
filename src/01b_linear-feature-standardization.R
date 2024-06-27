#
# Title: Linear feature standardization
# Created: September 1st, 2021
# Last Updated: June 27th, 2024
# Author: Brandon Allen
# Objectives: Standardization of the linear features roads and rails network
# Keywords: Notes, Merging Linear Features, Subsetting Linear Features
#
#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
###########################
# Merging Linear Features #
###########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(foreign)
library(parallel)

# Source data cleaning scripts
source("src/linear-features_functions.R")

# Create the appropriate linear feature subsets using the three HUC scales of interest
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

# Define the cores and objects required for for parallel processing
n.clusters <- length(hfi.series)
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("hfi.series", "linearfeature_merging"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreach)
        library(foreign)
        library(reticulate)
        
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

###########
# Merging #
###########

parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(linearfeature_merging(road.layer = paste0("data/base/gis/roadrail-centerlines/", hfi, 
                                                                                 "/road_centerlines_", hfi, ".shp"),
                                                             rail.layer = paste0("data/base/gis/roadrail-centerlines/", hfi, 
                                                                                 "/rail_centerlines_", hfi, ".shp"), 
                                                             hfi.year = hfi,
                                                             arcpy = arcpy), error = function(e) e)
)

stopCluster(core.input)

##############################
# Subsetting Linear Features #
##############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                            "huc.layer", "linearfeature_subsetting"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        
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

# Loop through each available HFI inventory
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(linearfeature_subsetting(watershed.layer = huc.layer,
                                                                        road.rail.layer = paste0(getwd(), 
                                                                                                 "/data/base/gis/roadrail-centerlines/", 
                                                                                                 hfi, 
                                                                                                 "/roadrail_centerlines_", 
                                                                                                 hfi, ".shp"),
                                                                        stream.layer = paste0(getwd(),
                                                                                              "/data/base/gis/strahler_stream_order/cleaned-network/stream_network_merged.shp"),
                                                                        hfi.year = hfi,
                                                                        huc.scale = huc.scale,
                                                                        huc.unit = huc,
                                                                        arcpy = arcpy), error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()

