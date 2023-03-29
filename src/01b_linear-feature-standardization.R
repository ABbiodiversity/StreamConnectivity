#
# Title: Linear feature standardization procedure
# Created: September 1st, 2021
# Last Updated: March 29th, 2023
# Author: Brandon Allen
# Objectives: Standardization of the linear features roads and rails network
# Keywords: Notes, Linear Features
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
###################
# Linear Features #
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(reticulate)

# Source data cleaning scripts
source("src/data-cleaning_functions.R")

# Initialize reticulate to communicate with ArcPro
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python version
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# Create the appropriate linear feature subsets using the three HUC scales of interest
huc.scales <- c(6, 8, 10) # Define HUC scales
hfi.years <- c(2010, 2014, 2016, 2018) # Define HFI years

for (huc.unit in huc.scales) {
        
        # Identify the watersheds within the focal HUC scale
        if(huc.unit == 6) {
                
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

        watershed.ids <- unique(as.character(watershed.ids[, paste0("HUC_", huc.unit)]))
        
        # Loop through each footprint inventory that is available
        for (hfi.year in hfi.years) {

                # Create the linear features for the stream and footprint layers
                linearfeature_subsetting(watershed.layer = huc.layer,
                                         road.layer = paste0("data/base/gis/roadrail-centerlines/", hfi.year, 
                                                             "/road_centerlines_", hfi.year, ".shp"),
                                         rail.layer = paste0("data/base/gis/roadrail-centerlines/", hfi.year, 
                                                             "/rail_centerlines_", hfi.year, ".shp"),
                                         stream.layer = "data/base/gis/strahler_stream_order/cleaned-network/stream_network_merged.shp",
                                         hfi.year = hfi.year,
                                         huc.scale = huc.unit,
                                         watershed.lookup = watershed.ids,
                                         arcpy = arcpy)
                
                print(paste("HUC", huc.unit, hfi.year))
                
        }
        
}


