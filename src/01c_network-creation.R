#
# Title: Network creation and standardization
# Created: September 1st, 2021
# Last Updated: March 29th, 2023
# Author: Brandon Allen
# Objectives: Create the network file required for calculating stream connectivity
# Keywords: Notes, Network Creation
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
####################
# Network Creation #
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
        
        # Create the required networks
        # Loop through HFI inventories
        for (hfi.year in hfi.years) {
                
                # Loop through watersheds
                for (HUC in watershed.ids) {
                        
                        gis.summaries <- network_extraction(watershed.geodatabase = paste0(getwd(), "/data/processed/huc-", huc.unit, "/", 
                                                                                           hfi.year, "/gis/", HUC, ".gdb"),
                                                            HUC = HUC,
                                                            dam.layer = paste0(getwd(), "/data/base/gis/dams/Alberta_Dams_3400_2021-05-11.shp"), 
                                                            mineable.boundary = paste0(getwd(), "/data/base/gis/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp"),
                                                            Slope = paste0(getwd(), "/data/base/gis/topographic/Slope_LiDAR.tif"),
                                                            DEM = paste0(getwd(), "/data/base/gis/topographic/GEE_srtm_mosaic/srtm.tif"), 
                                                            MAP = paste0(getwd(), "/data/base/gis/climate/MAP.asc"), 
                                                            Eref = paste0(getwd(), "/data/base/gis/climate/Eref.asc"),
                                                            AT.bridges = paste0(getwd(), "/data/base/gis/bridges/alberta-transportation/Bridges-20m-3400_2019.shp"), 
                                                            NP.rail.bridges = paste0(getwd(), "/data/base/gis/bridges/access-layer/railway-bridges-np-20m_2020.shp"),
                                                            NP.road.bridges = paste0(getwd(), "/data/base/gis/bridges/access-layer/road-bridges-np-20m_2020.shp"),
                                                            arcpy = arcpy)
                        
                        save(gis.summaries, file = paste0("data/processed/huc-", huc.unit, 
                                                          "/", hfi.year, "/connectivity/",
                                                          "network_",  HUC, ".Rdata"))
                        
                        print(HUC)
                        
                }
                
                print(hfi)
                
        }
        
}
