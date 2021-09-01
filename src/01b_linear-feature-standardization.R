#
# Title: Linear feature standardization procedure
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
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
library(raster)
library(rgdal)
library(RPyGeo)

# Source data cleaning scripts
source("src/data-cleaning-functions.R")

# Create the appropriate linear feature subsets using the three HUC scales of interest
huc.scales <- c(6, 8, 10) # Define HUC scales
hfi.years <- c(2010, 2014, 2016, 2018) # Define HFI years

# Initialize the rpygeo environment outside of the function to prevent collapsing when looped
arcpy <- rpygeo_build_env(path = "C:/Python27/ArcGISx6410.4/",
                          workspace = "data/base/gis/", 
                          x64 = TRUE, 
                          overwrite = TRUE,
                          extensions = c("Spatial", "GeoStats"))

for (huc.unit in huc.scales) {
        
        # Depending on the HUC unit, identify focal watersheds
        
        if(huc.unit == 6) {
                
                huc.layer <- "data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp"
                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_6))
          
        }
        
        if(huc.unit == 8) {
                
                huc.layer <- "data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp"
                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_8))
                
        }
        
        if(huc.unit == 10) {
                
                huc.layer <- "data/base/gis/watersheds/boundary/HUC_10_EPSG3400.shp"
                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_10_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_10))
                
        }
        
        for (hfi in hfi.years) {
                
                # Run subsetting if files don't exist
                if(!file.exists(paste0("data/base/gis/roadrail-centerlines/", hfi, "/huc-", huc.unit, "/", watershed.ids[1], "_roadrail.shp"))) {
                        
                        linearfeature_subsetting(watershed.layer = huc.layer,
                                                 road.layer = paste0("roadrail-centerlines/", hfi, "/road_centerlines_", hfi, ".shp"),
                                                 rail.layer = paste0("roadrail-centerlines/", hfi, "/rail_centerlines_", hfi, ".shp"),
                                                 stream.layer = "data/base/gis/strahler_stream_order/cleaned-network/stream_network_merged.shp",
                                                 workspace = "data/base/gis/",
                                                 hf.year = hfi,
                                                 huc.scale = huc.unit,
                                                 watershed.lookup = watershed.ids)
                }
                
        }
        
}


