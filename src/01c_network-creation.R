#
# Title: Network creation and standardization
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
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
                          workspace = "data/base/gis/scratch/", 
                          x64 = TRUE, 
                          overwrite = TRUE,
                          extensions = c("Spatial", "GeoStats"))

for (huc.unit in huc.scales) {
        
        # Depending on the HUC unit, identify focal watersheds
        
        if(huc.unit == 6) {
                
                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_6))
                
        }
        
        if(huc.unit == 8) {

                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_8))
                
        }
        
        if(huc.unit == 10) {
                
                watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_10_EPSG3400.dbf")
                watershed.ids <- unique(as.character(watershed.ids$HUC_10))
                
        }
        
        # Network creation
        
        for (hfi in hfi.years) {
                
                # Create folder structure
                
                if(!file.exists(paste0("data/processed/huc-", huc.unit, "/", hfi, "/gis/", watershed.ids[1]))) {
                        
                        for(HUC in watershed.ids) {
                                
                                dir.create(paste0("data/processed/huc-", huc.unit, "/", hfi, "/gis/", HUC))
                                dir.create(paste0("data/processed/huc-", huc.unit, "/", hfi, "/gis/", HUC, "/temporary-databases"))
                                dir.create(paste0("data/processed/huc-", huc.unit, "/", hfi, "/gis/", HUC, "/temporary-shapefiles"))
                                
                        }
                        
                }
                
                for (HUC in watershed.ids) {
                        
                        gis.summaries <- network_extraction(stream.layer = paste0("data/base/gis/strahler_stream_order/huc-", huc.unit, "/", HUC, "_stream.shp"),
                                                                road.layer = paste0("data/base/gis/roadrail-centerlines/", hfi, "/huc-", huc.unit, "/", HUC, "_roadrail.shp"),
                                                                dam.layer = "C:/Users/beallen/Desktop/Lotic-Connectivity/data/base/gis/dams/Alberta_Dams_3400_2021-05-11.shp", 
                                                                HUC = HUC,
                                                                mineable.boundary = "data/base/gis/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp",
                                                                relative.path = paste0("data/processed/huc-", huc.unit, "/", hfi, "/gis/", HUC, "/"))
                        
                        if (length(gis.summaries) == 1) {
                                
                                write.csv(gis.summaries[1], file = paste0("data/processed/huc-", huc.unit, "/", hfi, "/connectivity/", HUC, "-node-connectivity.csv"), row.names = FALSE)
                                
                        } else {
                                
                                write.csv(gis.summaries[1], file = paste0("data/processed/huc-", huc.unit, "/", hfi, "/connectivity/", HUC, "-node-connectivity.csv"), row.names = FALSE)
                                write.csv(gis.summaries[2], file = paste0("data/processed/huc-", huc.unit, "/", hfi, "/connectivity/", HUC, "-edge-connectivity.csv"), row.names = FALSE)
                                write.csv(gis.summaries[3], file = paste0("data/processed/huc-", huc.unit, "/", hfi, "/reach/", HUC, "-node-reach.csv"), row.names = FALSE)
                                write.csv(gis.summaries[4], file = paste0("data/processed/huc-", huc.unit, "/", hfi, "/reach/", HUC, "-edge-reach.csv"), row.names = FALSE)
                                
                        }
                        
                        print(HUC)
                        
                }
                
                print(hfi)
                
        }
        

        
}
