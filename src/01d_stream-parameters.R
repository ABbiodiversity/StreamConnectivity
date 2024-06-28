#
# Title: Calculate local stream parameters. 
# Created: September 1st, 2021
# Last Updated: June 28th, 2024
# Author: Brandon Allen
# Objectives: Based on the network files that have been created, calculate the confluence bound slope, local stream slope, and upstream distance.
# Remove any and remove self intersecting points.
# Keywords: Notes, Stream Repair, Stream Slope, Upstream Distance 
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
#################
# Stream Repair #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# There are four watersheds where the end point of a stream falls outside the watershed boundary. 
# Manually repair this for the following watersheds (080304, 040306, 110403, 230101)

hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years 
watershed.id <- c("080304", "040306", "110403", "230101")

for (hfi in hfi.series) {
        
        for (watershed in watershed.id) {
                
                # Load watershed
                load(paste0("data/processed/huc-6/", hfi, "/connectivity/network_", 
                            watershed, ".Rdata"))
                watershed.edge <- watershed.network$Edge
                
                # Identify the NA segments
                target.fid <- watershed.edge$TARGET_FID[is.na(watershed.edge$WatershedPerm)]
                
                for (target in target.fid) {
                        
                        # For each target, identify the appropriate stream
                        segment <- watershed.edge$UpstreamSeg[watershed.edge$TARGET_FID == target]
                        
                        # For each segment, assign a new value based on other edges in network (first non NA value)
                        nearest.edge <- watershed.edge[watershed.edge$UpstreamSeg == segment |
                                                               watershed.edge$DownstreamSeg == segment, ]
                        nearest.edge <- nearest.edge[!is.na(nearest.edge$WatershedPerm), ]
                        
                        # Replace the appropriate values to the target
                        watershed.edge[watershed.edge$TARGET_FID == target, "WatershedPerm"] <- nearest.edge$WatershedPerm[1]
                        watershed.edge[watershed.edge$TARGET_FID == target, "WatershedArea"] <- nearest.edge$WatershedArea[1]
                        watershed.edge[watershed.edge$TARGET_FID == target, "PourStream"] <- nearest.edge$PourStream[1]
                        watershed.edge[watershed.edge$TARGET_FID == target, "PourElevation"] <- nearest.edge$PourElevation[1]
                        
                }
                
                # Save
                watershed.network$Edge <- watershed.edge
                save(watershed.network, file = paste0("data/processed/huc-6/", hfi, 
                                                      "/connectivity/network_", 
                                                      watershed, ".Rdata"))
                
        }
        
}

################
# Stream Slope #
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(foreign)
library(parallel)

# Source data cleaning scripts
source("src/stream-parameters_functions.R")

# Create the appropriate linear feature subsets using the three HUC scales of interest
hfi.series <- c(2010) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

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
                            "stream_confluence", "stream_slope"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(parallel)
        

})

# Loop through each available HFI inventory
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(stream_confluence(watershed.path = paste0("data/processed/huc-6/", hfi, 
                                                                                         "/connectivity/network_", 
                                                                                         huc, ".Rdata")), 
                                               error = function(e) e)
        )

stopCluster(core.input)

#
# Upstream Distance Test
#

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(foreign)
library(igraph)
library(parallel)

# Source data cleaning scripts
source("src/stream-parameters_functions.R")

# Create the appropriate linear feature subsets using the three HUC scales of interest
hfi.series <- c(2010) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

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
                            "stream_distance", "upstream_distance", "network_visualization"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(igraph)
        library(parallel)
        
        
})

# Loop through each available HFI inventory
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(stream_distance(watershed.path = paste0("data/processed/huc-6/", hfi, 
                                                                                         "/connectivity/network_", 
                                                                                         huc, ".Rdata")), 
                                               error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()
