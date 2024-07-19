#
# Title: Calculating stream connectivity status
# Created: September 1st, 2021
# Last Updated: July 19th, 2024
# Author: Brandon Allen
# Objectives: Calculating lotic connectivity for individual streams and watersheds
# Keywords: Notes, Environment initialization, Stream connectivity, Watershed connectivity
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# 1) All paths defined in this script are local
#
##############################
# Environment initialization # Last piece is to add the bootstrap predictions (mean plus 90% predictions)
##############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries required scripts
library(foreach)
library(foreign)
library(igraph)
library(parallel)

source("src/connectivity-status_functions.R")

# Define the watershed lookup table
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- as.character(unique(watershed.ids$HUC_6)) # Includes all HUC-6 watersheds now

# Define analysis years and watershed scale
hfi.series <- c(2021)
huc.scale <- 6
n.clusters <- 14 # Adjust the number of cores

# Define spatial autocorrelation variable
# Mean distance of first order streams to highest order stream segment
# HUC 6 - 71000, based on results from 02c_autocorrelation-distance.R
Autocorrelation.d0 <- 71000 

# Define the cores and objects required for for parallel processing
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series", "Autocorrelation.d0",
                            "network_visualization", "connectivity_wrapper", "watershed_status",
                            "stream_connectivity_matrix"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(igraph)
        
})

start.time <- Sys.time()
# Loop through each available HFI inventory
# Model mean
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelMean"), 
                                               error = function(e) e)
        )

# Loop through each available HFI inventory
# Model Upper
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelUpper"), 
                                               error = function(e) e)
        )

# Loop through each available HFI inventory
# Model Lower
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelLower"), 
                                               error = function(e) e)
        )

stopCluster(core.input)
end.time <- Sys.time()
end.time - start.time

# Clear memory
rm(list=ls())
gc()
