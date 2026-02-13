# ---
# title: "Stream Connectivity Status"
# author: "Brandon Allen"
# created: "2025-01-21"
# inputs: ["network_HUC.Rdata - One for each year and HUC watershed"]
# outputs: ["network_HUC.Rdata - One for each year and HUC watershed" (Processed Connectivity tables)]
# notes: 
#   "Using the available culvert surveys, predicted passability, and network information for each watershed,
#    calculate regional stream connectivity."
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries required scripts ----
library(foreach)
library(foreign)
library(igraph)
library(parallel)
source("1_code/r-scripts/connectivity-status_functions.R")

# 1.3 Define scale, region, and years for processing. ----
watershed.ids <- read.dbf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- as.character(unique(watershed.ids$HUC_6)) # Includes all HUC-6 watersheds now
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021, 2022)
huc.scale <- 6

# 1.4 Define spatial autocorrelation variable ----
# Mean distance of first order streams to highest order stream segment
# HUC 6 - 71000, based on results from 02c_autocorrelation-distance.R
Autocorrelation.d0 <- 71000 

# 2.0 Parallel processing of stream connectivity ----

# 2.1 Define the cores and objects required for for parallel processing ----
n.clusters <- 14 # Adjust the number of cores
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series", "Autocorrelation.d0",
                            "network_visualization", "connectivity_wrapper", "watershed_status",
                            "stream_connectivity_matrix"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(igraph)
        
})

# 2.2 Calculate connectivity based on mean culvert predictions ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelMean",
                                                                    path.in = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                                  hfi, "/connectivity/network_", huc, ".Rdata")), 
                                               error = function(e) e)
        )

# 2.3 Calculate connectivity based on upper culvert predictions (upper 90%) ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelUpper",
                                                                    path.in = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                                     hfi, "/connectivity/network_", huc, ".Rdata")), 
                                               error = function(e) e)
        )

# 2.4 Calculate connectivity based on lower culvert predictions (lower 10%) ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(connectivity_wrapper(huc.scale = huc.scale,
                                                                    huc = huc,
                                                                    hfi = hfi,
                                                                    Autocorrelation.d0 = Autocorrelation.d0,
                                                                    culvert.model = "ModelLower",
                                                                    path.in = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                                     hfi, "/connectivity/network_", huc, ".Rdata")), 
                                               error = function(e) e)
        )

# Clear memory
rm(list=ls())
gc()
