# ---
# title: "Calculate stream parameters"
# author: "Brandon Allen"
# created: "2025-01-11"
# inputs: ["network_HUC.Rdata - One for each year and HUC watershed"]
# outputs: ["network_HUC.Rdata - One for each year and HUC watershed"]
# notes: 
#   "Based on the network files that have been created, calculate the confluence bound slope, 
#    local stream slope, and upstream distance. Remove any self intersecting points and repair any point errors."
# ---

# 1.0 Stream repair ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Define the focal watersheds that need to be repaired ----
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years 
watershed.id <- c("080304", "040306", "110403", "230101")

# 1.3 Repair each watershed ----
for (hfi in hfi.series) {
        
        for (watershed in watershed.id) {
                
                # Load watershed
                load(paste0("2_pipeline/huc-6/", hfi, "/connectivity/network_", 
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
                save(watershed.network, file = paste0("2_pipeline/huc-6/", hfi, 
                                                      "/connectivity/network_", 
                                                      watershed, ".Rdata"))
                
        }
        
}

# 2.0 Stream slope ----

# 2.1 Clear memory ----
rm(list=ls())
gc()

# 2.1 Load libraries and source functions ----
library(foreach)
library(foreign)
library(igraph)
library(parallel)
source("1_code/r-scripts/stream-parameters_functions.R")

# 2.2 Define the focal years that HFI are available for processing
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

# 2.3 Define HUC scale and valid watershed ids
huc.scale <- 6
if(huc.scale == 6) {
        
        huc.layer <- paste0("0_data/external/watersheds/boundary/HUC_",
                            8,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
        
} else {
        
        huc.layer <- paste0("0_data/external/watersheds/boundary/HUC_",
                            huc.unit,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
}

watershed.ids <- unique(as.character(watershed.ids[, paste0("HUC_", huc.scale)]))

# 2.4 Define the cores and objects required for for parallel processing ----
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series",
                            "stream_confluence", "stream_slope"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(parallel)
        

})

# 2.5 Loop through each watershed and available centerline inventory ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(stream_confluence(watershed.path = paste0("2_pipeline/huc-6/", hfi, 
                                                                                         "/connectivity/network_", 
                                                                                         huc, ".Rdata")), 
                                               error = function(e) e)
        )

stopCluster(core.input)

# 3.0 Upstream Distance ----

# 3.1 Define the cores and objects required for for parallel processing ----
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

# 3.2 Loop through each watershed and available centerline inventory ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(stream_distance(watershed.path = paste0("2_pipeline/huc-6/", hfi, 
                                                                                         "/connectivity/network_", 
                                                                                         huc, ".Rdata")), 
                                               error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()
