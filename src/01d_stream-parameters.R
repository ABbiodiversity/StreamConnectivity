#
# Title: Calculating confluence bound slope, local stream slope, and upstream distance
# Created: September 1st, 2021
# Last Updated: March 31st, 2023
# Author: Brandon Allen
# Objectives: Based on the network files that have been created, calculate the stream slopes and remove self intersecting points
# Keywords: Notes, Stream Slope, Upstream Distance 
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# 1) All paths defined in this script are local
#
################
# Stream Slope #
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Source data cleaning scripts
source("src/data-cleaning_functions.R")

# Identify the set of watersheds which need to be formatted. Any culvert without edge information is assumed 100% connected
watershed.path <- list.files(path = "data/processed/huc-6/2010/connectivity/", full.names = TRUE)
watershed.name <- list.files(path = "data/processed/huc-6/2010/connectivity/", full.names = FALSE)

for (watershed in watershed.name) {
        
        #####################
        # Stream Confluence #
        #####################
        
        # load the stream network data
        load(watershed.path[grep(watershed, watershed.path)])
        
        # Load appropriate node and edge data
        confluence.edge <- watershed.network[["Edge"]]
        confluence.node <- watershed.network[["Node"]]
        
        # If Upstream or Downstream segments == 0, make Upstream == Downstream
        confluence.edge$UpstreamSeg[confluence.edge$UpstreamSeg == 0] <- confluence.edge$DownstreamSeg[confluence.edge$UpstreamSeg == 0]
        confluence.edge$DownstreamSeg[confluence.edge$DownstreamSeg == 0] <- confluence.edge$UpstreamSeg[confluence.edge$DownstreamSeg == 0]
        
        # Update the node information in the edges
        confluence.edge$Node <- paste(confluence.edge$UpstreamSeg, confluence.edge$DownstreamSeg, sep = "-")
        
        # Create new column for storing the confluence information
        confluence.edge$Confluence <- NA
        
        # Create the list of culverts to get estimates for
        culvert.list <- confluence.edge[confluence.edge$Class == "Culvert", ]
        
        # If the culvert is identified at a end/start node, remove it
        culvert.list <- culvert.list[culvert.list$UpstreamSeg != culvert.list$DownstreamSeg, ]
        
        # If the stream types are identified as a lake, define as a new habitat type (-1)
        confluence.node$HabitatType[confluence.node$StreamType == "LAKE PATH"]  <- -1
        
        # Loop through each culvert and calculate the gradient of the stream confluence
        
        for (culvert.id in culvert.list$TARGET_FID) {
                
                confluence.edge$Confluence[confluence.edge$TARGET_FID == culvert.id] <- stream_slope(culvert.id = culvert.id, 
                                                                                                     culvert.list = culvert.list,
                                                                                                     node.data = confluence.node, 
                                                                                                     edge.data = confluence.edge, 
                                                                                                     slope.type = "Confluence")
                
        }
        
        print(paste0("Confluence estimate complete: ", watershed), Sys.time())
        
        rm(culvert.list, culvert.id)
        
        # If there are NA values for any of the missing variables, update them to represent the mean value of culverts
        for(col.id in 14:ncol(confluence.edge)) {
                
                confluence.edge[, col.id][confluence.edge$Class == "Culvert" & is.na(confluence.edge[, col.id])] <- mean(confluence.edge[, col.id], na.rm = TRUE)
                
        }
        
        # Add strahler order
        confluence.edge$Strahler <- NA
        
        for(edge.id in confluence.edge$TARGET_FID) {
                
                confluence.edge$Strahler[confluence.edge$TARGET_FID == edge.id] <- max(c(confluence.node[confluence.node$Stream %in% confluence.edge[confluence.edge$TARGET_FID == edge.id, "UpstreamSeg"], "HabitatType"],
                                                                                         confluence.node[confluence.node$Stream %in% confluence.edge[confluence.edge$TARGET_FID == edge.id, "DownstreamSeg"], "HabitatType"]))
        }
        
        print(paste0("Strahler assignment complete: ", watershed), Sys.time())
        
        # Identify stream segments associated with each HUC 8. There may be instances of segments running into both, but 
        # this sound be a small total length.
        
        confluence.edge$TotalLength <- NA
        
        for(basin in unique(confluence.edge$WatershedPerm)) {
                
                temp.data <- unique(c(confluence.edge[confluence.edge$WatershedPerm == basin, c("UpstreamSeg")], 
                               confluence.edge[confluence.edge$WatershedPerm == basin, c("DownstreamSeg")]))
                
                confluence.edge$TotalLength[confluence.edge$WatershedPerm == basin] <- sum(confluence.node[confluence.node$Stream %in% temp.data, "SectionLength"])
                
        }
        
        # Correct the pour elevation as -9999 values exist
        
        for(watershed.id in unique(confluence.edge$WatershedArea)) {
                
                confluence.edge$Elevation[confluence.edge$WatershedArea == watershed.id & is.na(confluence.edge$Elevation)] <- mean(confluence.edge$Elevation[confluence.edge$WatershedArea == watershed.id & !is.na(confluence.edge$Elevation)], na.rm = TRUE)
                confluence.edge$PourElevation[confluence.edge$WatershedArea == watershed.id & confluence.edge$PourElevation == -9999] <- min(confluence.edge$Elevation[confluence.edge$WatershedArea == watershed.id & confluence.edge$Elevation != -9999], na.rm = TRUE)
                
        }
        
        # Add properties for watershed characteristics
        min.values <- aggregate(confluence.edge[, c("Elevation", "PourElevation")], by = list(WatershedPerm = confluence.edge$WatershedPerm), FUN = function(x) min(x, na.rm = TRUE))
        max.values <- aggregate(confluence.edge[, c("Elevation", "PourElevation")], by = list(WatershedPerm = confluence.edge$WatershedPerm), FUN = function(x) max(x, na.rm = TRUE))
        
        confluence.edge$WatershedElevationMin <- min.values$PourElevation[match(confluence.edge$WatershedPerm, min.values$WatershedPerm)]
        confluence.edge$WatershedElevationMax <- max.values$Elevation[match(confluence.edge$WatershedPerm, max.values$WatershedPerm)]
        
        rm(min.values, max.values)
        
        confluence.edge$BasinRelief <- confluence.edge$WatershedElevationMax - confluence.edge$WatershedElevationMin
        confluence.edge$Compactness <- confluence.edge$WatershedPerm / (2 * sqrt(pi * confluence.edge$WatershedArea))
        confluence.edge$DrainageDensity <- confluence.edge$TotalLength / confluence.edge$WatershedArea
        
        
        print(paste0("Watershed characteristics complete: ", watershed), Sys.time())
        
        # Update R object and save
        watershed.network[["Edge_Cleaned"]] <- confluence.edge
        watershed.network[["Node_Cleaned"]] <- confluence.node
        save(watershed.network, file = watershed.path[grep(watershed, watershed.path)])

}

#####################
# Upstream Distance #
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load library
library(foreign)
library(igraph)

# Source data cleaning scripts
source("src/data-cleaning_functions.R")
source("src/connectivity-status_functions.R")

# Identify the set of watersheds to calculate total upstream distance
watershed.path <- list.files(path = "data/processed/huc-6/2010/connectivity/", full.names = TRUE)
watershed.name <- list.files(path = "data/processed/huc-6/2010/connectivity/", full.names = FALSE)

for (watershed in edge.nwatershed.nameames) {
        
        #####################
        # Upstream Distance #
        #####################
        
        # load the stream network data
        load(watershed.path[grep(watershed, watershed.path)])
        
        # Load appropriate node and edge data
        edge.in <- watershed.network[["Edge_Cleaned"]]
        node.in <- watershed.network[["Node_Cleaned"]]
        
        # Create the list of culverts to get estimates for
        culvert.list <- edge.in[edge.in$Class == "Culvert", ]
        
        # If the culvert is identified at a end/start node, remove it
        culvert.list <- culvert.list[culvert.list$UpstreamSeg != culvert.list$DownstreamSeg, ]
        
        # Add a new column for storing the results
        edge.in$Distance <- NA
        edge.in$UpstreamCulverts <- NA
        
        # Loop through each culvert and calculate the gradient of the stream confluence
        
        print(paste0("Upstream assignment start: ", Sys.time()))
        
        for (culvert in culvert.list$TARGET_FID) {
                
                edge.in[edge.in$TARGET_FID == culvert, c("Distance", "UpstreamCulverts")] <- upstream_distance(culvert.id = culvert.list[culvert.list$TARGET_FID == culvert, "Node"],
                                                                                        node.network = node.in, 
                                                                                        edge.network = edge.in)
                
        }
        
        print(paste0("Upstream assignment complete: ", Sys.time()))
        
        # Remove all self intersecting end points
        edge.in <- edge.in[edge.in$UpstreamSeg != edge.in$DownstreamSeg, ]
        
        # Update R object and save
        watershed.network[["Edge_Cleaned"]] <- edge.in
        save(watershed.network, file = watershed.path[grep(watershed, watershed.path)])
        
        print(watershed)
        
}