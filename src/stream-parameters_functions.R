#
# Title: Functions for extracting stream parameters
# Created: September 1st, 2021
# Last Updated: June 28th, 2024
# Author: Brandon Allen
# Objectives: Define functions required for extracting stream slope and upsteam distance
# Keywords: Stream confluence, Stream Slope, Stream Distance, Upstream Distance, Network Visualization
# Note: 
#

#####################
# Stream confluence # Wrapper function that includes the stream_slope function
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_confluence <- function(watershed.path) {
        
        # Load watershed
        load(watershed.path)
        
        # Load appropriate node and edge data
        confluence.edge <- watershed.network[["Edge"]]
        confluence.node <- watershed.network[["Node"]]
        
        # If a watershed does not have any culverts present, it is assumed 100% connected.
        # Therefore, we can skip all watersheds where the "Edge" network is NULL
        if(is.null(watershed.network[["Edge"]])) {
                
                # Update R object and save
                watershed.network[["Edge_Cleaned"]] <- watershed.network$Edge
                watershed.network[["Node_Cleaned"]] <- watershed.network$Node
                save(watershed.network, file = watershed.path)
                next()
                
        }
        
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
        
        # Identify stream segments associated with each HUC 8. There may be instances of segments running into both, but 
        # this sound be a small total length.
        
        confluence.edge$TotalLength <- NA
        
        for(basin in unique(confluence.edge$WatershedPerm)) {
                
                temp.data <- unique(c(confluence.edge[confluence.edge$WatershedPerm == basin, c("UpstreamSeg")], 
                                      confluence.edge[confluence.edge$WatershedPerm == basin, c("DownstreamSeg")]))
                
                confluence.edge$TotalLength[confluence.edge$WatershedPerm == basin] <- sum(confluence.node[confluence.node$Stream %in% temp.data, "SectionLength"])
                
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
        
        # Update R object and save
        watershed.network[["Edge_Cleaned"]] <- confluence.edge
        watershed.network[["Node_Cleaned"]] <- confluence.node
        save(watershed.network, file = watershed.path)
        
}

#################
# Stream Slopes # Calculates the stream slope (confluence or reach) using the base edge and node files
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_slope <- function(culvert.id, culvert.list, node.data, edge.data, slope.type) {
  
        # Identify focal culvert and neighbouring segments
        focal.culvert <- culvert.list[culvert.list$TARGET_FID == culvert.id, ]
        upstream.segment <- focal.culvert$UpstreamSeg
        downstream.segment <- focal.culvert$DownstreamSeg
        
        # Add lengths together
        total.length <- sum(node.data[node.data$Stream %in% c(upstream.segment, downstream.segment), "SectionLength"])
        
        # Upstream check
        # Define false flag so while statement will continue until objective is met
        upstream.check <- FALSE
        previous.node <- focal.culvert$Node # Define the previous node visited so we can remove it
        
        while(upstream.check == FALSE) {
                
                # Identify segments directly upstream
                edge.upstream <- edge.data[edge.data$UpstreamSeg == upstream.segment | edge.data$DownstreamSeg == upstream.segment, ]
                
                # Remove the previous node
                edge.upstream <- edge.upstream[edge.upstream$Node != previous.node, ]
                
                if(nrow(edge.upstream) == 1){
                        
                        # Check if it is the start of a network
                        if(edge.upstream$UpstreamSeg == edge.upstream$DownstreamSeg) {
                                
                                upstream.elevation <- edge.upstream$Elevation
                                upstream.check <- TRUE
                                
                        } else {
                                
                                # As we can't confirm upstream/downstream via the value, identify the value that does not equal the current downstream* value
                                if(edge.upstream$UpstreamSeg != upstream.segment) {
                                        
                                        upstream.segment <- edge.upstream$UpstreamSeg
                                        
                                } else {
                                        
                                        upstream.segment <- edge.upstream$DownstreamSeg
                                        
                                }
                                
                                # Check if stream segment connects to like habitat (lake, strahler order)
                                strm.seg <- node.data[node.data$Stream %in% upstream.segment, ]
                                habitat.match <- node.data[node.data$Stream %in% c(edge.upstream$UpstreamSeg, edge.upstream$DownstreamSeg), "HabitatType"]
                                
                                if(habitat.match[1] != habitat.match[2]) {
                                        
                                        # Pull out the elevation information and stop
                                        upstream.elevation <- edge.upstream$Elevation
                                        upstream.check <- TRUE   
                                        
                                } else {
                                        
                                        if(slope.type == "Reach") {
                                                
                                                if(edge.upstream$Reach_Match == focal.culvert$Culvert_Match) {
                                                        
                                                        # Pull out the elevation information and stop
                                                        upstream.elevation <- edge.upstream$Elevation
                                                        upstream.check <- TRUE  
                                                        
                                                } else {
                                                        
                                                        # Define add length of new segment and update previous node
                                                        total.length <- total.length + strm.seg$SectionLength
                                                        previous.node <- edge.upstream$Node
                                                        
                                                }
                                                
                                        }
                                        
                                        if(slope.type == "Confluence") {
                                                
                                                # Define add length of new segment and update previous node
                                                total.length <- total.length + strm.seg$SectionLength
                                                previous.node <- edge.upstream$Node
                                                
                                        }
                                        
                                        
                                }
                                
                        }
                } 
                
                # Check if it is a confluence bound
                if(nrow(edge.upstream) > 1) {
                        
                        # Take the mean elevation value at the location
                        upstream.elevation <- mean(edge.upstream$Elevation)
                        upstream.check <- TRUE
                        
                }
                
                
        }
        
        # Downstream check
        # Define false flag so while statement will continue until objective is met
        downstream.check <- FALSE
        previous.node <- focal.culvert$Node # Define the previous node visited so we can remove it
        
        while(downstream.check == FALSE) {
                
                # Identify segments directly downstream*
                edge.downstream <- edge.data[edge.data$UpstreamSeg == downstream.segment | edge.data$DownstreamSeg == downstream.segment, ]
                
                # Remove the previous node
                edge.downstream <- edge.downstream[edge.downstream$Node != previous.node, ]
                
                if(nrow(edge.downstream) == 1){
                        
                        # Check if it is the start of a network
                        if(edge.downstream$UpstreamSeg == edge.downstream$DownstreamSeg) {
                                
                                downstream.elevation <- edge.downstream$Elevation
                                downstream.check <- TRUE
                                
                        } else {
                                
                                # As we can't confirm upstream/downstream via the value, identify the value that does not equal the current downstream* value
                                if(edge.downstream$UpstreamSeg != downstream.segment) {
                                        
                                        downstream.segment <- edge.downstream$UpstreamSeg
                                        
                                } else {
                                        
                                        downstream.segment <- edge.downstream$DownstreamSeg
                                        
                                }
                                
                                # Check if stream segment connects to like habitat (lake, strahler order)
                                strm.seg <- node.data[node.data$Stream %in% downstream.segment, ]
                                habitat.match <- node.data[node.data$Stream %in% c(edge.downstream$UpstreamSeg, edge.downstream$DownstreamSeg), "HabitatType"]
                                
                                if(habitat.match[1] != habitat.match[2]) {
                                        
                                        # Pull out the elevation information and stop
                                        downstream.elevation <- edge.downstream$Elevation
                                        downstream.check <- TRUE   
                                        
                                } else {
                                        
                                        if(slope.type == "Reach") {
                                                
                                                if(edge.downstream$Reach_Match == focal.culvert$Culvert_Match) {
                                                        
                                                        # Pull out the elevation information and stop
                                                        downstream.elevation <- edge.downstream$Elevation
                                                        downstream.check <- TRUE  
                                                        
                                                } else {
                                                        
                                                        # Define add length of new segment and update previous node
                                                        total.length <- total.length + strm.seg$SectionLength
                                                        previous.node <- edge.downstream$Node
                                                        
                                                }
                                        }
                                        
                                        if (slope.type == "Confluence") {
                                                
                                                # Define add length of new segment and update previous node
                                                total.length <- total.length + strm.seg$SectionLength
                                                previous.node <- edge.downstream$Node
                                                
                                        }
                                        
                                }
                                
                        }
                        
                } 
                
                # Check if it is a confluence bound
                if(nrow(edge.downstream) > 1) {
                        
                        # Take the mean elevation value at the location
                        downstream.elevation <- mean(edge.downstream$Elevation)
                        downstream.check <- TRUE
                        
                }
                
        }
        
        
        # Return the results
        return(ifelse(downstream.elevation >= upstream.elevation, 
                      (downstream.elevation - upstream.elevation) / total.length,
                      (upstream.elevation - downstream.elevation) / total.length))
        
        
}

###################
# Stream Distance # Wrapper function that includes the upstream_distance function
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_distance <- function(watershed.path) {
        
        #####################
        # Upstream Distance #
        #####################
        
        # load the stream network data
        load(watershed.path)
        
        # Load appropriate node and edge data
        edge.in <- watershed.network[["Edge_Cleaned"]]
        node.in <- watershed.network[["Node_Cleaned"]]
        
        # If a watershed does not have any culverts present, it is assumed 100% connected.
        # Therefore, we can skip all watersheds where the "Edge" network is NULL
        if(is.null(watershed.network[["Edge_Cleaned"]])) {
                
                # Update R object and save
                watershed.network[["Edge_Cleaned"]] <- watershed.network$Edge_Cleaned
                watershed.network[["Node_Cleaned"]] <- watershed.network$Node_Cleaned
                save(watershed.network, file = watershed.path)
                
        }
        
        # Create the list of culverts to get estimates for
        culvert.list <- edge.in[edge.in$Class == "Culvert", ]
        
        # If the culvert is identified at a end/start node, remove it
        culvert.list <- culvert.list[culvert.list$UpstreamSeg != culvert.list$DownstreamSeg, ]
        
        # Add a new column for storing the results
        edge.in$Distance <- NA
        edge.in$UpstreamCulverts <- NA
        
        # Loop through each culvert and calculate the gradient of the stream confluence
        for (culvert in culvert.list$TARGET_FID) {
                
                edge.in[edge.in$TARGET_FID == culvert, c("Distance", "UpstreamCulverts")] <- upstream_distance(culvert.id = culvert.list[culvert.list$TARGET_FID == culvert, "Node"],
                                                                                                               node.network = node.in, 
                                                                                                               edge.network = edge.in)
                
        }
        
        # Remove all self intersecting end points
        edge.in <- edge.in[edge.in$UpstreamSeg != edge.in$DownstreamSeg, ]
        
        # Update R object and save
        watershed.network[["Edge_Cleaned"]] <- edge.in
        save(watershed.network, file = watershed.path)
        
}

#####################
# Upstream Distance # Calculates total upstream distance from any point and repairs the stream network
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

upstream_distance <- function(edge.network, node.network, culvert.id) {
  
  require(igraph)
  
  # Create the stream network from the broken network
  broken.network <- network_visualization(edge.network = edge.network[edge.network$Node != culvert.id, "Node"], 
                                          conversion = TRUE) 
  
  # If the last stream segment has a culvert on it, the membership component will fail.
  # Manually add that node to the graph
  if(length(V(broken.network)) != nrow(node.network)) {
          
          broken.network <- add_vertices(broken.network, (nrow(node.network) - length(V(broken.network))))
          
  }
  
  # Identify focal segments
  upstream.segment <- strsplit(culvert.id, split = "-")[[1]][1]
  downstream.segment <- strsplit(culvert.id, split = "-")[[1]][2]
  
  # Identify the membership of each stream segment
  node.network["Membership"] <- components(broken.network)$membership
  
  upstream.group <- node.network[node.network$Stream == upstream.segment, "Membership"]
  upstream.group <- node.network[node.network$Membership == upstream.group, ]
  
  downstream.group <- node.network[node.network$Stream == downstream.segment, "Membership"]
  downstream.group <- node.network[node.network$Membership == downstream.group, ]
  
  # Identify nodes directly upstream*
  edge.upstream <- edge.network[edge.network$UpstreamSeg %in% upstream.group$Stream | edge.network$DownstreamSeg %in% upstream.group$Stream, ]

  # Identify nodes directly downstream*
  edge.downstream <- edge.network[edge.network$UpstreamSeg %in% downstream.group$Stream | edge.network$DownstreamSeg %in% downstream.group$Stream, ]

  # Identify which group is upstream
  if(min(edge.upstream$Elevation, na.rm = TRUE) > min(edge.downstream$Elevation, na.rm = TRUE)) {
    
    upstream.distance <- sum(upstream.group$SectionLength)
    culvert.modifications <- as.numeric(table(edge.upstream$Class)["Culvert"])
    
  } else {
    
    upstream.distance <- sum(downstream.group$SectionLength)
    culvert.modifications <- as.numeric(table(edge.downstream$Class)["Culvert"])
    
  }
  
  # If there are no culverts upstream, mark as 0
  if(is.na(culvert.modifications)) {
    
    culvert.modifications <- 0
    
  }
  
  return(c(upstream.distance, culvert.modifications))
  
}

#########################
# Network Visualization # Format edge data and construct a network.
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
                
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
                
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}