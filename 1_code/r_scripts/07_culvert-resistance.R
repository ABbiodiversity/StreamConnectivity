# ---
# title: "Culvert resistance indicator"
# author: "Brandon Allen"
# created: "2025-12-12"
# inputs: ["2_pipeline/huc-6/connectivity/"]
# outputs: ["2_pipeline/resistance/", watershed, "-resistance-collapse.RData"]
# notes: 
#   "The objective of this case study is to assess if we can apply a CRI to individual streams
#    Calculating individual end-point leads to a misrepresentation of the impacts at intersections with multiple
#    pathways. Therefore, we are going to block all intersections associated with each stream to give us a representation
#    of potential resistance if a stream segment is "removed".
#    We also calculate this same information but only block culverts on the landscape instead of all potential stream segments.
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries and code ----
library(foreach)
library(foreign)
library(igraph)
library(parallel)
source("1_code/r_scripts/connectivity-status_functions.R")

# 1.3 Define watershed boundaries to be calculated ----
# It does not matter which year is used as we are estimating maximum potential impact
watershed.ids <- c("040102")
huc.scale <- 6
hfi.year <- 2022
Start.time <- Sys.time()

# 3.0 Stream Resistance (Only Culverts) ----
for (watershed in watershed.ids) {
        
        # Load focal watershed of interest
        results.list <- list.files(paste0("2_pipeline/huc-", huc.scale, "/", 
                                          hfi.year, "/connectivity/"), full.names = TRUE)
        
        # Load a stream layer
        load(results.list[grep(watershed, results.list)])
        
        # Load the watershed of interest 
        node.data.raw <- watershed.network$Node_Cleaned
        edge.data.cur <- watershed.network$Edge_Predicted
        
        # Create results data frame
        results.store <- data.frame(StrmID = node.data.raw$Stream,
                                    Status = NA)
        
        # Identify base network structure
        base.network <- network_visualization(edge.network = edge.data.cur$Node, 
                                              conversion = TRUE) 
        
        # Assign membership
        membership.check <- components(base.network)$membership
        
        if(nrow(node.data.raw) == length(membership.check) ) {
                
                node.data.raw["Membership"] <- membership.check
                
        } else{
                
                stream.extra <- node.data.raw[-(1:length(membership.check)), ]
                node.data.raw <- node.data.raw[1:length(membership.check), ]
                node.data.raw["Membership"] <- membership.check
                
        }
        
        # Define spatial autocorrelation variable ----
        # Mean distance of first order streams to highest order stream segment
        # HUC 6 - 71000, based on results from 02c_autocorrelation-distance.R
        d0 <- 71000 
        
        # Define distance and probability weights
        row.names(node.data.raw) <- node.data.raw$Stream
        edge.data.cur["StreamWeight"] <- (node.data.raw[edge.data.cur$UpstreamSeg, "SectionLength"] / 2) + (node.data.raw[edge.data.cur$DownstreamSeg, "SectionLength"] / 2)
        
        # Define lakes as a stream order -1
        node.data.raw$HabitatType[node.data.raw$StreamType == "LAKE PATH"] <- -1
        
        # Create cleaned edge and node data that can be modified within the function
        base.node <- node.data.raw[, c("Stream", "SectionLength", "HabitatType", "HabitatQuality", "Membership")]
        base.edge <- edge.data.cur[, c("TARGET_FID", "Node", "UpstreamSeg", "DownstreamSeg", "Up", "Down", "Class", "StreamWeight")]
        
        rm(edge.data.cur, node.data.raw)
        
        # Create the distance and si matrices, only need to update the prob.mat
        dist.mat <- (1 / (1 + (distances(graph = base.network, weights = base.edge$StreamWeight) / d0)^2))
        
        # Calculate the matrices for Si, Qi, and Ni
        # si.vect <- base.node$SectionLength # Just using dataframe instead of storing separately.
        
        # For each culvert associated with the focal stream, block.
        # Not all segments will have up and down stream points, only blocks if present.
        unique.segments <- base.edge[base.edge$Class %in% c("Culvert", "Dam", "Bridge"), c("UpstreamSeg", "DownstreamSeg")]
        unique.segments <- unique(unique.segments$UpstreamSeg, unique.segments$DownstreamSeg)
        
        for (focus.stream in unique.segments) {
                
                base.edge$Up <- 1
                base.edge$Up[base.edge$UpstreamSeg == focus.stream] <- 0
                base.edge$Up[base.edge$DownstreamSeg == focus.stream] <- 0
                
                # Calculate the matrix of distance and probability paths. Probability weights are converted to log scale to allow for multiplicative response
                
                prob.mat <- exp(distances(graph = base.network, weights = log(base.edge$Up) * -1) * -1)
                diag(dist.mat) <- 0 # Change the diagonal to 0 as a segment being connected to itself does not contribute to connectivity.
                
                # Calculate the matrices for Qi and Ni, Currently not implemented, mask to save resources
                # qi.mat <- matrix(nrow = nrow(prob.mat), ncol = ncol(prob.mat), data = 1)
                # ni.mat <- matrix(nrow = nrow(prob.mat), ncol = ncol(prob.mat), data = 1)
                
                # Identify the stream habitat types within the network
                habitat.type <- as.numeric(names(table(base.node$HabitatType))) 
                network.results <- data.frame(StreamID = rep(1:nrow(dist.mat), length(habitat.type)),
                                              HabitatType = sort(rep(habitat.type, nrow(dist.mat))),
                                              RefConnect = 0,
                                              CurConnect = 0)
                
                for (habitat in habitat.type) {
                        
                        # Create theta matrix
                        thetai.vect <- ifelse(base.node$HabitatType == habitat, 1, 0)
                        
                        # Calculate current and reference connectivity of stream in the context of the surrounding network, add to results
                        network.results[network.results$HabitatType == habitat, "RefConnect"] <- colSums(dist.mat * base.node$SectionLength * thetai.vect, na.rm = TRUE)
                        network.results[network.results$HabitatType == habitat, "CurConnect"] <-  colSums(dist.mat * prob.mat * base.node$SectionLength * thetai.vect, na.rm = TRUE)
                        
                }
                
                ##########################
                # Watershed Connectivity #
                ##########################
                
                # Calculate watershed connectivity for each stream segment within the network
                c.watershed <- watershed_status(filter.node = base.node, connect.status = network.results)
                
                # Subset to stream segments within the focal streams network
                c.watershed <- c.watershed[base.node$Membership %in% base.node[base.node$Stream == focus.stream, "Membership"], ]
                
                # If there is only one segment, fix the connectivity to 100%
                if(nrow(c.watershed) == 1) {
                        
                        c.watershed$Denominator <- NaN
                        
                }
                
                # As we want to look at the impact to the surrounding stream segments, as we know the isolate stream will have a 
                # connectivity value of 0, set that Numerator and Denominator value to NA.
                
                c.watershed[c.watershed$StreamID == focus.stream, c("Numerator", "Denominator")] <- NA
                
                stream.connect <- sum(c.watershed$Numerator, na.rm = TRUE) / sum(c.watershed$Denominator, na.rm = TRUE) * 100
                stream.connect <- ifelse(is.nan(stream.connect) == TRUE, 100, stream.connect) # Adjust for single stream networks
                
                # Store the results
                results.store[results.store$StrmID == focus.stream, "Status"] <- stream.connect
                
                rm(c.watershed, network.results, stream.connect, prob.mat)
                gc()
                
                print(focus.stream)
                
        }
        
        save(results.store, file = paste0("2_pipeline/resistance/", watershed, "-resistance-culverts.RData"))
        print(watershed.ids)
        
}

# 3.0 Stream Resistance (All Segments) ----
for (watershed in watershed.ids) {
        
        # Load focal watershed of interest
        results.list <- list.files(paste0("2_pipeline/huc-", huc.scale, "/", 
                                          hfi.year, "/connectivity/"), full.names = TRUE)
        
        # Load a stream layer
        load(results.list[grep(watershed, results.list)])
        
        # Load the watershed of interest 
        node.data.raw <- watershed.network$Node_Cleaned
        edge.data.cur <- watershed.network$Edge_Predicted

        # Create results data frame
        results.store <- data.frame(StrmID = node.data.raw$Stream,
                                    Status = NA)
        
        # Identify base network structure
        base.network <- network_visualization(edge.network = edge.data.cur$Node, 
                                              conversion = TRUE) 
        
        # Assign membership
        membership.check <- components(base.network)$membership
        
        if(nrow(node.data.raw) == length(membership.check) ) {
                
                node.data.raw["Membership"] <- membership.check
                
        } else{
                
                stream.extra <- node.data.raw[-(1:length(membership.check)), ]
                node.data.raw <- node.data.raw[1:length(membership.check), ]
                node.data.raw["Membership"] <- membership.check
                
        }
        
        # Define spatial autocorrelation variable ----
        # Mean distance of first order streams to highest order stream segment
        # HUC 6 - 71000, based on results from 02c_autocorrelation-distance.R
        d0 <- 71000 
        
        # Define distance and probability weights
        row.names(node.data.raw) <- node.data.raw$Stream
        edge.data.cur["StreamWeight"] <- (node.data.raw[edge.data.cur$UpstreamSeg, "SectionLength"] / 2) + (node.data.raw[edge.data.cur$DownstreamSeg, "SectionLength"] / 2)
        
        # Define lakes as a stream order -1
        node.data.raw$HabitatType[node.data.raw$StreamType == "LAKE PATH"] <- -1
        
        # Create cleaned edge and node data that can be modified within the function
        base.node <- node.data.raw[, c("Stream", "SectionLength", "HabitatType", "HabitatQuality", "Membership")]
        base.edge <- edge.data.cur[, c("TARGET_FID", "Node", "UpstreamSeg", "DownstreamSeg", "Up", "Down", "StreamWeight")]
        
        rm(edge.data.cur, node.data.raw)
        
        # Create the distance and si matrices, only need to update the prob.mat
        dist.mat <- (1 / (1 + (distances(graph = base.network, weights = base.edge$StreamWeight) / d0)^2))
        
        # Calculate the matrices for Si, Qi, and Ni
        # si.vect <- base.node$SectionLength # Just using dataframe instead of storing separately.
        
        # For each intersection associated with the focal stream, block.
        # Not all segments will have up and down stream points, only blocks if present.
        
        for (focus.stream in base.node$Stream) {
                
                base.edge$Up <- 1
                base.edge$Up[base.edge$UpstreamSeg == focus.stream] <- 0
                base.edge$Up[base.edge$DownstreamSeg == focus.stream] <- 0
                
                # Calculate the matrix of distance and probability paths. Probability weights are converted to log scale to allow for multiplicative response
                
                prob.mat <- exp(distances(graph = base.network, weights = log(base.edge$Up) * -1) * -1)
                diag(dist.mat) <- 0 # Change the diagonal to 0 as a segment being connected to itself does not contribute to connectivity.
                
                # Calculate the matrices for Qi and Ni, Currently not implemented, mask to save resources
                # qi.mat <- matrix(nrow = nrow(prob.mat), ncol = ncol(prob.mat), data = 1)
                # ni.mat <- matrix(nrow = nrow(prob.mat), ncol = ncol(prob.mat), data = 1)
                
                # Identify the stream habitat types within the network
                habitat.type <- as.numeric(names(table(base.node$HabitatType))) 
                network.results <- data.frame(StreamID = rep(1:nrow(dist.mat), length(habitat.type)),
                                              HabitatType = sort(rep(habitat.type, nrow(dist.mat))),
                                              RefConnect = 0,
                                              CurConnect = 0)
                
                for (habitat in habitat.type) {
                        
                        # Create theta matrix
                        thetai.vect <- ifelse(base.node$HabitatType == habitat, 1, 0)
                        
                        # Calculate current and reference connectivity of stream in the context of the surrounding network, add to results
                        network.results[network.results$HabitatType == habitat, "RefConnect"] <- colSums(dist.mat * base.node$SectionLength * thetai.vect, na.rm = TRUE)
                        network.results[network.results$HabitatType == habitat, "CurConnect"] <-  colSums(dist.mat * prob.mat * base.node$SectionLength * thetai.vect, na.rm = TRUE)
                        
                }
                
                ##########################
                # Watershed Connectivity #
                ##########################
                
                # Calculate watershed connectivity for each stream segment within the network
                c.watershed <- watershed_status(filter.node = base.node, connect.status = network.results)
                
                # Subset to stream segments within the focal streams network
                c.watershed <- c.watershed[base.node$Membership %in% base.node[base.node$Stream == focus.stream, "Membership"], ]
                
                # If there is only one segment, fix the connectivity to 100%
                if(nrow(c.watershed) == 1) {
                        
                        c.watershed$Denominator <- NaN
                        
                }
                
                # As we want to look at the impact to the surrounding stream segments, as we know the isolate stream will have a 
                # connectivity value of 0, set that Numerator and Denominator value to NA.
                
                c.watershed[c.watershed$StreamID == focus.stream, c("Numerator", "Denominator")] <- NA
                
                stream.connect <- sum(c.watershed$Numerator, na.rm = TRUE) / sum(c.watershed$Denominator, na.rm = TRUE) * 100
                stream.connect <- ifelse(is.nan(stream.connect) == TRUE, 100, stream.connect) # Adjust for single stream networks
                
                # Store the results
                results.store[results.store$StrmID == focus.stream, "Status"] <- stream.connect
                
                rm(c.watershed, network.results, stream.connect, prob.mat)
                gc()
                
                print(focus.stream)
                
        }
        
        save(results.store, file = paste0("2_pipeline/resistance/", watershed, "-resistance-collapse.RData"))
        print(watershed.ids)
        
}

Sys.time() - Start.time 

rm(list=ls())
gc()