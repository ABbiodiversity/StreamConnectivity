#
# Title: Calculating stream connectivity status
# Created: September 1st, 2021
# Last Updated: May 11th, 2023
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
# Environment initialization #
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
hfi <- c(2010, 2018)
huc.scale <- 6
n.clusters <- 8 # Adjust the number of cores

# Define spatial autocorrelation variable
# Mean distance of first order streams to highest order stream segment
# HUC 6 - 71000, based on results from 02c_autocorrelation-distance.R
Autocorrelation.d0 <- 71000 

for (hfi.year in hfi) {
        
        for (HUC in watershed.ids) { 
                
                ##################
                # Create Network #
                ##################
                
                # Load the appropriate Rdata with the culvert information
                load(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                            hfi.year, "/connectivity/network_", HUC, ".Rdata"))
                
                # If there are no culverts in the watershed, skip the summary. 
                # This is because we are skipping watersheds where we can assuming connectivity is 100% due to no road/rail infrastructure
                if(is.null(watershed.network[["Edge_Cleaned"]])) {
                        
                        next()
                        
                }
                
                node.data <- watershed.network[["Node_Cleaned"]]
                edge.data <- watershed.network[["Edge_Cleaned"]]
                
                # If there is an NA value edge, remove it
                edge.data <- edge.data[!is.na(edge.data$UpstreamSeg), ]
                
                # Create the reference condition where passability is 1
                edge.data.ref <- edge.data
                edge.data.ref$Up <- 1
                
                # Identify base network structure
                stream.network <- network_visualization(edge.network = edge.data$Node, 
                                                        conversion = TRUE) 
                
                #######################
                # Stream Connectivity #
                #######################
                
                # In some instances, when the last stream segments are not connected to other segments,
                # they are removed from the created stream network. If this occurs, remove the extra stream segments and store them.
                membership.check <- components(stream.network)$membership
                
                if(nrow(node.data) == length(membership.check)) {
                        
                        node.data["Membership"] <- membership.check
                        
                } else{
                        
                        stream.extra <- node.data[-(1:length(membership.check)), ]
                        node.data <- node.data[1:length(membership.check), ]
                        node.data["Membership"] <- membership.check
                        
                }
                
                #########################################
                # Parallel Processing of Stream Network # 
                #########################################
                
                # For each network, calculate connectivity
                row.names(node.data) <- node.data$Stream
                stream.weight <- (node.data[edge.data$UpstreamSeg, "SectionLength"] / 2) + (node.data[edge.data$DownstreamSeg, "SectionLength"] / 2)
                passability.weight <- log(edge.data$Up) * -1
                
                # For each network, calculate connectivity
                largest.network <- as.numeric(names(sort(table(node.data$Membership), decreasing = FALSE))) # Smallest to largest network ID
                
                # Create a blank object for storing the results across sub networks
                connectivity <- NULL
                
                for (ID in largest.network) { 
                        
                        # Subset the stream segments and filter the number of available streams
                        node.data.subset <- node.data[grep(paste("^", ID, "$", sep = ""), node.data$Membership), ]
                        stream.habitat <- as.numeric(names(table(node.data.subset$HabitatType))) # Identify the stream habitat types within the network
                        
                        # Define inputs required for parallel processing.
                        core.input <- makeCluster(n.clusters)
                        clusterExport(core.input, c("stream.network", "node.data", "edge.data", "edge.data.ref", 
                                                    "stream.habitat", "Autocorrelation.d0",
                                                    "stream.weight", "passability.weight", "stream_connectivity"))
                        clusterEvalQ(core.input, {
                                library(igraph)
                        })
                        
                        # Calculate connectivity for each stream in the network
                        Cj <- parSapply(core.input, node.data.subset$Stream, function(stream.id) stream_connectivity(focus.stream = stream.id, 
                                                                                                                     base.network = stream.network, 
                                                                                                                     base.node = node.data, 
                                                                                                                     ref.edge = edge.data.ref, 
                                                                                                                     cur.edge = edge.data, 
                                                                                                                     habitat.type = stream.habitat,
                                                                                                                     distance.weight = stream.weight,
                                                                                                                     probability.weight = passability.weight,
                                                                                                                     d0 = Autocorrelation.d0))
                        
                        
                        stopCluster(core.input)
                        
                        ##################
                        # Data formating #
                        ##################
                        
                        Cj <- data.frame(t(Cj)) # Transpose
                        
                        for (col.id in 0:(ncol(Cj) / 4 - 1)) {
                                
                                if(col.id == 0) {
                                        
                                        Cj.formated <- Cj[, 1:4]
                                        colnames(Cj.formated) <- c("StreamID", "HabitatType", "RefConnect", "CurConnect")
                                        
                                } else {
                                        
                                        Cj.temp <- Cj[, seq(col.id * 4 + 1, (col.id + 1) * 4, 1)]
                                        colnames(Cj.temp) <- c("StreamID", "HabitatType", "RefConnect", "CurConnect")
                                        
                                        Cj.formated <- rbind.data.frame(Cj.formated, Cj.temp)
                                        
                                }
                                
                        }
                        
                        rm(Cj)
                        
                        ##########################
                        # Watershed Connectivity #
                        ##########################
                        
                        # Calculate watershed connectivity for each stream segment within the network
                        c.watershed <- watershed_status(filter.node = node.data.subset, connect.status = Cj.formated)
                        
                        # If single segments (NA numerator) are lakes, fix numerator and denominator to 0 with StreamConnect == 1
                        # If single segments (NA numerator) are streams, fix numerator to denominator and StreamConnect == 1
                        
                        # If that single segment is a lake, it does not count for regional connectivity, but the polyline is still StreamConnect = 1. 
                        # Otherwise, it does count.
                        
                        # If there is only one segment, fix the connectivity to 100%
                        if(nrow(c.watershed) == 1) {
                                
                                if (node.data[node.data$Stream == c.watershed$StreamID, "HabitatType"] == -1) {
                                        
                                        c.watershed[1, c("StreamConnect", "Numerator", "Denominator")] <- c(1, NaN, NaN)
                                        
                                } else {
                                        
                                        c.watershed[1, c("StreamConnect", "Numerator")] <- c(1, c.watershed[1, c("Denominator")])
                                        
                                }
                                
                        }
                        
                        # Store the results from each sub network in a list
                        connectivity <- rbind(c.watershed, connectivity)
                        
                }
                
                # Store the results as part of the original R object
                watershed.network[["Connectivity"]] <- connectivity
                
                save(watershed.network, file = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                      hfi.year, "/connectivity/network_", HUC, ".Rdata"))
                
                print(HUC)
                
        }
        
}
