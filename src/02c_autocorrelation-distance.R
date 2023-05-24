#
# Title: Assessment of autocorrelation distance
# Created: September 1st, 2021
# Last Updated: May 10th, 2023
# Author: Brandon Allen
# Objectives: Assess the mean distance between each segment and other stream order types.
# Keywords: Notes, Autocorrelation Distance, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
############################
# Autocorrelation Distance # 
############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries required scripts
library(foreach)
library(igraph)
library(parallel)

source("src/connectivity-status_functions.R")

# Define watersheds
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
huc.scale <- 6
hfi.year <- 2018
n.clusters <- 8 # Adjust the number of cores

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
        stream.habitat <- sort(unique(node.data$HabitatType))
        stream.weight <- (node.data[edge.data$UpstreamSeg, "SectionLength"] / 2) + (node.data[edge.data$DownstreamSeg, "SectionLength"] / 2)
        
        # Define inputs required for parallel processing.
        core.input <- makeCluster(n.clusters)
        clusterExport(core.input, c("stream.network", "node.data",  
                                    "stream.habitat", "stream.weight", "stream_distance"))
        clusterEvalQ(core.input, {
                library(igraph)
        })
        
        # Calculate distance for each stream in the network
        Cj.temp <- parSapply(core.input, node.data$Stream, function(stream.id) stream_distance(focus.stream = stream.id,
                                                                                               base.network = stream.network, 
                                                                                               base.node = node.data, 
                                                                                               habitat.type = stream.habitat,
                                                                                               segment.weight = stream.weight))
        
        
        stopCluster(core.input)
        
        # Format the data frame
        autocorrelation.distance <- t(Cj.temp)
        colnames(autocorrelation.distance) <- c("StreamID", "HabitatType", "Distance") 
        watershed.network[["d_j"]] <- autocorrelation.distance
        
        save(watershed.network, file = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                              hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        print(HUC)
}


#################
# Visualization #
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(abmi.themes)
library(ggplot2)

# For each watershed, loop through and stitch the autocorrelation results together
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
huc.scale <- 6
hfi.year <- 2018
n.clusters <- 8 # Adjust the number of cores
autocorrelation.distance <- NA

for (HUC in watershed.ids) { 
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                    hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        autocorrelation.distance <- rbind(watershed.network$d_j, autocorrelation.distance)
        
}

autocorrelation.distance <- autocorrelation.distance[!is.na(autocorrelation.distance[, "HabitatType"]), ]
autocorrelation.distance <- autocorrelation.distance[!is.na(autocorrelation.distance[, "Distance"]), ]
autocorrelation.distance <- as.data.frame(autocorrelation.distance[autocorrelation.distance[, "HabitatType"] == 1, ])
autocorrelation.distance[, "Kilometres"] <- autocorrelation.distance[, "Distance"] / 1000

png(filename =  paste0("results/figures/autocorrelation-distance.png"),
    width = 1200,
    height = 800)

print(ggplot(autocorrelation.distance, aes(x=Kilometres)) + 
              geom_histogram(color = abmi_pal(palette = "main")(2)[1], fill =  abmi_pal(palette = "main")(2)[2], bins = 50) + 
              geom_vline(aes(xintercept = mean(Kilometres)), color = abmi_pal(palette = "main")(2)[1], linetype = "dashed", size = 1) +
              scale_x_continuous(breaks = seq(from = 0, to = 800, by = 50)) +
              scale_fill_identity() +
              labs(x = "Kilometres", y = "Count") +
              ggtitle(paste0("Autocorrelation distance = ", round(mean(autocorrelation.distance$Kilometres), 2))) +
              theme_light() +
              theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()))

dev.off()

rm(list=ls())
gc()
