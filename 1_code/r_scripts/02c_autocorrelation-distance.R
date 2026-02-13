# ---
# title: "Assessing autocorrelation distance"
# author: "Brandon Allen"
# created: "2025-01-12"
# inputs: ["0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf";
#          "network_HUC.Rdata - One for each year and HUC watershed"]
# outputs: ["network_HUC.Rdata - One for each year and HUC watershed. 
#           "autocorrelation-distance.png"]
# notes: 
#   "This script assesses the mean distance between each segment and other stream order types."
# ---

# 1.0 Clear memory ----
rm(list=ls())
gc()

# 1.1 Load libraries and source functions ----
library(foreach)
library(ggplot2)
library(igraph)
library(parallel)
source("1_code/r_scripts/connectivity-status_functions.R")

# 1.2 Define watersheds and analysis year (only required for the most recent inventory) ----
watershed.ids <- read.dbf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
huc.scale <- 6
hfi.year <- 2021
n.clusters <- 8 # Adjust the number of cores

# 1.3 loop through each watershed and calculate the mean stream distances ----
for (HUC in watershed.ids) {
        
        ##################
        # Create Network #
        ##################
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
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
        
        save(watershed.network, file = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                              hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        print(HUC)
}


# 2.0 Visualization ----
autocorrelation.distance <- NULL
for (HUC in watershed.ids) { 
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                    hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        autocorrelation.distance <- rbind(watershed.network$d_j, autocorrelation.distance)
        
}

autocorrelation.distance <- autocorrelation.distance[!is.na(autocorrelation.distance[, "HabitatType"]), ]
autocorrelation.distance <- autocorrelation.distance[!is.na(autocorrelation.distance[, "Distance"]), ]
autocorrelation.distance <- as.data.frame(autocorrelation.distance[autocorrelation.distance[, "HabitatType"] == 1, ])
autocorrelation.distance[, "Kilometres"] <- autocorrelation.distance[, "Distance"] / 1000


correlation.plot <- ggplot(autocorrelation.distance, aes(x=Kilometres)) + 
        geom_histogram(color = "#000000", fill =  "#a8af8c", bins = 50) + 
        geom_vline(aes(xintercept = mean(Kilometres)), color = "#000000", linetype = "dashed", size = 1) +
        scale_x_continuous(breaks = seq(from = 0, to = 800, by = 50)) +
        scale_fill_identity() +
        labs(x = "Kilometres", y = "Count") +
        ggtitle(paste0("Autocorrelation distance = ", round(mean(autocorrelation.distance$Kilometres), 2))) +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "3_output/figures/autocorrelation-distance.jpeg",
       plot = correlation.plot,
       height = 800,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")


rm(list=ls())
gc()
