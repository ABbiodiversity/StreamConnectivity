#
# Title: Assessment of autocorrelation distance
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Assess the mean distance between each segment and other stream order types.
# Keywords: Notes, Autocorrelation Distance
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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

source("src/connectivity-status-functions.R")

# Define watersheds
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

for (SSO.ID in watershed.ids) {
    
    # Check if predicted values exist
    
    temp.name <- paste0("data/processed/huc-6/2018/predicted/passability/", SSO.ID, "-predicted-culverts_2021-05-26.csv")
    if (file.exists(temp.name) != TRUE) {
        
        next
        
    } else {
        
        node.data.raw <- read.csv(paste0("data/processed/huc-6/2018/connectivity/", SSO.ID, "-node-connectivity.csv"))
        edge.data.cur <- read.csv(temp.name)
        
        # Identify base network structure
        stream.network <- network_visualization(edge.network = edge.data.cur$Node, 
                                                conversion = TRUE) 
        
        #######################
        # Stream Connectivity #
        #######################
        
        # In some instances, when the last stream segments are not connected to other segments,
        # they are removed from the created stream network. If this occurs, remove the extra stream segments and store them.
        
        membership.check <- components(stream.network)$membership
        
        if(nrow(node.data.raw) == length(membership.check) ) {
            
            node.data.raw["Membership"] <- membership.check
            
        } else{
            
            stream.extra <- node.data.raw[-(1:length(membership.check)), ]
            node.data.raw <- node.data.raw[1:length(membership.check), ]
            node.data.raw["Membership"] <- membership.check
            
        }
        
        #########################################
        # Parallel Processing of Stream Network # 
        #########################################
        
        # For each network, calculate connectivity
        # Fix the lakes to -1
        
        node.data.raw$HabitatType[node.data.raw$StreamType == "LAKE PATH"] <- -1
        stream.habitat <- sort(unique(node.data.raw$HabitatType))
        stream.weight <- (node.data.raw[edge.data.cur$UpstreamSeg, "SectionLength"] / 2) + (node.data.raw[edge.data.cur$DownstreamSeg, "SectionLength"] / 2)
        
        # Define group size (2500)
        # Create x number of groups
        x <- 1
        group.se <- NULL
        while (x < nrow(node.data.raw)) {
            
            group.se <- rbind(group.se, c(x, x + 2499))
            x <- x + 2500
            
        }
        
        # Adjust values that are greater than the number of possible segments
        group.se[group.se >= nrow(node.data.raw)] <- nrow(node.data.raw)
        
        # Remove unecessary objects
        rm(edge.data.cur)
        
        for (ngroup in 1:nrow(group.se)) {
            
            # Define inputs required for parallel processing.
            core.input <- makeCluster(3)
            clusterExport(core.input, c("stream.network", "node.data.raw",  
                                        "stream.habitat", "stream.weight", "stream_distance_2.0"))
            clusterEvalQ(core.input, {
                library(igraph)
            })
            
            # Calculate connectivity for each stream in the network
            Cj.temp <- parSapply(core.input, node.data.raw$Stream[group.se[ngroup, 1]:group.se[ngroup, 2]], function(stream.id) stream_distance_2.0(focus.stream = stream.id, 
                                                                                                                                                base.network = stream.network, 
                                                                                                                                                base.node = node.data.raw,  
                                                                                                                                                habitat.type = stream.habitat,
                                                                                                                                                segment.weight = stream.weight))
            
            
            stopCluster(core.input)
            
            Cj.temp <- t(Cj.temp)
            colnames(Cj.temp) <- c("StreamID", "HabitatType", "Distance") 
            
            write.csv(Cj.temp, paste0("data/processed/autocorrelation-distance/tables/stream-distance-", SSO.ID, "-subset-", ngroup, "_2021-06-28.csv"), row.names = FALSE)
            
            print(SSO.ID)
            
        }
        
    }
    
}

####################
# Average Distance #
####################

# Combine distances into a single file

distance.list <- list.files("data/processed/autocorrelation-distance/tables/", full.names = TRUE)

for (x in 1:length(distance.list)) {
    
    if (x == 1) {
        
        compiled.network <- read.csv(distance.list[x])
        compiled.network <- compiled.network[!duplicated(compiled.network), ]
        
    } else {
        
        temp.network <- read.csv(distance.list[x])
        temp.network <- temp.network[!duplicated(temp.network), ]
        compiled.network <- rbind(compiled.network, temp.network)
        
        rm(temp.network)
        
    }

    print(x)
    
}

# Visualize
library(ggplot2)
library(abmi.themes)

compiled.network <- compiled.network[!is.na(compiled.network$HabitatType), ]
compiled.network <- compiled.network[!is.na(compiled.network$Distance), ]

compiled.network <- compiled.network[compiled.network$HabitatType == 1, ]

png(filename =  paste0("data/processed/autocorrelation-distance/figures/stream-distance-huc6_", Sys.Date(), ".png"),
    width = 1200,
    height = 800)

compiled.network$Kilometres <- compiled.network$Distance / 1000

print(ggplot(compiled.network, aes(x=Kilometres)) + 
    geom_histogram(color = abmi_pal(palette = "main")(2)[1], fill =  abmi_pal(palette = "main")(2)[2], bins = 50) + 
    geom_vline(aes(xintercept = mean(Kilometres)), color = abmi_pal(palette = "main")(2)[1], linetype = "dashed", size = 1) +
    scale_x_continuous(breaks = seq(from = 0, to = 800, by = 50)) +
    scale_fill_identity() +
    labs(x = "Kilometres", y = "Count") +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()))

dev.off()

rm(list=ls())
gc()
