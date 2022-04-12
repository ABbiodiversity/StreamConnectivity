#
# Title: Calculating stream connectivity status
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
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

source("src/connectivity-status-functions.R")

# Define the watershed lookup table

# Define watersheds
hfi <- c(2010, 2014, 2016, 2018)
huc.unit <- 6

watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- as.character(unique(watershed.ids$HUC_6)) # Includes all HUC-6 watersheds now

for (analysis.year in hfi) {
  
  for (SSO.ID in watershed.ids) {
    
    temp.name <- paste0("data/processed/huc-", huc.unit, "/", analysis.year, "/predicted/passability/", SSO.ID, "-predicted-culverts_2021-06-21.csv")
    if (file.exists(temp.name) != TRUE) {
      
      next
      
    }
    
    # Create directory to store the results
    dir.create(paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year, "/", SSO.ID))
    
    # Load cleaned datasets
    node.data.raw <- read.csv(paste("data/processed/huc-", huc.unit, "/", analysis.year, "/connectivity/", SSO.ID, "-node-connectivity.csv", sep = ""))
    edge.data.ref <- read.csv(temp.name)
    edge.data.cur <- edge.data.ref
    
    # Fix reference passability to 1
    edge.data.ref$Up <- 1
    
    # Identify base network structure
    stream.network <- network_visualization(edge.network = edge.data.cur$Node, 
                                            conversion = TRUE) 
    
    # Define spatial autocorrelation variable
    # Mean distance of first order streams to highest order stream segment
    # HUC 6 - 71000, based on results from 02b_autocorrelation-distance.R
    Autocorrelation.d0 <- 71000 
    
    #######################
    # Stream Connectivity #
    #######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # In some instances (SSO_5), when the last stream segments are not connected to other segments,
    # they are removed from the created stream network. If this occurs, remove the extra stream segments and store them.
    
    membership.check <- components(stream.network)$membership
    
    if(nrow(node.data.raw) == length(membership.check) ) {
      
      node.data.raw["Membership"] <- membership.check
      
    } else{
      
      stream.extra <- node.data.raw[-(1:length(membership.check)), ]
      node.data.raw <- node.data.raw[1:length(membership.check), ]
      node.data.raw["Membership"] <- membership.check
      
    }
    
    # Define Lakes as a unique habitat class
    # Fix the lakes to -1
    node.data.raw$HabitatType[node.data.raw$StreamType == "LAKE PATH"] <- -1
    
    # Add edge weights to the distance measure
    row.names(node.data.raw) <- node.data.raw$Stream
    stream.weight <- (node.data.raw[edge.data.cur$UpstreamSeg, "SectionLength"] / 2) + (node.data.raw[edge.data.cur$DownstreamSeg, "SectionLength"] / 2)
    passability.weight <- log(edge.data.cur$Up) * -1
    
    #########################################
    # Parallel Processing of Stream Network # 
    #########################################
    
    # For each network, calculate connectivity
    largest.network <- as.numeric(names(sort(table(node.data.raw$Membership), decreasing = FALSE))) # Largest to smallest network ID
    
    for (ID in largest.network) {
      
      node.data <- node.data.raw[grep(paste("^", ID, "$", sep = ""), node.data.raw$Membership), ]
      stream.habitat <- as.numeric(names(table(node.data$HabitatType))) # Identify the stream habitat types within the network
      
      # If node network is exceptionally large, subset into groups then stitch together.
      if(nrow(node.data) >= 2500) {
        
        # Create x number of groups
        x <- 1
        group.se <- NULL
        while (x <= nrow(node.data)) {
          
          group.se <- rbind(group.se, c(x, x + 2499))
          x <- x + 2500
          
        }
        
        # Adjust values that are greater than the number of possible segments 
        group.se[group.se >= nrow(node.data)] <- nrow(node.data)
        
        # Go through each group
        for (ngroup in 1:nrow(group.se)) {
          
          # Define inputs required for parallel processing.
          core.input <- makeCluster(3)
          clusterExport(core.input, c("stream.network", "node.data.raw", "edge.data.cur", "edge.data.ref", 
                                      "stream.habitat", "Autocorrelation.d0",
                                      "stream.weight", "passability.weight", "stream_connectivity"))
          clusterEvalQ(core.input, {
            library(igraph)
          })
          
          # Calculate connectivity for each stream in the network
          Cj.temp <- parSapply(core.input, node.data$Stream[group.se[ngroup, 1]:group.se[ngroup, 2]], function(stream.id) stream_connectivity(focus.stream = stream.id, 
                                                                                                                                                  base.network = stream.network, 
                                                                                                                                                  base.node = node.data.raw, 
                                                                                                                                                  ref.edge = edge.data.ref, 
                                                                                                                                                  cur.edge = edge.data.cur, 
                                                                                                                                                  habitat.type = stream.habitat,
                                                                                                                                                  distance.weight = stream.weight,
                                                                                                                                                  probability.weight = passability.weight,
                                                                                                                                                  d0 = Autocorrelation.d0))
          stopCluster(core.input)
          
          # Stitch results together
          if (ngroup == 1) {
            
            Cj <- Cj.temp 
            
          } else {
            
            Cj <- cbind(Cj, Cj.temp)
            
          }
          
          rm(Cj.temp)
          
          print(ngroup)
          
        }
        
      } else {
        
        # Define inputs required for parallel processing.
        core.input <- makeCluster(3)
        clusterExport(core.input, c("stream.network", "node.data.raw", "edge.data.cur", "edge.data.ref", 
                                    "stream.habitat", "Autocorrelation.d0",
                                    "stream.weight", "passability.weight", "stream_connectivity"))
        clusterEvalQ(core.input, {
          library(igraph)
        })
        
        # Calculate connectivity for each stream in the network
        Cj <- parSapply(core.input, node.data$Stream, function(stream.id) stream_connectivity(focus.stream = stream.id, 
                                                                                                  base.network = stream.network, 
                                                                                                  base.node = node.data.raw, 
                                                                                                  ref.edge = edge.data.ref, 
                                                                                                  cur.edge = edge.data.cur, 
                                                                                                  habitat.type = stream.habitat,
                                                                                                  distance.weight = stream.weight,
                                                                                                  probability.weight = passability.weight,
                                                                                                  d0 = Autocorrelation.d0))
        stopCluster(core.input)
        
      }
      
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
      
      rm(Cj.temp, Cj)
      
      ##########################
      # Watershed Connectivity #
      ##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calcualte watershed connectivity for each stream segment within the network
      c.watershed <- watershed_status(filter.node = node.data, connect.status = Cj.formated)
      
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
      
      # Store the results
      write.csv(c.watershed, file = paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year, "/", SSO.ID, "/HUC-", SSO.ID, "_", ID, "-connectivity_", Sys.Date(), ".csv"), row.names = FALSE)
      
      # # For reporting purposes, overall watershed connectivity can be calcualted using the following equation. 
      # # Calculate overall watershed connectivity
      # sum(c.watershed$Numerator) / sum(c.watershed$Denominator) * 100
      
      rm(c.watershed, Cj.formated)
      
    }
    
    print(SSO.ID)
    
  }
  
}
