#
# Title: Functions for calculating connectivity status
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Functions required for calculating stream connectivity
# Keywords: Network Visualization, Stream distance, Stream connectivity, Watershed connectivity
#

#########################
# Network Visualization # Format edge data and construct a network.
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
 
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
        
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}

###################
# Stream distance # Calculate the minimum distance from each stream segment to the highest strahler order in the network
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_distance <- function(focus.stream, base.network, base.node, habitat.type, segment.weight) {
        
        # Calculate the distance between segments
        pw.dist <- t(distances(graph = base.network, v = focus.stream, weights = segment.weight))
        rownames(pw.dist) <- base.node$Stream
        temp.streams <- base.node[pw.dist != "Inf", c("Stream", "SectionLength", "HabitatType")]
        rownames(temp.streams) <- temp.streams$Stream
        temp.streams <- temp.streams[!(rownames(temp.streams) %in% focus.stream), ]
        temp.streams["Distance"] <- pw.dist[rownames(temp.streams), ]
        
        # Create matrix for storing the variables of interest
        variable.matrix <- matrix(nrow = 1,
                                  ncol = 3,
                                  dimnames = list(NULL, c("StreamID", "HabitatType", "Distance")))
        
        if (nrow(temp.streams) == 0) {
                
                variable.matrix[, "StreamID"] <- focus.stream
                
        } else {
                
                variable.matrix[, c("StreamID", "HabitatType")] <- as.numeric(base.node[base.node$Stream == focus.stream, c("Stream", "HabitatType")])
                min.distance <- temp.streams[temp.streams$HabitatType %in% max(temp.streams$HabitatType), "Distance"]
                variable.matrix[, "Distance"] <- ifelse(length(min.distance) == 0, NA, min(min.distance, na.rm = TRUE))
                
        }
        
        
        return(variable.matrix) # Return results to the user
        
}

#######################
# Stream connectivity # Calculates the connectivity status for a defined focal stream segment.
#######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_connectivity <- function(focus.stream, base.network, base.node, ref.edge, cur.edge, habitat.type, distance.weight, probability.weight, d0) {
        
        # Subset network within X Distance from the focal stream of interest and remove focal stream from the list
        pw.dist <- t(distances(graph = base.network, v = focus.stream, weights = distance.weight))
        prob.weight <- t(distances(graph = base.network, v = focus.stream, weights = probability.weight))
        network.filter <- base.node$Stream[pw.dist != Inf] # Filters out segments not in the network (Inf distance)
        network.filter <- network.filter[!(network.filter %in% focus.stream)] 
        pw.dist <- pw.dist[network.filter, ]
        prob.weight <- prob.weight[network.filter, ]
        
        # Create matrix for storing the variables of interest
        variable.matrix <- matrix(nrow = length(network.filter),
                                  ncol = 7,
                                  dimnames = list(NULL, c("StreamID", "Si", "Thetai", "Qi", "Ni", "Pi", "Di")))
        
        # Populate variable matrix with stable values
        variable.matrix[, "StreamID"] <- network.filter # Stream IDs within the network filter
        variable.matrix[, "Si"] <- base.node[match(network.filter, base.node$Stream, nomatch = 0), "SectionLength"] # Section Length
        variable.matrix[, "Thetai"] <- 0 # Default habitat inclusion
        variable.matrix[, "Qi"] <- base.node[match(network.filter, base.node$Stream, nomatch = 0), "HabitatQuality"] # Habitat Quality
        variable.matrix[, "Ni"] <- 1 # Reference cumulative passability based on path between stream segments
        variable.matrix[, "Pi"] <- exp(prob.weight * -1) # Current cumulative passability based on path between stream segments
        variable.matrix[, "Di"] <- (1 / (1 + (pw.dist/d0)^2)) # Distance weighting function using user defined autocorrelation matrix

        # Calculate connectivity for each habitat type within the subset network.
        network.results <- vapply(habitat.type, function(habitat = habitat.type) {
                
                # Adjust habitat type for inclusion
                variable.matrix[, "Thetai"] <- ifelse(base.node[match(network.filter, base.node$Stream, nomatch = 0), "HabitatType"] == habitat, 1, 0) # Habitat Type
                
                # Calculate current and reference connectivity of stream in the context of the surrounidng network
                ANjt <- sum((variable.matrix[,2] * variable.matrix[,3] * variable.matrix[,4] * variable.matrix[,5] * variable.matrix[,7]), na.rm = TRUE)
                APjt <- sum((variable.matrix[,2] * variable.matrix[,3] * variable.matrix[,4] * variable.matrix[,5] * variable.matrix[,6] * variable.matrix[,7]), na.rm = TRUE)
                
                return(c(focus.stream, habitat, ANjt, APjt))
                
        }, FUN.VALUE = numeric(4))
        
        return(network.results) # Return results to the user
        
}

##########################
# Watershed Connectivity # Calculate connectivity status of a watersehd using results created from the stream_connectivity function.
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

watershed_status <- function(filter.node, connect.status) {

        # Calculate mean connectivity score for each stream across habitat types
        connect.status["Connectivity"] <- connect.status$CurConnect / connect.status$RefConnect
        connect.status <- aggregate(x = connect.status[, "Connectivity"], by = list(connect.status$StreamID), function(x) mean(x, na.rm = TRUE))
        colnames(connect.status) <- c("StreamID", "Connectivity")
        
        # Store results
        C.watershed <- NULL
        
        for (j in 1:length(filter.node$Stream)) {
                
                stream.id <- filter.node[j, "Stream"]
                Sj <- filter.node[filter.node$Stream %in% stream.id, "SectionLength"] # Section Length
                
                if (stream.id %in% connect.status$StreamID == TRUE) {
                        
                        C <- connect.status[connect.status$StreamID %in% stream.id, "Connectivity"] # connectivity
                        
                } else {
                        
                        C <- 0
                        
                }
                
                meanQ <- NULL
                
                for (m in unique(filter.node$HabitatType)) {
                        
                        Thetaj <- ifelse(filter.node[filter.node$Stream %in% stream.id, "HabitatType"] == m, 1, 0) # Section Habitat (Only 1 stream order per section)
                        # This is due to  pseudo nodes with passability of 1 being placed at each junction
                        Qj <- filter.node[filter.node$Stream %in% stream.id, "HabitatQuality"] # Section Quality
                        
                        meanQ <- c(meanQ, Thetaj*Qj)
                        
                }
                
                meanQ <- sum(meanQ)
                C.watershed <- rbind(C.watershed, c(stream.id, Sj, C, Sj * C * meanQ, Sj * meanQ))
                
        }
        
        # Standardize output
        C.watershed <- as.data.frame(C.watershed)
        colnames(C.watershed) <- c("StreamID", "StreamLength", "StreamConnect", "Numerator", "Denominator")
        return(as.data.frame(C.watershed))

}
