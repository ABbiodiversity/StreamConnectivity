# ---
# title: "Connectivity Status functions"
# author: "Brandon Allen"
# created: "2025-01-21"
# notes: 
#   "Functions required for calculating stream connectivity."
# ---

#' [Network Visualization]
#'
#' [Creates the igraph network that is used for calculating network properties.]
#'
#' @param [edge.network] [Nodes used to generate the network.]
#' @param [conversion] [If the nodes have both upstream and downstream edges (edge1-edge2), string is split before passed to igraph.]
#' @return [Returns the igraph network object.]
#' 
#' 

network_visualization <- function(edge.network, conversion) {
        
        if (conversion == TRUE) {
 
                edge.network <- unlist(strsplit(as.character(edge.network), "-")) # Split factor/characters as preparation for the network creation.
        
        }
        
        return(graph(as.numeric(edge.network))) # Create network based on edge IDs
        
}

#' [Stream distance]
#'
#' [Calculate the minimum distance from each stream segment to the highest strahler order in the network.]
#'
#' @param [focus.stream] [Target stream ID.]
#' @param [base.network] [Prepared igraph network derived from the network_visualization function.]
#' @param [base.node] [Node data frame.]
#' @param [habitat.type] [Vector of unique habitat types found within the target stream network.]
#' @param [segment.weight] [Length associated with each node (1/2 Upstream + 1/2 Downstream length).]
#' @return [Returns the shortest distance of each stream segment to the target habitat types.]
#' 

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

#' [Stream connectivity]
#'
#' [Calculates the connectivity status for a defined focal stream segment.]
#'
#' @param [focus.stream] [Target stream ID.]
#' @param [base.network] [Prepared igraph network derived from the network_visualization function.]
#' @param [base.node] [Node data frame.]
#' @param [ref.edge] [Edge data frame that assumes all edges are passable.]
#' @param [cur.edge] [Edge data frame that has culvert passability assigned based on current conditions.]
#' @param [habitat.type] [Vector of unique habitat types found within the target stream network.]
#' @param [distance.weight] [Length associated with each node (1/2 Upstream + 1/2 Downstream length).]
#' @param [probability.weight] [-1 * log(probability of passability) for each culvert.]
#' @param [d0] [Autocorrelation distance calculated from 02c_autocorrelation-distance.R]
#' @return [Returns the stream connectivity scores for the target stream segment.]
#' 

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
                
                # Calculate current and reference connectivity of stream in the context of the surrounding network
                ANjt <- sum((variable.matrix[,2] * variable.matrix[,3] * variable.matrix[,4] * variable.matrix[,5] * variable.matrix[,7]), na.rm = TRUE)
                APjt <- sum((variable.matrix[,2] * variable.matrix[,3] * variable.matrix[,4] * variable.matrix[,5] * variable.matrix[,6] * variable.matrix[,7]), na.rm = TRUE)
                
                return(c(focus.stream, habitat, ANjt, APjt))
                
        }, FUN.VALUE = numeric(4))
        
        return(network.results) # Return results to the user
        
}

#' [Connectivity wrapper]
#'
#' [Wrapper function for parallel and matrix processing framework.]
#'
#' @param [huc.scale] [HUC scale.]
#' @param [huc] [HUC watershed unit code.]
#' @param [hfi] [Year of Human Footprint information.]
#' @param [culvert.model] [Defines if the ModelMean, ModelUpper, or ModelLower predicted values are used.]
#' @param [path.in] [Path to Rdata file containing the processed network information.]
#' @return [Returns the processed connectivity scores for each stream segment.]
#' 

connectivity_wrapper <- function(huc.scale, huc, hfi, Autocorrelation.d0, culvert.model = "ModelMean",
                                 path.in = NULL) {
        
        ##################
        # Create Network #
        ##################
        
        if(is.null(path.in)) {
                
                return("User needs to define a path for cleaned objects")
                
        }
        
        # Load the appropriate Rdata with the culvert information
        load(path.in)
        
        # If there are no culverts in the watershed, skip the summary. 
        # This is because we are skipping watersheds where we can assuming connectivity is 100% due to no road/rail infrastructure
        if(is.null(watershed.network[["Edge_Predicted"]])) {
                
                return("No culverts or barriers detected, connectivity assumed to be 100%.")
                
        }
        
        node.data <- watershed.network[["Node_Cleaned"]]
        edge.data <- watershed.network[["Edge_Predicted"]]
        
        # If there is an NA value edge, remove it
        edge.data <- edge.data[!is.na(edge.data$UpstreamSeg), ]
        
        # Create the reference condition where passability is 1
        edge.data.ref <- edge.data
        edge.data.ref$Up <- 1
        
        # Apply the appropriate culvert predictions
        if(!(culvert.model %in% c("ModelMean", "ModelLower", "ModelUpper"))) {
                
                return("Culvert model not defined. ModelMean, ModelLower, ModelUpper, are acceptible values.")
                
        }
        edge.data$Up[edge.data$ModelPassability] <- edge.data[, culvert.model][edge.data$ModelPassability]
        
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
                
                Cj <- stream_connectivity_matrix(focal.streams = node.data.subset$Stream, 
                                                 base.network = stream.network, 
                                                 base.node = node.data, 
                                                 ref.edge = edge.data.ref, 
                                                 cur.edge = edge.data, 
                                                 habitat.type = stream.habitat,
                                                 distance.weight = stream.weight,
                                                 probability.weight = passability.weight,
                                                 d0 = Autocorrelation.d0)
                
                ##########################
                # Watershed Connectivity #
                ##########################
                
                # Calculate watershed connectivity for each stream segment within the network
                c.watershed <- watershed_status(filter.node = node.data.subset, connect.status = Cj)
                
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
        watershed.network[[culvert.model]] <- connectivity
        
        save(watershed.network, file = path.in)
        
}

#' [Stream Connectivity Matrix]
#'
#' [Calculates the connectivity status for a vector of focal stream segments. Same objective as the stream connectivity function but implements a matric multiplication approach]
#'
#' @param [focal.streams] [Vector of streams.]
#' @param [base.network] [Prepared igraph network derived from the network_visualization function.]
#' @param [base.node] [Node data frame.]
#' @param [ref.edge] [Edge data frame that assumes all edges are passable.]
#' @param [cur.edge] [Edge data frame that has culvert passability assigned based on current conditions.]
#' @param [habitat.type] [Vector of unique habitat types found within the target stream network.]
#' @param [distance.weight] [Length associated with each node (1/2 Upstream + 1/2 Downstream length).]
#' @param [probability.weight] [-1 * log(probability of passability) for each culvert.]
#' @param [d0] [Autocorrelation distance calculated from 02c_autocorrelation-distance.R]
#' @return [Returns the stream connectivity scores for all stream segments.]
#' 

stream_connectivity_matrix <- function(focal.streams, base.network, base.node, ref.edge, cur.edge, habitat.type, distance.weight, probability.weight, d0) {
        
        # Subset network within X Distance from the focal stream of interest and remove focal stream from the list
        pw.dist <- t(distances(graph = base.network, v = focal.streams, weights = distance.weight))
        prob.weight <- t(distances(graph = base.network, v = focal.streams, weights = probability.weight))
        network.filter <- base.node$Stream[pw.dist[,1] != Inf] # Filters out segments not in the network (Inf distance)
        pw.dist <- pw.dist[network.filter, ]
        prob.weight <- prob.weight[network.filter, ]
        
        # Define the variables of interest (vectors and matrices)
        StreamID <- network.filter # Stream IDs within the network filter
        Si <- base.node[match(network.filter, base.node$Stream, nomatch = 0), "SectionLength"] # Section Length
        Ni <- 1 # Reference cumulative passability based on path between stream segments
        Qi <- base.node[match(network.filter, base.node$Stream, nomatch = 0), "HabitatQuality"] # Habitat Quality (assumed 1)
        
        Thetai <- matrix(data = 0, nrow = length(StreamID), ncol = length(StreamID)) # Default habitat inclusion
        Pi <- exp(prob.weight * -1) # Current cumulative passability based on path between stream segments
        Di <- (1 / (1 + (pw.dist/d0)^2)) # Distance weighting function using user defined autocorrelation matrix
        
        # Define matrix for storing results
        stream.results <- matrix(nrow = length(StreamID),
                          ncol = 4, dimnames = list(1:length(StreamID),
                                                    c("StreamID", "HabitatType", "RefConnect", "CurConnect")))
        
        # Calculate connectivity for each habitat type within the subset network.
        network.results <- lapply(habitat.type, function(habitat = habitat.type) {
                
                # Adjust habitat type for inclusion
                Theta <- ifelse(base.node[match(network.filter, base.node$Stream, nomatch = 0), "HabitatType"] == habitat, 1, 0) # Habitat Type
                Thetai <- matrix(data = Theta, nrow = length(StreamID), 
                                 ncol = length(StreamID), byrow = FALSE)
                
                # Define the diagonal as 0 since we only want the connectivity of the focal stream
                # to other stream segments. We don't care about the connectivity to itself
                diag(Thetai) <- 0

                # Calculate current and reference connectivity of stream in the context of the surrounding network
                ANjt <- colSums((Si * Thetai * Qi * Ni * Di), na.rm = TRUE)
                APjt <- colSums((Si * Thetai * Qi * Ni * Pi * Di), na.rm = TRUE)
                
                stream.results[, "StreamID"] <- StreamID
                stream.results[, "HabitatType"] <- habitat
                stream.results[, "RefConnect"] <- ANjt
                stream.results[, "CurConnect"] <- APjt
                
                return(stream.results)
                
        })
        
        # Bind matrices convert to data frame
        network.results <- as.data.frame(do.call(rbind, network.results))
        
        return(network.results) # Return results to the user
        
}

#' [Watershed status]
#'
#' [Calculate connectivity status of a watershed using results created from the stream_connectivity function.]
#'
#' @param [filter.node] [Subset of streams that are in the network being processed.]
#' @param [connect.status] [Stream connectivity scores processed by the stream_connectivity_matrix function.]
#' @return [Returns the processed connectivity score for the watershed.]
#' 

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
