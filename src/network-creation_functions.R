#
# Title: Functions for creating the stream network
# Created: September 1st, 2021
# Last Updated: July 5th, 2024
# Author: Brandon Allen
# Objectives: Define functions required for ecreating the stream network
# Keywords: Network extraction
# Note: 
#

######################
# Network extraction # Network extraction based on the new culvert passability model
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_extraction <- function(watershed.geodatabase, huc.scale, huc.unit, hfi.year, dam.layer, mineable.boundary, Slope, DEM, MAP, Eref, AT.bridges, NP.rail.bridges, NP.road.bridges, arcpy) {
  
        # Define an increment function for renaming stream segments
        increment_function <- "
rec=0
def autoIncrement():
    global rec
    pStart = 1 #adjust start value, if req'd 
    pInterval = 1 #adjust interval value, if req'd
    if (rec == 0): 
        rec = pStart 
    else: 
        rec = rec + pInterval 
    return rec"
        
        # Define workspace
        arcpy$env$workspace <- watershed.geodatabase
  
        #####################################
        # Clean the Mineable Boundary Layer # 
        #####################################
        
        # The layers have strange artifacts between leases. 
        # Select scheme of interest, buffer the boundary, dissolve, then remove buffer.
        
        # Subset the intersect road/mineable and stream layers
        arcpy$Select_analysis(in_features = mineable.boundary, 
                              out_feature_class = "MinableSubset_temp",
                              where_clause = "\"SCHE_NAME\" NOT IN ('Aurora Mine South', 'Jackpine Mine Expansion', 'Frontier Oil Sands Project')")
        
        arcpy$PairwiseBuffer_analysis(in_features = "MinableSubset_temp", 
                                      out_feature_class = "MinableBuffer_temp", 
                                      buffer_distance_or_field = "15 Meters")
        
        arcpy$PairwiseDissolve_analysis(in_features = "MinableBuffer_temp", 
                                        out_feature_class = "MinableDissolve_temp")
        
        # This negative buffer prevents the issue of the stream segment aligning with the boundary of Jackpine Expansion
        arcpy$PairwiseBuffer_analysis(in_features = "MinableDissolve_temp", 
                                      out_feature_class = "MineableBoundary", 
                                      buffer_distance_or_field = "-25 Meters")
  
        ############################################
        # Identification of Dams on Stream Network #
        ############################################
        
        # Identify Dams within the watershed
        arcpy$PairwiseClip_analysis(in_features = dam.layer, 
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "DamLocationsOriginal_temp")
        
        # Convert stream to point to identify the closest node on the stream network
        arcpy$FeatureVerticesToPoints_management(in_features = "stream_network",
                                                 out_feature_class = "StreamPoints_temp",
                                                 point_location = "BOTH_ENDS")
        
        arcpy$Near_analysis(in_features = "DamLocationsOriginal_temp", 
                            near_features = "StreamPoints_temp", 
                            search_radius = "50 Meters", 
                            location = "LOCATION",
                            method = "Geodesic")
        
        # Identifies all dams that fall on the current stream network
        arcpy$Select_analysis(in_features = "DamLocationsOriginal_temp", 
                              out_feature_class = "DamInclusion_1_temp",
                              where_clause = "\"NEAR_FID\" <> -1")
        
        #############################################
        # Identification of Dams off Stream Network #
        #############################################
        
        arcpy$Select_analysis(in_features = "DamLocationsOriginal_temp", 
                              out_feature_class = "DamSubset_temp",
                              where_clause = "\"NEAR_FID\" = -1")
        
        arcpy$Near_analysis(in_features = "DamSubset_temp", 
                            near_features = "stream_network", 
                            search_radius = "50 Meters", 
                            location = "LOCATION",
                            method = "Geodesic")
        
        arcpy$Select_analysis(in_features = "DamSubset_temp", 
                              out_feature_class ="DamSubset_2_temp",
                              where_clause = "\"NEAR_FID\" <> -1")
        
        arcpy$XYTableToPoint_management(in_table = "DamSubset_2_temp", 
                                        out_feature_class = "DamInclusion_2_temp", 
                                        x_field = "NEAR_X",
                                        y_field = "NEAR_Y")
        
        arcpy$SplitLineAtPoint_management(in_features = "stream_network", 
                                          point_features = "DamInclusion_2_temp", 
                                          out_feature_class = "StreamDam_Segmentation_temp", 
                                          search_radius = "5 Meters")
        
        # Combine the two dam objects into a single file
        arcpy$Merge_management(inputs = "DamInclusion_1_temp; DamInclusion_2_temp",
                               output = "AllDams_temp")
  
        ############################################
        # Identification of Stream/Road Intersects #
        ############################################
  
        # Intersect the road and the mineable boundary
        arcpy$FeatureToLine_management(in_features = paste("MineableBoundary", "road_rail", sep = ";"),
                                       out_feature_class = "RoadsMineable", 
                                       cluster_tolerance = "", 
                                       attributes = "ATTRIBUTES")
  
        # Intersect the road/mineable and stream networks and add length attribute
        arcpy$FeatureToLine_management(in_features = paste("StreamDam_Segmentation_temp", "RoadsMineable", sep = ";"),
                                       out_feature_class = "StreamRoads", 
                                       cluster_tolerance = "", 
                                       attributes = "ATTRIBUTES")
  
        arcpy$AddGeometryAttributes_management(Input_Features = "StreamRoads", 
                                               Geometry_Properties = "LENGTH", 
                                               Length_Unit = "METERS", 
                                               Area_Unit = "", 
                                               Coordinate_System = "")
  
        # Subset the intersect road/minable and stream layers
        arcpy$Select_analysis(in_features = "StreamRoads", 
                              out_feature_class = "StreamSeg",
                              where_clause = "\"FID_RoadsMineable\" = -1")
  
        arcpy$Select_analysis(in_features = "StreamRoads", 
                              out_feature_class = "RoadSeg",
                              where_clause = "\"FID_RoadsMineable\" <> -1")
  
        # Add unique identifier to each stream and road segment in the watershed
        arcpy$AddField_management(in_table = "StreamSeg",
                                  field_name = "StreamID", 
                                  field_type = "DOUBLE")
  
        arcpy$CalculateField_management(in_table = "StreamSeg",
                                        field = "StreamID",
                                        expression = "autoIncrement()",
                                        expression_type = "PYTHON", 
                                        code_block = increment_function)
  
        arcpy$AddField_management(in_table = "RoadSeg",
                                  field_name = "RoadID", 
                                  field_type = "DOUBLE")
        
        arcpy$CalculateField_management(in_table = "RoadSeg",
                                        field = "RoadID",
                                        expression = "autoIncrement()",
                                        expression_type = "PYTHON", 
                                        code_block = increment_function)
  
        # Identify the starting and ending points for the stream segments, and join the connections
        arcpy$FeatureVerticesToPoints_management(in_features = "StreamSeg",
                                                 out_feature_class = "StreamSegNodes_temp",
                                                 point_location = "BOTH_ENDS")
        
        arcpy$FeatureVerticesToPoints_management(in_features = "StreamSeg",
                                                 out_feature_class = "StreamSegStartNodes_temp",
                                                 point_location = "START")
        
        arcpy$FeatureVerticesToPoints_management(in_features = "StreamSeg",
                                                 out_feature_class = "StreamSegEndNodes_temp",
                                                 point_location = "END")
  
        arcpy$SpatialJoin_analysis(target_features = "StreamSegNodes_temp",
                                   join_features = "StreamSegNodes_temp",
                                   out_feature_class = "StreamConnections_temp",
                                   join_operation = "JOIN_ONE_TO_MANY")
        
        arcpy$SpatialJoin_analysis(target_features = "StreamSegStartNodes_temp",
                                   join_features = "StreamSegStartNodes_temp",
                                   out_feature_class = "StreamConnections1_temp",
                                   join_operation = "JOIN_ONE_TO_ONE")
        
        arcpy$SpatialJoin_analysis(target_features = "StreamSegEndNodes_temp",
                                   join_features = "StreamSegEndNodes_temp",
                                   out_feature_class = "StreamConnections2_temp",
                                   join_operation = "JOIN_ONE_TO_ONE")
  
        # Remove Identify the Start, End, and all other intersection points
        arcpy$Select_analysis(in_features = "StreamConnections_temp",
                              out_feature_class = "StreamConnectionsFilter_temp",
                              where_clause = "\"StreamID_1\" > \"StreamID\"")
        
        arcpy$Select_analysis(in_features = "StreamConnections1_temp",
                              out_feature_class = "StreamConnectionsFilter1_temp",
                              where_clause = "\"StreamID_1\" = \"StreamID\"")
        
        arcpy$Select_analysis(in_features = "StreamConnections2_temp",
                              out_feature_class = "StreamConnectionsFilter2_temp",
                              where_clause = "\"StreamID_1\" = \"StreamID\"")
  
        # Filter the Start and End points so we exclude self intersecting points at confluences.
        arcpy$Erase_analysis(in_features = "StreamConnectionsFilter1_temp", 
                             erase_features = "StreamConnectionsFilter_temp", 
                             out_feature_class = "stream_start_points_temp")
        
        arcpy$Erase_analysis(in_features = "StreamConnectionsFilter2_temp", 
                             erase_features = "StreamConnectionsFilter_temp", 
                             out_feature_class = "stream_end_points_temp")
  
        # Merge the Start, End, and other intersections together
        arcpy$Merge_management(inputs = paste("StreamConnectionsFilter_temp", 
                                              "stream_start_points_temp", 
                                              "stream_end_points_temp", sep = ";"),
                               output = "AllStreamConnection")
        
        arcpy$AddField_management(in_table = "AllStreamConnection",
                                  field_name = "InterID", 
                                  field_type = "LONG")
        
        arcpy$CalculateField_management(in_table = "AllStreamConnection",
                                        field = "InterID",
                                        expression = "autoIncrement()",
                                        expression_type = "PYTHON", 
                                        code_block = increment_function)
  
        # Identify the location of culverts
        arcpy$FeatureVerticesToPoints_management(in_features = "RoadSeg",
                                                 out_feature_class = "RoadSegEndNodes_temp",
                                                 point_location = "END")
  
        ######################################################
        # Identification of Culverts and Variable Extraction #
        ######################################################
        
        # Identify culverts
        arcpy$Clip_analysis(in_features = "AllStreamConnection", 
                            clip_features = "RoadSegEndNodes_temp", 
                            out_feature_class = "Culverts_temp")
        
        # Merge with the appropriate footprint information
        arcpy$SpatialJoin_analysis(target_features = "Culverts_temp",
                                   join_features = "RoadSegEndNodes_temp",
                                   out_feature_class = "Culverts",
                                   join_operation = "JOIN_ONE_TO_ONE")
        
        n.culverts <- as.numeric(as.character(arcpy$GetCount_management("Culverts")))
        
        # If there are no culverts in the region, skip the extraction and store only the relevant node information.
        if (n.culverts == 0) {
                
                # We can read in the entire object using SF
                node.data <- read_sf(dsn = watershed.geodatabase,
                                            layer = "StreamSeg")
                
                # Nodes
                node.data <- data.frame(Stream = node.data$StreamID,
                                        SectionLength = node.data$LENGTH,
                                        Watershed = rep(huc.unit, nrow(node.data)),
                                        StreamType = node.data$StrmType,
                                        HabitatType = node.data$Strahler,
                                        HabitatQuality = rep(1, nrow(node.data)))

                # Save the output
                watershed.network <- list(Node = node.data)
                save(watershed.network, file = paste0("data/processed/huc-", huc.scale, 
                                                      "/", hfi.year, "/connectivity/",
                                                      "network_",  huc.unit, ".Rdata"))
                
        } else {
                
                ###############
                # Topographic #
                ###############
                
                # Slope at culvert location
                arcpy$sa$ExtractValuesToPoints(in_point_features = "Culverts", 
                                               in_raster = Slope, 
                                               out_point_features = "SlopePoint_temp", 
                                               interpolate_values = "NONE")
    
                ################
                # Stream slope #
                ################
                
                # Elevation of stream segments used to calculate slope
                arcpy$sa$ExtractValuesToPoints(in_point_features = "AllStreamConnection",
                                               in_raster = DEM,
                                               out_point_features = "Elevation_temp",
                                               interpolate_values = "NONE")
                
                ###########
                # Climate #
                ###########
                
                # Mean annual precipitation
                arcpy$sa$ExtractValuesToPoints(in_point_features = "Culverts", 
                                               in_raster = MAP, 
                                               out_point_features = "MAP_temp", 
                                               interpolate_values = "NONE")
                
                # Evapotranspiration
                arcpy$sa$ExtractValuesToPoints(in_point_features = "Culverts", 
                                               in_raster = Eref, 
                                               out_point_features = "Eref_temp", 
                                               interpolate_values = "NONE")


                #########################
                # Bridge Identification #
                #########################
                
                # Alberta Transportation
                arcpy$SpatialJoin_analysis(target_features = "Culverts",
                                           join_features = AT.bridges,
                                           out_feature_class = "ATBridges",
                                           join_operation = "JOIN_ONE_TO_MANY")
    
                # Rail bridges NP
                arcpy$SpatialJoin_analysis(target_features = "Culverts",
                                           join_features = NP.rail.bridges,
                                           out_feature_class = "NPRailBridges",
                                           join_operation = "JOIN_ONE_TO_MANY")
                
                # Road bridges NP
                arcpy$SpatialJoin_analysis(target_features = "Culverts",
                                           join_features = NP.road.bridges,
                                           out_feature_class = "NPRoadBridges",
                                           join_operation = "JOIN_ONE_TO_MANY")
    
                ######################
                # Dam Identification #
                ###################### 
                
                # AB Transportation Dams
                n.dams <- as.numeric(as.character(arcpy$GetCount_management("AllDams_temp")))
                if(n.dams != 0) {
                        
                        arcpy$SpatialJoin_analysis(target_features = "AllDams_temp",
                                                   join_features = "AllStreamConnection",
                                                   out_feature_class = "Dams",
                                                   join_operation = "JOIN_ONE_TO_ONE", 
                                                   match_option = "WITHIN_A_DISTANCE_GEODESIC",
                                                   search_radius = "50 Meters")
                        
                }

    
                #################################
                # Mineable Region Identification #
                #################################
                
                arcpy$Clip_analysis(in_features = "AllStreamConnection",
                                    clip_features = "MineableBoundary",
                                    out_feature_class = "IntersectMineable")
    
    
                #############################
                # Watershed Characteristics #
                #############################
                
                arcpy$Dissolve_management(in_features = "watershed_boundary",
                                          out_feature_class = "watershed_summaries_temp",
                                          dissolve_field = "HUC_8")
    
                # Area
                arcpy$AddGeometryAttributes_management(Input_Features = "watershed_summaries_temp",
                                                       Geometry_Properties = "AREA",
                                                       Length_Unit = "METERS",
                                                       Area_Unit = "SQUARE_METERS")
                
                # Perimeter
                arcpy$AddGeometryAttributes_management(Input_Features = "watershed_summaries_temp",
                                                       Geometry_Properties = "PERIMETER_LENGTH_GEODESIC",
                                                       Length_Unit = "METERS",
                                                       Area_Unit = "SQUARE_METERS")
    
                # Basin Length
                huc.8.boundaries <- read_sf(dsn = watershed.geodatabase,
                                            layer = "watershed_boundary")
                
                # Double check this is working properly

                for (basin.id in as.character(unique(huc.8.boundaries$HUC_8))) {
                        
                        arcpy$Select_analysis(in_features = "watershed_boundary",
                                              out_feature_class = paste0("watershed_temp_", basin.id),
                                              where_clause = paste0("\"HUC_8\" IN ('", basin.id, "')"))
                        
                        arcpy$Dissolve_management(in_features = paste0("watershed_temp_", basin.id), 
                                                  out_feature_class = paste0("watershed_dissolve_temp_", basin.id), 
                                                  dissolve_field = "HUC_8")
                        
                        arcpy$Intersect_analysis(in_features = paste("StreamSeg", paste0("watershed_dissolve_temp_", basin.id), sep = ";"), 
                                                 out_feature_class = paste0("pour_point_temp_", basin.id),
                                                 join_attributes = "ALL", 
                                                 output_type = "POINT")
                        
                        arcpy$FeatureToPoint_management(in_features = paste0("pour_point_temp_", basin.id), 
                                                        out_feature_class = paste0("pour_point_single_temp_", basin.id))
                        
                        arcpy$sa$ExtractValuesToPoints(in_point_features = paste0("pour_point_single_temp_", basin.id),
                                                       in_raster = DEM,
                                                       out_point_features = paste0("pour_point_elevation_temp_", basin.id),
                                                       interpolate_values = "NONE")
                        
                        arcpy$SpatialJoin_analysis(target_features = paste0("pour_point_elevation_temp_", basin.id),
                                                   join_features = "StreamSeg",
                                                   out_feature_class = paste0("pour_point_id_temp_", basin.id),
                                                   join_operation = "JOIN_ONE_TO_ONE", 
                                                   match_option = "WITHIN_A_DISTANCE_GEODESIC",
                                                   search_radius = "1 Meters")

                }

                #################################
                # Creation on final data frames #
                #################################

                # Alignment of culvert point attributes
                att.type <- c("SlopePoint", "MAP", "Eref")
                
                for (att.id in 1:length(att.type)) {
                        
                        # Read in the table
                        culvert.attribute <- read_sf(dsn = watershed.geodatabase,
                                                    layer = paste0(att.type[att.id], "_temp"))
                        
                        if(att.id == 1) {
                                
                                culvert.df <- data.frame(InterID = culvert.attribute$InterID,
                                                         Attribute = culvert.attribute$RASTERVALU)
                                colnames(culvert.df)[2] <- att.type[att.id]
                                
                        } else {
                                
                                culvert.attribute <- culvert.attribute[, c("InterID", "RASTERVALU")]
                                colnames(culvert.attribute)[2] <- att.type[att.id]
                                culvert.df <- merge.data.frame(culvert.df, culvert.attribute, by = "InterID")
                                
                        }
                        
                        # Delete the temp file
                        rm(culvert.attribute)
                        
                }
    
                # Rename to to allow proper matching between files
                colnames(culvert.df)[1] <- "TARGET_FID"
                
                ################
                # Node summary #
                ################
                
                # Read in the table
                node.data <- read_sf(dsn = watershed.geodatabase,
                                             layer = "StreamSeg")
                
                node.data <- data.frame(Stream = node.data$StreamID,
                                        SectionLength = node.data$LENGTH,
                                        Watershed = rep(huc.unit, nrow(node.data)),
                                        StreamType = node.data$StrmType,
                                        HabitatType = node.data$Strahler,
                                        HabitatQuality = rep(1, nrow(node.data)))
                
                ################
                # Edge summary #
                ################
    
                basin.df <- data.frame(HUC_8 = as.character(unique(huc.8.boundaries$HUC_8)),
                                       PourStream = NA,
                                       PourElevation = NA)
                
                # Merge basin info with the other stream properties
                for (basin.id in as.character(unique(huc.8.boundaries$HUC_8))) {
                        
                        # Read in the table
                        temp.df <- read_sf(dsn = watershed.geodatabase,
                                             layer = paste0("pour_point_id_temp_", basin.id))
                        
                        temp.df <- as.data.frame(temp.df)
                        
                        # Identify min elevation and max strahler (represents the lowest pour point)
                        temp.df <- temp.df[temp.df$RASTERVALU == min(temp.df$RASTERVALU),  ]
                        temp.df <- temp.df[temp.df$Strahler == max(temp.df$Strahler),  ]
                        basin.df[basin.df$HUC_8 == basin.id, c("PourStream", "PourElevation")] <- temp.df[1, c("StreamID", "RASTERVALU")]
                        
                        # Remove object
                        rm(temp.df)
                        
                }
    
                # Add the Feature type, and watershed properties
                arcpy$SpatialJoin_analysis(target_features = "Elevation_temp",
                                           join_features = "watershed_summaries_temp",
                                           out_feature_class = "Segment_temp",
                                           join_operation = "JOIN_ONE_TO_ONE")
                
                # Read in the table
                stream.edges <- read_sf(dsn = watershed.geodatabase,
                                   layer = "Segment_temp")
                
                # Edges
                stream.edges <- as.data.frame(stream.edges)
    
                # Merge the basin info
                stream.edges <- merge.data.frame(stream.edges, basin.df, by = "HUC_8", all = TRUE)
                
                # Format data set
                stream.edges <- data.frame(TARGET_FID = as.numeric(as.character(stream.edges$InterID)),
                                           StreamID_1 = stream.edges$StreamID, # Downstream segment
                                           StreamID_2 = stream.edges$StreamID_1, # Upstream segment
                                           Strahler = stream.edges$Strahler,
                                           Elevation = stream.edges$RASTERVALU,
                                           WatershedArea = stream.edges$POLY_AREA,
                                           WatershedPerm = stream.edges$PERIM_GEO,
                                           PourStream = stream.edges$PourStream,
                                           PourElevation = stream.edges$PourElevation)
    
                # If there is no elevation value, set to NA
                stream.edges$Elevation[stream.edges$Elevation == -9999] <- NA
                
                # Identify which stream segments are culverts or natural splits
                stream.edges$Class <- ifelse(is.element(stream.edges$TARGET_FID, culvert.df$TARGET_FID), "Culvert", "Split")
                stream.edges$Node <- paste0(stream.edges$StreamID_1, "-", stream.edges$StreamID_2)
                stream.edges$Up <- ifelse(stream.edges$Class == "Split", 1, NA)
                stream.edges <- merge(stream.edges, culvert.df, by = "TARGET_FID", all = TRUE)
                
                # Add the Road/Rail feature type
                # Read in the table
                feature.ty <- read_sf(dsn = watershed.geodatabase,
                                      layer = "Culverts")
                
                feature.ty <- as.data.frame(feature.ty)
                feature.ty <- feature.ty[, c("InterID", "FEATURE_TY_12")]
                colnames(feature.ty) <- c("TARGET_FID", "FEATURE_TY")
                
                # Merge the basin info
                stream.edges <- merge.data.frame(stream.edges, feature.ty, by = "TARGET_FID", all = TRUE)
                
                # Correct culverts that are actually bridges and add the date of construction/survey
                
                # Access Layer Road bridges
                # Read in the table
                bridges.df <- read_sf(dsn = watershed.geodatabase,
                                        layer = "NPRoadBridges")
                
                bridges.df <- bridges.df[!is.na(bridges.df$GEO_DATE), ]
                bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
                
                stream.edges$BridgeDate <- NA
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
                
                # Remove temporary object
                rm(bridges.df)
                
                # Access Layer Rail bridges
                # Read in the table
                bridges.df <- read_sf(dsn = watershed.geodatabase,
                                      layer = "NPRailBridges")

                bridges.df <- bridges.df[!is.na(bridges.df$GEO_DATE), ]
                bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
                
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
                
                # Remove temporary file
                rm(bridges.df)
    
                # Alberta Transportation
                # Read in the table
                bridges.df <- read_sf(dsn = watershed.geodatabase,
                                      layer = "ATBridges")

                bridges.df <- bridges.df[bridges.df$STRUCTURE1 %in% c("STANDARD BRIDGE", "MAJOR BRIDGE"), ] # Bridges
                bridges.df <- bridges.df[bridges.df$STRUCTURE2 %in% c("IN SERVICE"), ] # In service
                bridges.df <- bridges.df[, c("InterID", "FIRST_IN_S")]
                
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- as.character(bridges.df$FIRST_IN_S)
                
                # Remove temporary file
                rm(bridges.df)
    
                # Add dam information
                stream.edges$Dam <- stream.edges$DamType <- stream.edges$DamStatus <- NA
                
                # If no dams are present in the area, skip
                if (n.dams != 0) {
                        
                        # Read in the table
                        dam.df <- read_sf(dsn = watershed.geodatabase,
                                          layer = "Dams")
                        
                        dam.df <- dam.df[!is.na(dam.df$ASSET_TYPE), ]
                        
                        stream.edges$Dam[match(dam.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Dam"
                        stream.edges$DamType[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$PURPOSE)
                        stream.edges$DamStatus[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$STATUS)
                        
                        # Remove temporary file
                        rm(dam.df)
                        
                }

                # Correct culverts that fall within the mineable region
                # Create temporary file for reading in table
                # Read in the table
                mineable.df <- read_sf(dsn = watershed.geodatabase,
                                  layer = "IntersectMineable")
                
                stream.edges["MineableRegion"] <- NA
                stream.edges$MineableRegion <- ifelse(stream.edges$TARGET_FID %in% mineable.df$InterID, "Inside", "Outside")
                
                # Remove temporary file
                rm(mineable.df)
    
                # Create the final data frame
                edge.data <- data.frame(TARGET_FID = stream.edges$TARGET_FID,
                                        Node = stream.edges$Node,
                                        UpstreamSeg = stream.edges$StreamID_2,
                                        DownstreamSeg = stream.edges$StreamID_1,
                                        Up = rep(1, nrow(stream.edges)),
                                        Down = rep(1, nrow(stream.edges)),
                                        Class = stream.edges$Class,
                                        FeatureType = stream.edges$FEATURE_TY,
                                        BridgeDate = stream.edges$BridgeDate,
                                        Dam = stream.edges$Dam,
                                        DamStatus = stream.edges$DamStatus,
                                        DamType = stream.edges$DamType,
                                        Mineable = stream.edges$MineableRegion, 
                                        SlopePoint = stream.edges$SlopePoint,
                                        MAP = stream.edges$MAP,
                                        Eref = stream.edges$Eref,
                                        Elevation = stream.edges$Elevation,
                                        WatershedPerm = stream.edges$WatershedPerm,
                                        WatershedArea = stream.edges$WatershedArea,
                                        PourStream = stream.edges$PourStream,
                                        PourElevation = stream.edges$PourElevation)
                
                # Save the output
                watershed.network <- list(Node = node.data, 
                                          Edge = edge.data)
                save(watershed.network, file = paste0("data/processed/huc-", huc.scale, 
                                                      "/", hfi.year, "/connectivity/",
                                                      "network_",  huc.unit, ".Rdata"))
    
        }
  
        ############
        # Clean up #
        ############
        
        temp.layers <- arcpy$ListFeatureClasses()
        temp.layers <- temp.layers[grep("_temp", temp.layers)]
        arcpy$Delete_management(in_data = temp.layers)
        
}

