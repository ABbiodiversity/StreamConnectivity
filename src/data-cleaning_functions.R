#
# Title: Functions for cleaning the base GIS data
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Functions required for extracting stream length, order, intersections (culverts) with the roads layer, and identification of intersections as bridges.
# Keywords: Stream Standardization, Network extraction, Linear Feature subsetting, Stream Slope, Upstream Distance
# Note: 
#

##########################
# Stream standardization # Creates a singular stream network for the province with standardized attributes
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_standardization <- function(stream.list, workspace, arcpy) {
                
        #####################################
        # Standardization of Stream Network #
        #####################################
        
        for (unique.stream in names(stream.list$stream_layer)) {
                
                # Copy Strahler Stream Order layer, delete unnecessary fields
                arcpy$Copy_management(in_data = stream.list$stream_layer[[unique.stream]], 
                                      out_data = paste0(workspace, 
                                                        "scratch/", 
                                                        unique.stream, 
                                                        ".shp"))
                
                # Identify fields within the layer to be removed
                field.list <- arcpy$ListFields(dataset = paste0(workspace,
                                                                "scratch/", 
                                                                unique.stream, 
                                                                ".shp"),
                                               field_type = "All")
                field.list <- unlist(lapply(field.list, function(x) x$name))
                field.list <- field.list[!field.list %in% c("FID", "Shape")] # Excluding the FID and shape fields
                
                # Rename attributes that are to be maintained
                sso.rename <- c("StrmType", "StrmLength", "Strahler")
                sso.type <- c("TEXT", "Double", "Double")
                
                for(field.name in 1:3) {
                        
                        # Rename attribute table names for consistency
                        arcpy$AddField_management(in_table = paste0(workspace, 
                                                                    "scratch/", 
                                                                    unique.stream, 
                                                                    ".shp"),
                                                  field_name = sso.rename[field.name], 
                                                  field_type = sso.type[field.name])
                        
                        arcpy$CalculateField_management(in_table = paste0(workspace, 
                                                                          "scratch/", 
                                                                          unique.stream, 
                                                                          ".shp"),
                                                        field = sso.rename[field.name],
                                                        expression = paste("!", stream.list$column_id[[unique.stream]][field.name + 2], "!", sep = ""),
                                                        expression_type = "PYTHON")
                        
                }
                
                # Remove all remaining attributes
                arcpy$DeleteField_management(in_table = paste0(workspace, "scratch/", unique.stream, ".shp"), 
                                             drop_field = field.list)
                
        }
        
        # Create the merged layer
        stream.layers <- list.files(paste0(workspace, "scratch/"), full.names = TRUE)
        stream.layers <- stream.layers[grep(".shp", stream.layers)]
        stream.layers <- stream.layers[-grep(".shp.xml", stream.layers)]
        
        arcpy$Merge_management(inputs = paste(stream.layers, collapse = ";"), 
                               output = paste0(workspace,
                                               "cleaned-network/stream_network_merged.shp"))
        
        # Remove all temporary files
        do.call(file.remove, list(list.files(paste0(workspace, "scratch/"), full.names = TRUE)))
                
}

#############################
# Linear Feature subsetting # Creates subsets for the stream and road/rail linear features for user defined regions
#############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linearfeature_subsetting <- function(watershed.layer, road.layer, rail.layer, stream.layer, hfi.year, watershed.lookup, huc.scale, arcpy) {
        
        # If the merged roadrail-centerlines exists for the HFI, skip.
        if (!file.exists(paste0(getwd(), "/data/base/gis/roadrail-centerlines/", hfi.year, "/roadrail_centerlines_", hfi.year, ".shp"))) {
                
                arcpy$Merge_management(inputs = paste(road.layer, rail.layer, sep = ";"), 
                                       output = paste0(getwd(), "/data/base/gis/roadrail-centerlines/", 
                                                       hfi.year, "/roadrail_centerlines_", hfi.year, ".shp"))
                
        }
        
        # For each watershed in the lookup, create the appropriate stream and road/rail subsets
        for (HUC in watershed.lookup) {
                
                # Create geodatabase
                arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/huc-", 
                                                                        huc.scale, "/", hfi.year, "/gis/"), 
                                               out_name = paste0(HUC, ".gdb"))
                
                # Define workspace
                arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                              hfi.year, "/gis/", HUC, ".gdb")
                
                # Define the where clause for including watersheds
                where.clause <- paste0("\"HUC_", huc.scale, "\" IN ('", HUC, "')")

                # Create a watershed mask
                arcpy$Select_analysis(in_features = paste0(getwd(), "/", watershed.layer),
                                      out_feature_class = paste0("watershed_boundary"), 
                                      where_clause = where.clause)
                
                # Clip the road and rail centerlines
                arcpy$PairwiseClip_analysis(in_features = paste0(getwd(), "/data/base/gis/roadrail-centerlines/", 
                                                                 hfi.year, 
                                                                 "/roadrail_centerlines_", 
                                                                 hfi.year, ".shp"), 
                                            clip_features = "watershed_boundary", 
                                            out_feature_class = "road_rail")
                
                # Clip the stream network
                arcpy$PairwiseClip_analysis(in_features = paste0(getwd(), "/data/base/gis/", 
                                                                 "strahler_stream_order/cleaned-network/stream_network_merged.shp"), 
                                            clip_features = "watershed_boundary", 
                                            out_feature_class = "stream_network")
                
        }
        
}

######################
# Network extraction # Network extraction based on the new culvert passability model
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_extraction <- function(watershed.geodatabase, HUC, dam.layer, mineable.boundary, Slope, DEM, MAP, Eref, AT.bridges, NP.rail.bridges, NP.road.bridges, arcpy) {
  
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
                
                # As we can't directly read the table from the geodatabase, we simply write it to a scratch
                # directory and import using the foreign package
                streamseg.temp <- as.character(arcpy$TableSelect_analysis(in_table = "StreamSeg", 
                                                                          out_table = "StreamSeg_temp.shp"))
                
                # Nodes
                node.data  <- read.dbf(streamseg.temp)
                node.data <- data.frame(Stream = node.data$StreamID,
                                        SectionLength = node.data$LENGTH,
                                        Watershed = rep(HUC, nrow(node.data)),
                                        StreamType = node.data$StrmType,
                                        HabitatType = node.data$Strahler,
                                        HabitatQuality = rep(1, nrow(node.data)))
                
                # Delete the temp file
                arcpy$Delete_management(streamseg.temp)
                
                return(list(node.data))
                
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
                arcpy$SpatialJoin_analysis(target_features = "AllDams_temp",
                                           join_features = "AllStreamConnection",
                                           out_feature_class = "Dams",
                                           join_operation = "JOIN_ONE_TO_ONE", 
                                           match_option = "WITHIN_A_DISTANCE_GEODESIC",
                                           search_radius = "50 Meters")
    
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
                watershed.temp <- as.character(arcpy$TableSelect_analysis(in_table = "watershed_boundary", 
                                                                          out_table = "watershed_boundary_temp.shp"))
                
                huc.8.boundaries  <- read.dbf(watershed.temp)
                
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
    
                # Delete the temp file
                arcpy$Delete_management(watershed.temp)

                #################################
                # Creation on final data frames #
                #################################

                # Alignment of culvert point attributes
                att.type <- c("SlopePoint", "MAP", "Eref")
                
                for (att.id in 1:length(att.type)) {
                        
                        # Create temporary file for reading in table
                        attribute.temp <- as.character(arcpy$TableSelect_analysis(in_table = paste0(att.type[att.id], "_temp"), 
                                                                                  out_table = paste0(att.type[att.id], "_temp.shp")))
                        
                        culvert.attribute  <- read.dbf(attribute.temp)
                        
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
                        arcpy$Delete_management(attribute.temp)
                        
                }
    
                # Rename to to allow proper matching between files
                colnames(culvert.df)[1] <- "TARGET_FID"
                
                ################
                # Node summary #
                ################
                
                # Create temporary file for reading in table
                node.temp <- as.character(arcpy$TableSelect_analysis(in_table = "StreamSeg", 
                                                                     out_table = "StreamSeg.shp"))
                
                node.data  <- read.dbf(node.temp)
                node.data <- data.frame(Stream = node.data$StreamID,
                                        SectionLength = node.data$LENGTH,
                                        Watershed = rep(HUC, nrow(node.data)),
                                        StreamType = node.data$StrmType,
                                        HabitatType = node.data$Strahler,
                                        HabitatQuality = rep(1, nrow(node.data)))
                
                # Remove temporary file
                arcpy$Delete_management(node.temp)
                
                ################
                # Edge summary #
                ################
    
                basin.df <- data.frame(HUC_8 = as.character(unique(huc.8.boundaries$HUC_8)),
                                       PourStream = NA,
                                       PourElevation = NA)
                
                # Merge basin info with the other stream properties
                for (basin.id in as.character(unique(huc.8.boundaries$HUC_8))) {
                        
                        # Create temporary file for reading in table
                        basin.temp <- as.character(arcpy$TableSelect_analysis(in_table = paste0("pour_point_id_temp_", basin.id), 
                                                                             out_table = paste0(basin.id, "_pour_point_id.shp")))
                        
                        temp.df <- read.dbf(basin.temp)
                        
                        # Identify min elevation and max strahler (represents the lowlest pour point)
                        temp.df <- temp.df[temp.df$RASTERVALU == min(temp.df$RASTERVALU),  ]
                        temp.df <- temp.df[temp.df$Strahler == max(temp.df$Strahler),  ]
                        basin.df[basin.df$HUC_8 == basin.id, c("PourStream", "PourElevation")] <- temp.df[, c("StreamID", "RASTERVALU")]
                        
                        # Remove temporary file
                        arcpy$Delete_management(basin.temp)
                }
    
                # Add the Feature type, and watershed properties
                arcpy$SpatialJoin_analysis(target_features = "Elevation_temp",
                                           join_features = "watershed_summaries_temp",
                                           out_feature_class = "Segment_temp",
                                           join_operation = "JOIN_ONE_TO_ONE")
                
                # Create temporary file for reading in table
                edge.temp <- as.character(arcpy$TableSelect_analysis(in_table = "Segment_temp", 
                                                                      out_table = "Segment_temp.shp"))
    
                # Edges
                stream.edges <- read.dbf(edge.temp)
                
                # Remove temporary file
                arcpy$Delete_management(edge.temp)
    
                # Merge the basin info
                stream.edges <- merge.data.frame(stream.edges, basin.df, by = "HUC_8")
                
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
    
                # Correct culverts that are actually bridges and add the date of construction/survey
                
                # Access Layer Road bridges
                # Create temporary file for reading in table
                bridge.temp <- as.character(arcpy$TableSelect_analysis(in_table = "NPRoadBridges", 
                                                                       out_table = "NPRoadBridges.shp"))
                bridges.df <- read.dbf(bridge.temp)
                bridges.df <- bridges.df[!is.na(bridges.df$GEO_DATE), ]
                bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
                
                stream.edges$BridgeDate <- NA
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
                
                # Remove temporary file
                arcpy$Delete_management(bridge.temp)
                
                # Access Layer Rail bridges
                # Create temporary file for reading in table
                bridge.temp <- as.character(arcpy$TableSelect_analysis(in_table = "NPRailBridges", 
                                                                       out_table = "NPRailBridges.shp"))
                bridges.df <- read.dbf(bridge.temp)
                bridges.df <- bridges.df[!is.na(bridges.df$GEO_DATE), ]
                bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
                
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
                
                # Remove temporary file
                arcpy$Delete_management(bridge.temp)
    
                # Alberta Transportation
                # Create temporary file for reading in table
                bridge.temp <- as.character(arcpy$TableSelect_analysis(in_table = "ATBridges", 
                                                                       out_table = "ATBridges.shp"))
                bridges.df <- read.dbf(bridge.temp)
                bridges.df <- bridges.df[bridges.df$STRUCTURE1 %in% c("STANDARD BRIDGE", "MAJOR BRIDGE"), ] # Bridges
                bridges.df <- bridges.df[bridges.df$STRUCTURE2 %in% c("IN SERVICE"), ] # In service
                bridges.df <- bridges.df[, c("InterID", "FIRST_IN_S")]
                
                stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
                stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- as.character(bridges.df$FIRST_IN_S)
                
                # Remove temporary file
                arcpy$Delete_management(bridge.temp)
    
                # Add dam information
                # Create temporary file for reading in table
                dam.temp <- as.character(arcpy$TableSelect_analysis(in_table = "Dams", 
                                                                       out_table = "Dams.shp"))
                dam.df <- read.dbf(dam.temp)
                dam.df <- dam.df[!is.na(dam.df$ASSET_TYPE), ]
                
                stream.edges$Dam <- stream.edges$DamType <- stream.edges$DamStatus <- NA
                stream.edges$Dam[match(dam.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Dam"
                stream.edges$DamType[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$PURPOSE)
                stream.edges$DamStatus[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$STATUS)
                
                # Remove temporary file
                arcpy$Delete_management(dam.temp)
    
                # Correct culverts that fall within the mineable region
                # Create temporary file for reading in table
                mineable.temp <- as.character(arcpy$TableSelect_analysis(in_table = "IntersectMineable", 
                                                                    out_table = "IntersectMineable.shp"))
                mineable.df <- read.dbf(mineable.temp)
                stream.edges["MineableRegion"] <- NA
                stream.edges$MineableRegion <- ifelse(stream.edges$TARGET_FID %in% mineable.df$InterID, "Inside", "Outside")
                
                # Remove temporary file
                arcpy$Delete_management(mineable.temp)
    
                # Create the final data frame
                edge.data <- data.frame(TARGET_FID = stream.edges$TARGET_FID,
                                        Node = stream.edges$Node,
                                        UpstreamSeg = stream.edges$StreamID_2,
                                        DownstreamSeg = stream.edges$StreamID_1,
                                        Up = rep(1, nrow(stream.edges)),
                                        Down = rep(1, nrow(stream.edges)),
                                        Class = stream.edges$Class,
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
    
        }
  
        ############
        # Clean up #
        ############
        
        temp.layers <- arcpy$ListFeatureClasses()
        temp.layers <- temp.layers[grep("_temp", temp.layers)]
        arcpy$Delete_management(in_data = temp.layers)
        
        return(list(Node = node.data, 
                    Edge = edge.data))
  
}

#################
# Stream Slopes # Calculates the stream slope (confluence or reach) using the base edge and node files
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    
    # Identify segments directly upstream*
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

#####################
# Upstream Distance # Calculates total upstream distance from any point and repairs the stream network
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

upstream_distance <- function(edge.network, node.network, culvert.id) {
  
  require(igraph)
  
  # Create the stream network from the broken network
  broken.network <- network_visualization(edge.network = edge.network[edge.network$Node != culvert.id, "Node"], 
                                          conversion = TRUE) 
  
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
