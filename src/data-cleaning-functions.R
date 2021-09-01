#
# Title: Functions for cleaning the base GIS data
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Functions required for extracting stream length, order, intersections (culverts) with the roads layer, and identification of intersections as bridges.
# Keywords: Network extraction, Stream Standardization, Linear Feature subsetting, Stream Slope, Upstream Distance
# Note: 
#

######################
# Network extraction # Network extraction based on the new culvert passability model
######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

network_extraction <- function(stream.layer, road.layer, dam.layer, HUC, mineable.boundary, relative.path) {
  
  require(foreign)
  require(raster)
  require(rgdal)
  require(RPyGeo)
  require(sf)
  
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
  
  ####################################
  # Clean the Minable Boundary Layer # 
  ####################################
  
  # The layers have strange artifacts between leases. 
  # Select scheme of interest, buffer the boundary, dissolve, then remove buffer.
  
  # Subset the intersect road/minable and stream layers
  
  arcpy$Select_analysis(in_features = mineable.boundary, 
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/MinableSubset.shp"),
                        where_clause = "\"SCHE_NAME\" NOT IN ('Aurora Mine South', 'Jackpine Mine Expansion', 'Frontier Oil Sands Project')")
  
  arcpy$Buffer_analysis(in_features = paste0(relative.path, "temporary-shapefiles/MinableSubset.shp"), 
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/MinableBuffer.shp"), 
                        buffer_distance_or_field = "15 Meters")
  
  arcpy$Dissolve_management(in_features = paste0(relative.path, "temporary-shapefiles/MinableBuffer.shp"), 
                            out_feature_class = paste0(relative.path, "temporary-shapefiles/MinableDissolve.shp"))
  
  # This negative buffer prevents the issue of the stream segment aligning with the boundary of Jackpine Expansion
  arcpy$Buffer_analysis(in_features = paste0(relative.path, "temporary-shapefiles/MinableDissolve.shp"), 
                        out_feature_class = paste0(relative.path, "MinableBoundary.shp"), 
                        buffer_distance_or_field = "-25 Meters")
  
  ############################################
  # Identification of Dams on Stream Network #
  ############################################
  
  # Identify Dams within the watershed
  arcpy$Clip_analysis(in_features = dam.layer, 
                      clip_features = paste0("data/base/gis/watersheds/huc-6/", HUC, "_watershed.shp"), 
                      out_feature_class = paste0(relative.path, "temporary-shapefiles/DamLocationsOriginal.shp"))
  
  # Convert stream to point to identify the closest node on the stream network
  arcpy$FeatureVerticesToPoints_management(in_features = stream.layer,
                                           out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamPoints.shp"),
                                           point_location = "BOTH_ENDS")
  
  arcpy$Near_analysis(in_features = paste0(relative.path, "temporary-shapefiles/DamLocationsOriginal.shp"), 
                      near_features = paste0(relative.path, "temporary-shapefiles/StreamPoints.shp"), 
                      search_radius = "50 Meters", 
                      location = "LOCATION",
                      method = "Geodesic")
  
  # Identifies all dams that fall on the current stream network
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/DamLocationsOriginal.shp"), 
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/DamInclusion_1.shp"),
                        where_clause = "\"NEAR_FID\" <> -1")
  
  #############################################
  # Identification of Dams off Stream Network #
  #############################################
  
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/DamLocationsOriginal.shp"), 
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/DamSubset.shp"),
                        where_clause = "\"NEAR_FID\" = -1")
  
  arcpy$Near_analysis(in_features = paste0(relative.path, "temporary-shapefiles/DamSubset.shp"), 
                      near_features = stream.layer, 
                      search_radius = "50 Meters", 
                      location = "LOCATION",
                      method = "Geodesic")
  
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/DamSubset.shp"), 
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/DamSubset_2.shp"),
                        where_clause = "\"NEAR_FID\" <> -1")
  
  arcpy$MakeXYEventLayer_management(table = paste0(relative.path, "temporary-shapefiles/DamSubset_2.dbf"), 
                                    in_x_field = "NEAR_X", 
                                    in_y_field = "NEAR_Y", 
                                    out_layer = "DamLayer")
  
  arcpy$SaveToLayerFile_management(in_layer = "DamLayer",
                                   out_layer = paste0(relative.path, "temporary-shapefiles/DamSubset_3.lyr"))
  
  arcpy$CopyFeatures_management(in_features = paste0(relative.path, "temporary-shapefiles/DamSubset_3.lyr"), 
                                out_feature_class = paste0(relative.path, "temporary-shapefiles/DamInclusion_2.shp"))
  
  arcpy$SplitLineAtPoint_management(in_features = stream.layer, 
                                    point_features = paste0(relative.path, "temporary-shapefiles/DamInclusion_2.shp"), 
                                    out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamDam_Segmentation.shp"), 
                                    search_radius = "5 Meters")
  
  # Combine the two dam objects into a single file
  arcpy$Merge_management(inputs = paste(paste0(relative.path, "temporary-shapefiles/DamInclusion_1.shp"), paste0(relative.path, "temporary-shapefiles/DamInclusion_2.shp"), sep = ";"), 
                         output = paste0(relative.path, "temporary-shapefiles/AllDams.shp"))
  
  ############################################
  # Identification of Stream/Road Intersects #
  ############################################
  
  # Intersect the road and the minable boundary
  arcpy$FeatureToLine_management(in_features = c(paste(paste0(relative.path, "MinableBoundary.shp"), road.layer, sep = ";")),
                                 out_feature_class = paste0(relative.path, "RoadsMinable.shp"), 
                                 cluster_tolerance = "", 
                                 attributes = "ATTRIBUTES")
  
  # Intersect the road/minable and stream networks and add length attribute
  arcpy$FeatureToLine_management(in_features = c(paste(paste0(relative.path, "temporary-shapefiles/StreamDam_Segmentation.shp"), paste0(relative.path, "RoadsMinable.shp"), sep = ";")),
                                 out_feature_class = paste0(relative.path, "StreamRoads.shp"), 
                                 cluster_tolerance = "", 
                                 attributes = "ATTRIBUTES")
  
  arcpy$AddGeometryAttributes_management(Input_Features = paste0(relative.path, "StreamRoads.shp"), 
                                         Geometry_Properties = "LENGTH", 
                                         Length_Unit = "METERS", 
                                         Area_Unit = "", 
                                         Coordinate_System = "")
  
  # Subset the intersect road/minable and stream layers
  arcpy$Select_analysis(in_features = paste0(relative.path, "StreamRoads.shp"), 
                        out_feature_class = paste0(relative.path, "StreamSeg.shp"),
                        where_clause = "\"FID_RoadsM\" = -1")
  
  arcpy$Select_analysis(in_features = paste0(relative.path, "StreamRoads.shp"), 
                        out_feature_class = paste0(relative.path, "RoadSeg.shp"),
                        where_clause = "\"FID_RoadsM\" <> -1")
  
  # Add unique identifier to each stream and road segment in the watershed
  arcpy$AddField_management(in_table = paste0(relative.path, "StreamSeg.shp"),
                            field_name = "StreamID", 
                            field_type = "DOUBLE")
  
  arcpy$CalculateField_management(in_table = paste0(relative.path, "StreamSeg.shp"),
                                  field = "StreamID",
                                  expression = "autoIncrement()",
                                  expression_type = "PYTHON", 
                                  code_block = increment_function)
  
  arcpy$AddField_management(in_table = paste0(relative.path, "RoadSeg.shp"),
                            field_name = "RoadID", 
                            field_type = "DOUBLE")
  
  arcpy$CalculateField_management(in_table = paste0(relative.path, "RoadSeg.shp"),
                                  field = "RoadID",
                                  expression = "autoIncrement()",
                                  expression_type = "PYTHON", 
                                  code_block = increment_function)
  
  # Identify the starting and ending points for the stream segments, and join the connections
  arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamSeg.shp"),
                                           out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamSegNodes.shp"),
                                           point_location = "BOTH_ENDS")
  
  arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamSeg.shp"),
                                           out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamSegStartNodes.shp"),
                                           point_location = "START")
  
  arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamSeg.shp"),
                                           out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamSegEndNodes.shp"),
                                           point_location = "END")
  
  arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/StreamSegNodes.shp"),
                             join_features = paste0(relative.path, "temporary-shapefiles/StreamSegNodes.shp"),
                             out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnections.shp"),
                             join_operation = "JOIN_ONE_TO_MANY")
  
  arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/StreamSegStartNodes.shp"),
                             join_features = paste0(relative.path, "temporary-shapefiles/StreamSegStartNodes.shp"),
                             out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnections1.shp"),
                             join_operation = "JOIN_ONE_TO_ONE")
  
  arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/StreamSegEndNodes.shp"),
                             join_features = paste0(relative.path, "temporary-shapefiles/StreamSegEndNodes.shp"),
                             out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnections2.shp"),
                             join_operation = "JOIN_ONE_TO_ONE")
  
  # Remove Identify the Start, End, and all other intersection points
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/StreamConnections.shp"),
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter.shp"),
                        where_clause = "\"StreamID_1\" > \"StreamID\"")
  
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/StreamConnections1.shp"),
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter1.shp"),
                        where_clause = "\"StreamID_1\" = \"StreamID\"")
  
  arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/StreamConnections2.shp"),
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter2.shp"),
                        where_clause = "\"StreamID_1\" = \"StreamID\"")
  
  # Filter the Start and End points so we exclude self intersecting points at confluences.
  arcpy$Erase_analysis(in_features = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter1.shp"), 
                       erase_features = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter.shp"), 
                       out_feature_class = paste0(relative.path, "temporary-shapefiles/stream_start_points.shp"))
  
  arcpy$Erase_analysis(in_features = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter2.shp"), 
                       erase_features = paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter.shp"), 
                       out_feature_class = paste0(relative.path, "temporary-shapefiles/stream_end_points.shp"))
  
  # Merge the Start, End, and other intersections together
  arcpy$Merge_management(inputs = paste(paste0(relative.path, "temporary-shapefiles/StreamConnectionsFilter.shp"), 
                                        paste0(relative.path, "temporary-shapefiles/stream_start_points.shp"), 
                                        paste0(relative.path, "temporary-shapefiles/stream_end_points.shp"), sep = ";"),
                         output = paste0(relative.path, "AllStreamConnection.shp"))
  
  arcpy$AddField_management(in_table = paste0(relative.path, "AllStreamConnection.shp"),
                            field_name = "InterID", 
                            field_type = "LONG")
  
  arcpy$CalculateField_management(in_table = paste0(relative.path, "AllStreamConnection.shp"),
                                  field = "InterID",
                                  expression = "autoIncrement()",
                                  expression_type = "PYTHON", 
                                  code_block = increment_function)
  
  # Identify the location of culverts
  arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "RoadSeg.shp"),
                                           out_feature_class = paste0(relative.path, "temporary-shapefiles/RoadSegEndNodes.shp"),
                                           point_location = "END")
  
  # Filter information except the Feature type
  field.list <- arcpy$ListFields(dataset = paste0(relative.path, "temporary-shapefiles/RoadSegEndNodes.shp"),
                                 field_type = "All")
  field.list <- unlist(lapply(field.list, function(x) x$name))
  field.list <- field.list[!field.list %in% c("FID", "Shape", "FEATURE_TY")] # Excluding the FID and shape fields
  
  # Remove all remaining attributes
  arcpy$DeleteField_management(in_table = paste0(relative.path, "temporary-shapefiles/RoadSegEndNodes.shp"), 
                               drop_field = field.list)
  
  ######################################################
  # Identification of Culverts and Variable Extraction #
  ######################################################
  
  # Identify culverts
  arcpy$Clip_analysis(in_features = paste0(relative.path, "AllStreamConnection.shp"), 
                      clip_features = paste0(relative.path, "temporary-shapefiles/RoadSegEndNodes.shp"), 
                      out_feature_class = paste0(relative.path, "temporary-shapefiles/CulvertsTemp.shp"))
  
  # Merge with the appropriate footprint information
  arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/CulvertsTemp.shp"),
                             join_features = paste0(relative.path, "temporary-shapefiles/RoadSegEndNodes.shp"),
                             out_feature_class = paste0(relative.path, "Culverts.shp"),
                             join_operation = "JOIN_ONE_TO_ONE")
  
  n.culverts <- read.dbf(paste0(relative.path, "Culverts.dbf"))
  
  # If there are no culverts in the region, skip the extraction and store only the relevant node information.
  if (nrow(n.culverts) == 0) {
    
    # Nodes
    node.data  <- read.dbf(paste0(relative.path, "StreamSeg.dbf"))
    node.data <- data.frame(
      Stream = node.data$StreamID,
      SectionLength = node.data$LENGTH,
      Watershed = rep(HUC, nrow(node.data)),
      StreamType = node.data$StrmType,
      HabitatType = node.data$Strahler,
      HabitatQuality = rep(1, nrow(node.data))
    )
    
    return(list(node.data))
    
  } else {
    
    ###############
    # Topographic #
    ###############
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "G:/Shared drives/ABMI_LUS_Submissions/LoticConnectivity/TPI500_LiDAR.tif", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/TPI_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "G:/Shared drives/ABMI_LUS_Submissions/LoticConnectivity/VBF_LIDAR.tif", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/VBF_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "G:/Shared drives/ABMI_LUS_Submissions/LoticConnectivity/Slope_LiDAR.tif", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", "Slope_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "G:/Shared drives/ABMI_LUS_Submissions/LoticConnectivity/SWI_LiDAR.tif", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/TWI_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "G:/Shared drives/ABMI_LUS_Submissions/LoticConnectivity/HAND1_SRTM.tif", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/HAND_Point.shp"), 
                                   interpolate_values = "NONE")
    
    ################
    # Stream slope #
    ################
    
    # Confluence bound
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "AllStreamConnection.shp"),
                                   in_raster = "//gisserver.abmi.ca/GIS/Terrain/DEM_SRTM/GEE_srtm_mosaic/srtm.tif",
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/Confluence_Elevation.shp"),
                                   interpolate_values = "NONE")
    
    # Reach bound
    
    # Create buffer to identify slope around culvert
    arcpy$Buffer_analysis(in_features = paste0(relative.path, "Culverts.shp"),
                          out_feature_class = paste0(relative.path, "temporary-shapefiles/culvert_150.shp"),
                          buffer_distance_or_field = "150 Meters",
                          dissolve_option = "NONE",
                          method = "GEODESIC")
    
    # Intersect boundary of buffer with stream network to identify bounds of the reach
    arcpy$Intersect_analysis(in_features = paste(paste0(relative.path, "StreamSeg.shp"), paste0(relative.path, "temporary-shapefiles/culvert_150.shp"), sep = ";"),
                             out_feature_class = paste0(relative.path, "temporary-shapefiles/reach_points.shp"),
                             join_attributes = "ALL",
                             output_type = "POINT")
    
    # Clip the steam segments and known connection points to within the buffer
    arcpy$Clip_analysis(in_features = paste0(relative.path, "AllStreamConnection.shp"),
                        clip_features = paste0(relative.path, "temporary-shapefiles/culvert_150.shp"),
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/subset_points.shp"))
    
    arcpy$Clip_analysis(in_features = paste0(relative.path, "StreamSeg.shp"),
                        clip_features = paste0(relative.path, "temporary-shapefiles/culvert_150.shp"),
                        out_feature_class = paste0(relative.path, "temporary-shapefiles/stream_reach.shp"))
    
    # Align the reach points with the stream layer, then split the line by the new locations
    arcpy$Integrate_management(in_features = paste(paste0(relative.path, "temporary-shapefiles/reach_points.shp"),
                                                   paste0(relative.path, "temporary-shapefiles/stream_reach.shp"),
                                                   sep = ";"))
    
    arcpy$SplitLineAtPoint_management(in_features = paste0(relative.path, "temporary-shapefiles/stream_reach.shp"),
                                      point_features = paste0(relative.path, "temporary-shapefiles/reach_points.shp"),
                                      out_feature_class = paste0(relative.path, "StreamReach.shp"),
                                      search_radius = "1 Meter")
    
    # Recalculate stream lengths
    arcpy$AddGeometryAttributes_management(Input_Features = paste0(relative.path, "StreamReach.shp"),
                                           Geometry_Properties = "LENGTH",
                                           Length_Unit = "METERS",
                                           Area_Unit = "",
                                           Coordinate_System = "")
    
    # Simplify the stream layer by removing useless attributes and create a new numbering system
    arcpy$AddField_management(in_table = paste0(relative.path, "StreamReach.shp"),
                              field_name = "REACHID", 
                              field_type = "LONG")
    
    arcpy$CalculateField_management(in_table = paste0(relative.path, "StreamReach.shp"),
                                    field = "REACHID",
                                    expression = "autoIncrement()",
                                    expression_type = "PYTHON", 
                                    code_block = increment_function)
    
    field.list <- arcpy$ListFields(dataset = paste0(relative.path, "StreamReach.shp"),
                                   field_type = "All")
    field.list <- unlist(lapply(field.list, function(x) x$name))
    field.list <- field.list[!field.list %in% c("FID", "Shape", "REACHID", "StrmType", "Strahler", "LENGTH")] # Excluding the FID and shape fields
    
    arcpy$DeleteField_management(in_table = paste0(relative.path, "StreamReach.shp"), 
                                 drop_field = field.list)
    
    # Clean up the three point files (Reach, Culvert, Stream Connection) and merge
    
    # Reach
    field.list <- arcpy$ListFields(dataset = paste0(relative.path, "temporary-shapefiles/reach_points.shp"),
                                   field_type = "All")
    field.list <- unlist(lapply(field.list, function(x) x$name))
    field.list <- field.list[!field.list %in% c("FID", "Shape", "FEATURE_TY", "InterID")] # Excluding the FID and shape fields
    
    arcpy$DeleteField_management(in_table = paste0(relative.path, "temporary-shapefiles/reach_points.shp"), 
                                 drop_field = field.list)
    
    # Update dbf 
    temp.dbf <- read.dbf(paste0(relative.path, "temporary-shapefiles/reach_points.dbf"))
    temp.dbf$FEATURE_TY <- "Reach"
    write.dbf(dataframe = temp.dbf, 
              file = paste0(relative.path, "temporary-shapefiles/reach_points.dbf"),
              max_nchar = 6)
    
    # Convert to points from multipoints
    arcpy$FeatureToPoint_management(in_features = paste0(relative.path, "temporary-shapefiles/reach_points.shp"),
                                    out_feature_class = paste0(relative.path, "temporary-shapefiles/reach_single_points.shp"))
    
    # Stream Connections
    arcpy$Erase_analysis(in_features = paste0(relative.path, "temporary-shapefiles/subset_points.shp"), 
                         erase_features = paste0(relative.path, "Culverts.shp"), 
                         out_feature_class = paste0(relative.path, "temporary-shapefiles/stream_points.shp"))
    
    field.list <- arcpy$ListFields(dataset = paste0(relative.path, "temporary-shapefiles/stream_points.shp"),
                                   field_type = "All")
    field.list <- unlist(lapply(field.list, function(x) x$name))
    field.list <- field.list[!field.list %in% c("FID", "Shape", "FEATURE_TY", "InterID")] # Excluding the FID and shape fields
    
    arcpy$DeleteField_management(in_table = paste0(relative.path, "temporary-shapefiles/stream_points.shp"), 
                                 drop_field = field.list)
    
    # Update dbf 
    temp.dbf <- read.dbf(paste0(relative.path, "temporary-shapefiles/stream_points.dbf"))
    if(nrow(temp.dbf) != 0) {
      
      temp.dbf$FEATURE_TY <- "Stream"
      write.dbf(dataframe = temp.dbf, 
                file = paste0(relative.path, "temporary-shapefiles/stream_points.dbf"),
                max_nchar = 6)
      
    }
    
    # Culverts
    arcpy$CopyFeatures_management(in_features = paste0(relative.path, "Culverts.shp"), 
                                  out_feature_class = paste0(relative.path, "temporary-shapefiles/culvert_points.shp"))
    
    field.list <- arcpy$ListFields(dataset = paste0(relative.path, "temporary-shapefiles/culvert_points.shp"),
                                   field_type = "All")
    field.list <- unlist(lapply(field.list, function(x) x$name))
    field.list <- field.list[!field.list %in% c("FID", "Shape", "FEATURE_TY", "InterID")] # Excluding the FID and shape fields
    
    arcpy$DeleteField_management(in_table = paste0(relative.path, "temporary-shapefiles/culvert_points.shp"), 
                                 drop_field = field.list)
    
    # Update dbf 
    temp.dbf <- read.dbf(paste0(relative.path, "temporary-shapefiles/culvert_points.dbf"))
    temp.dbf$FEATURE_TY <- "Culvert"
    write.dbf(dataframe = temp.dbf, 
              file = paste0(relative.path, "temporary-shapefiles/culvert_points.dbf"),
              max_nchar = 6)
    
    # Combine point locations into one file and label as Culvert, Connection, Reach
    arcpy$Merge_management(inputs = paste(paste0(relative.path, "temporary-shapefiles/stream_points.shp"),
                                          paste0(relative.path, "temporary-shapefiles/culvert_points.shp"), 
                                          paste0(relative.path, "temporary-shapefiles/reach_single_points.shp"),
                                          sep = ";"), 
                           output = paste0(relative.path, "temporary-shapefiles/reach_connections.shp"))
    
    arcpy$AddField_management(in_table = paste0(relative.path, "temporary-shapefiles/reach_connections.shp"),
                              field_name = "PointID", 
                              field_type = "LONG")
    
    arcpy$CalculateField_management(in_table = paste0(relative.path, "temporary-shapefiles/reach_connections.shp"),
                                    field = "PointID",
                                    expression = "autoIncrement()",
                                    expression_type = "PYTHON", 
                                    code_block = increment_function)
    
    # Calculate node network
    
    # Identify the starting and ending points for the stream segments, and join the connections
    arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamReach.shp"),
                                             out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachSegNodes.shp"),
                                             point_location = "BOTH_ENDS")
    
    arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamReach.shp"),
                                             out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachSegStartNodes.shp"),
                                             point_location = "START")
    
    arcpy$FeatureVerticesToPoints_management(in_features = paste0(relative.path, "StreamReach.shp"),
                                             out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachSegEndNodes.shp"),
                                             point_location = "END")
    
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/ReachSegNodes.shp"),
                               join_features = paste0(relative.path, "temporary-shapefiles/ReachSegNodes.shp"),
                               out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnections.shp"),
                               join_operation = "JOIN_ONE_TO_MANY")
    
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/ReachSegStartNodes.shp"),
                               join_features = paste0(relative.path, "temporary-shapefiles/ReachSegStartNodes.shp"),
                               out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnections1.shp"),
                               join_operation = "JOIN_ONE_TO_ONE")
    
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/ReachSegEndNodes.shp"),
                               join_features = paste0(relative.path, "temporary-shapefiles/ReachSegEndNodes.shp"),
                               out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnections2.shp"),
                               join_operation = "JOIN_ONE_TO_ONE")
    
    # Remove self intersecting nodes by finding nodes with same StreamIDs
    arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/ReachConnections.shp"),
                          out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter.shp"),
                          where_clause = "\"REACHID_1\" > \"REACHID\"")
    
    arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/ReachConnections1.shp"),
                          out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter1.shp"),
                          where_clause = "\"REACHID_1\" = \"REACHID\"")
    
    arcpy$Select_analysis(in_features = paste0(relative.path, "temporary-shapefiles/ReachConnections2.shp"),
                          out_feature_class = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter2.shp"),
                          where_clause = "\"REACHID_1\" = \"REACHID\"")
    
    # Combine the node network together into a single file and create unique ID for each intersection
    arcpy$Erase_analysis(in_features = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter1.shp"),
                         erase_features = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter.shp"),
                         out_feature_class = paste0(relative.path, "temporary-shapefiles/reach_start_points.shp"))
    
    arcpy$Erase_analysis(in_features = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter2.shp"),
                         erase_features = paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter.shp"),
                         out_feature_class = paste0(relative.path, "temporary-shapefiles/reach_end_points.shp"))
    
    arcpy$Merge_management(inputs = paste(paste0(relative.path, "temporary-shapefiles/ReachConnectionsFilter.shp"),
                                          paste0(relative.path, "temporary-shapefiles/reach_start_points.shp"),
                                          paste0(relative.path, "temporary-shapefiles/reach_end_points.shp"), sep = ";"),
                           output = paste0(relative.path, "temporary-shapefiles/all_reach_filtered.shp"))
    
    arcpy$AddField_management(in_table = paste0(relative.path, "temporary-shapefiles/all_reach_filtered.shp"),
                              field_name = "PointID",
                              field_type = "LONG")
    
    arcpy$CalculateField_management(in_table = paste0(relative.path, "temporary-shapefiles/all_reach_filtered.shp"),
                                    field = "PointID",
                                    expression = "autoIncrement()",
                                    expression_type = "PYTHON",
                                    code_block = increment_function)
    
    # Elevation extraction
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "temporary-shapefiles/all_reach_filtered.shp"),
                                   in_raster = "//gisserver.abmi.ca/GIS/Terrain/DEM_SRTM/GEE_srtm_mosaic/srtm.tif",
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/reach_elevation.shp"),
                                   interpolate_values = "NONE")
    
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/reach_elevation.shp"),
                               join_features = paste0(relative.path, "temporary-shapefiles/reach_connections.shp"),
                               out_feature_class = paste0(relative.path, "AllReachConnections.shp"),
                               join_operation = "JOIN_ONE_TO_ONE")
    
    # Update dbf 
    temp.dbf <- read.dbf(paste0(relative.path, "AllReachConnections.dbf"))
    temp.dbf$FEATURE_TY <- as.character(temp.dbf$FEATURE_TY)
    temp.dbf$FEATURE_TY[is.na(temp.dbf$FEATURE_TY)] <- "End"
    write.dbf(dataframe = temp.dbf, 
              file = paste0(relative.path, "AllReachConnections.dbf"),
              max_nchar = 6)
    
    ###########
    # Climate #
    ###########
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "data/base/gis/climate/MAP.asc", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", "MAP_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "data/base/gis/climate/FFP.asc", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", "FFP_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "data/base/gis/climate/Eref.asc", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", "Eref_Point.shp"), 
                                   interpolate_values = "NONE")
    
    arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "Culverts.shp"), 
                                   in_raster = "data/base/gis/climate/AHM.asc", 
                                   out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", "AHM_Point.shp"), 
                                   interpolate_values = "NONE")
    
    #########################
    # Bridge Identification #
    #########################
    
    # Alberta Transportation
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "Culverts.shp"),
                               join_features = "C:/users/beallen/desktop/Lotic-Connectivity/data/base/gis/bridges/alberta-transportation/Bridges-20m-3400_2019.shp",
                               out_feature_class = paste0(relative.path, "ATBridges.shp"),
                               join_operation = "JOIN_ONE_TO_MANY")
    
    # Rail bridges NP
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "Culverts.shp"),
                               join_features = "C:/users/beallen/desktop/Lotic-Connectivity/data/base/gis/bridges/access-layer/railway-bridges-np-20m_2020.shp",
                               out_feature_class = paste0(relative.path, "RailBridges.shp"),
                               join_operation = "JOIN_ONE_TO_MANY")
    
    # Road bridges NP
    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "Culverts.shp"),
                               join_features = "C:/users/beallen/desktop/Lotic-Connectivity/data/base/gis/bridges/access-layer/road-bridges-np-20m_2020.shp",
                               out_feature_class = paste0(relative.path, "RoadBridges.shp"),
                               join_operation = "JOIN_ONE_TO_MANY")
    
    ######################
    # Dam Identification #
    ###################### 

    arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/AllDams.shp"),
                               join_features = paste0(relative.path, "AllStreamConnection.shp"),
                               out_feature_class = paste0(relative.path, "Dams.shp"),
                               join_operation = "JOIN_ONE_TO_ONE", 
                               match_option = "WITHIN_A_DISTANCE_GEODESIC",
                               search_radius = "50 Meters")
    
    #################################
    # Minable Region Identification #
    #################################
    
    arcpy$Clip_analysis(in_features = paste0(relative.path, "AllStreamConnection.shp"),
                        clip_features = paste0(relative.path, "MinableBoundary.shp"),
                        out_feature_class = paste0(relative.path, "IntersectMinable.shp"))
    
    
    #############################
    # Watershed Characteristics #
    #############################
    
    arcpy$Dissolve_management(in_features = paste0("data/base/gis/watersheds/huc-6/", HUC, "_watershed.shp"),
                              out_feature_class = paste0(relative.path, "temporary-shapefiles/watershed_summaries.shp"),
                              dissolve_field = "HUC_8")
    
    # Area
    arcpy$AddGeometryAttributes_management(Input_Features = paste0(relative.path, "temporary-shapefiles/watershed_summaries.shp"),
                                           Geometry_Properties = "AREA",
                                           Length_Unit = "METERS",
                                           Area_Unit = "SQUARE_METERS")
    
    # Perimeter
    arcpy$AddGeometryAttributes_management(Input_Features = paste0(relative.path, "temporary-shapefiles/watershed_summaries.shp"),
                                           Geometry_Properties = "PERIMETER_LENGTH_GEODESIC",
                                           Length_Unit = "METERS",
                                           Area_Unit = "SQUARE_METERS")
    
    # Basin Length
    
    huc.8.boundaries <- read.dbf(paste0("data/base/gis/watersheds/huc-6/", HUC, "_watershed.dbf"))
    
    for (basin.id in as.character(unique(huc.8.boundaries$HUC_8))) {
      
      arcpy$Select_analysis(in_features = paste0("data/base/gis/watersheds/huc-6/", HUC, "_watershed.shp"),
                            out_feature_class = paste0(relative.path, "temporary-shapefiles/", basin.id, "_watershed.shp"),
                            where_clause = paste0("\"HUC_8\" IN ('", basin.id, "')"))
      
      arcpy$Dissolve_management(in_features = paste0(relative.path, "temporary-shapefiles/", basin.id, "_watershed.shp"), 
                                out_feature_class = paste0(relative.path, "temporary-shapefiles/", basin.id, "_watershed_dissolve.shp"), 
                                dissolve_field = "HUC_8")
      
      arcpy$Intersect_analysis(in_features = paste(paste0(relative.path, "StreamSeg.shp"), paste0(relative.path, "temporary-shapefiles/", basin.id, "_watershed_dissolve.shp"), sep = ";"), 
                               out_feature_class = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point.shp"),
                               join_attributes = "ALL", 
                               output_type = "POINT")
      
      arcpy$FeatureToPoint_management(in_features = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point.shp"), 
                                      out_feature_class = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point_single.shp"))
      
      arcpy$sa$ExtractValuesToPoints(in_point_features = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point_single.shp"),
                                     in_raster = "//gisserver.abmi.ca/GIS/Terrain/DEM_SRTM/GEE_srtm_mosaic/srtm.tif",
                                     out_point_features = paste0(getwd(), "/", relative.path, "temporary-shapefiles/", basin.id, "_pour_point_elevation.shp"),
                                     interpolate_values = "NONE")
      
      arcpy$SpatialJoin_analysis(target_features = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point_elevation.shp"),
                                 join_features = paste0(relative.path, "StreamSeg.shp"),
                                 out_feature_class = paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point_id.shp"),
                                 join_operation = "JOIN_ONE_TO_ONE", 
                                 match_option = "WITHIN_A_DISTANCE_GEODESIC",
                                 search_radius = "1 Meters")
      
    }

    #################################
    # Creation on final data frames #
    #################################
    
    # Alignment of culvert point attributes
    att.type <- c("TPI", "VBF", "Slope", "TWI", "HAND", "MAP", "FFP", "Eref", "AHM")
    
    for (att.id in att.type) {
      
      if(att.id == "TPI") {
        
        tpi.point <- read.dbf(paste0(relative.path, "temporary-shapefiles/TPI_Point.dbf"))
        culvert.df <- data.frame(InterID = tpi.point$InterID,
                                 TPI_Point = tpi.point$RASTERVALU)
        rm(tpi.point)
        
      }else {
        
        temp.point <- read.dbf(paste0(relative.path, "temporary-shapefiles/", att.id, "_Point.dbf"))
        temp.point <- temp.point[, c("InterID", "RASTERVALU")]
        colnames(temp.point)[2] <- paste0(att.id, "_Point")
        culvert.df <- merge.data.frame(culvert.df, temp.point, by = "InterID")
        rm(temp.point)
        
      }
      
    }
    
    colnames(culvert.df)[1] <- "TARGET_FID"
    
    # Final data sets
    node.data  <- read.dbf(paste0(relative.path, "StreamSeg.dbf"))
    
    # Nodes
    node.data <- data.frame(
      Stream = node.data$StreamID,
      SectionLength = node.data$LENGTH,
      Watershed = rep(HUC, nrow(node.data)),
      StreamType = node.data$StrmType,
      HabitatType = node.data$Strahler,
      HabitatQuality = rep(1, nrow(node.data))
    )
    
    basin.df <- data.frame(HUC_8 = as.character(unique(huc.8.boundaries$HUC_8)),
                           PourStream = NA,
                           PourElevation = NA)
    
    # Merge basin info with the other stream properties
    for (basin.id in as.character(unique(huc.8.boundaries$HUC_8))) {
      
      temp.df <- read.dbf(paste0(relative.path, "temporary-shapefiles/", basin.id, "_pour_point_id.dbf"))
      
      # Identify min elevation and max strahler
      temp.df <- temp.df[temp.df$RASTERVALU == min(temp.df$RASTERVALU),  ]
      temp.df <- temp.df[temp.df$Strahler == max(temp.df$Strahler),  ]
      basin.df[basin.df$HUC_8 == basin.id, c("PourStream", "PourElevation")] <- temp.df[, c("StreamID", "RASTERVALU")]
      
    }
    
    # Add the Feature type, and watershed properties
    arcpy$SpatialJoin_analysis(target_features =paste0(relative.path, "temporary-shapefiles/Confluence_Elevation.shp"),
                               join_features = paste0(relative.path, "temporary-shapefiles/watershed_summaries.shp"),
                               out_feature_class = paste0(relative.path, "temporary-shapefiles/Segment_temp.shp"),
                               join_operation = "JOIN_ONE_TO_ONE")
    
    # Edges
    stream.edges <- read.dbf(paste0(relative.path, "temporary-shapefiles/Segment_temp.dbf"))
    
    # Merge the basin info
    stream.edges <- merge.data.frame(stream.edges, basin.df, by = "HUC_8")
    
    # Format data set
    stream.edges <- data.frame(
      TARGET_FID = as.numeric(as.character(stream.edges$InterID)),
      StreamID_1 = stream.edges$StreamID, # Downstream segment
      StreamID_2 = stream.edges$StreamID_1, # Upstream segment
      Strahler = stream.edges$Strahler,
      Elevation = stream.edges$RASTERVALU,
      WatershedArea = stream.edges$POLY_AREA,
      WatershedPerm = stream.edges$PERIM_GEO,
      PourStream = stream.edges$PourStream,
      PourElevation = stream.edges$PourElevation
    )
    
    # If there is no elevation value, set to NA
    stream.edges$Elevation[stream.edges$Elevation == -9999] <- NA
    
    stream.edges$Class <- ifelse(is.element(stream.edges$TARGET_FID, culvert.df$TARGET_FID), "Culvert", "Split")
    stream.edges$Node <- paste0(stream.edges$StreamID_1, "-", stream.edges$StreamID_2)
    stream.edges$Up <- ifelse(stream.edges$Class == "Split", 1, NA)
    stream.edges <- merge(stream.edges, culvert.df, by = "TARGET_FID", all = TRUE)
    
    # Correct culverts that are actually bridges and add the date of construction/survey
    # Access Layer Road
    bridges.df <- read.dbf(paste0(relative.path, "RoadBridges.dbf"))
    bridges.df <- bridges.df[!is.na(bridges.df$OBJECTID_2), ]
    bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
    
    stream.edges$BridgeDate <- NA
    stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
    stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
    
    # Access Layer Rail
    bridges.df <- read.dbf(paste0(relative.path, "RailBridges.dbf"))
    bridges.df <- bridges.df[!is.na(bridges.df$OBJECTID_2), ]
    bridges.df <- bridges.df[, c("InterID", "GEO_DATE")]
    
    stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
    stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- 2010 # Assuming 2010
    
    # Alberta Transportation
    bridges.df <- read.dbf(paste0(relative.path, "ATBridges.dbf"))
    bridges.df <- bridges.df[bridges.df$STRUCTURE1 %in% c("STANDARD BRIDGE", "MAJOR BRIDGE"), ] # Bridges
    bridges.df <- bridges.df[bridges.df$STRUCTURE2 %in% c("IN SERVICE"), ] # In service
    bridges.df <- bridges.df[, c("InterID", "FIRST_IN_S")]
    
    stream.edges$Class[match(bridges.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Bridge"
    stream.edges$BridgeDate[match(bridges.df$InterID, stream.edges$TARGET_FID)] <- as.character(bridges.df$FIRST_IN_S)
    
    # Add dam information
    dam.df <- read.dbf(paste0(relative.path, "Dams.dbf"))
    dam.df <- dam.df[!is.na(dam.df$ASSET_TYPE), ]
    
    stream.edges$Dam <- stream.edges$DamType <- stream.edges$DamStatus <- NA
    stream.edges$Dam[match(dam.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- "Dam"
    stream.edges$DamType[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$PURPOSE)
    stream.edges$DamStatus[match(dam.df$InterID, stream.edges$TARGET_FID)] <- as.character(dam.df$STATUS)
    
    # Add road/rail feature type
    features.df <- read.dbf(paste0(relative.path, "RoadBridges.dbf"))
    stream.edges$FeatureType <- NA
    stream.edges$FeatureType[match(features.df$InterID, stream.edges$TARGET_FID, nomatch = 0)] <- as.character(features.df$FEATURE__2)
    
    # Correct culverts that fall within the minable region
    minable.df <- read.dbf(paste(relative.path, "IntersectMinable.dbf", sep = ""))
    stream.edges["MinableRegion"] <- NA
    stream.edges$MinableRegion <- ifelse(stream.edges$TARGET_FID %in% minable.df$InterID, "Inside", "Outside")
    
    edge.data.ref <- data.frame(
      TARGET_FID = stream.edges$TARGET_FID,
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
      FeatureType = stream.edges$FeatureType,
      Mineable = stream.edges$MinableRegion, 
      TWI_Point = stream.edges$TWI_Point,
      Slope_Point = stream.edges$Slope_Point,
      TPI_Point = stream.edges$TPI_Point,
      VBF_Point = stream.edges$VBF_Point,
      HAND_Point = stream.edges$HAND_Point,
      MAP_Point = stream.edges$MAP_Point,
      FFP_Point = stream.edges$FFP_Point,
      Eref_Point = stream.edges$Eref_Point,
      AHM_Point = stream.edges$AHM_Point,
      Elevation = stream.edges$Elevation,
      WatershedPerm = stream.edges$WatershedPerm,
      WatershedArea = stream.edges$WatershedArea,
      PourStream = stream.edges$PourStream,
      PourElevation = stream.edges$PourElevation
    )
    
    # Create reach network
    node.reach  <- read.dbf(paste0(relative.path, "StreamReach.dbf"))
    
    # Nodes
    node.reach <- data.frame(
      Stream = node.reach$REACHID,
      SectionLength = node.reach$LENGTH,
      Watershed = rep(HUC, nrow(node.reach)),
      StreamType = node.reach$StrmType,
      HabitatType = node.reach$Strahler,
      HabitatQuality = rep(1, nrow(node.reach))
    )
    
    # Edges
    edge.reach <- read.dbf(paste0(relative.path, "AllReachConnections.dbf"))
    
    # Format data set
    edge.reach <- data.frame(
      TARGET_FID = as.numeric(as.character(edge.reach$PointID)),
      Culvert_Match = as.numeric(as.character(edge.reach$InterID)),
      Reach_Match = as.numeric(as.character(edge.reach$InterID)),
      Node = paste0(edge.reach$REACHID_1, "-", edge.reach$REACHID),
      UpstreamSeg = edge.reach$REACHID_1, # Upstream segment
      DownstreamSeg = edge.reach$REACHID, # Downstream segment
      Class = edge.reach$FEATURE_TY, 
      Elevation = edge.reach$RASTERVALU
    )
    
    # Replace Culver with Culvert
    edge.reach$Class <- gsub("Culver", "Culvert", edge.reach$Class)
    
    # Remove the culvert match value if not a culvert
    edge.reach$Culvert_Match[edge.reach$Class != "Culvert"] <- NA
    
    # If there is no elevation value, set to NA
    edge.reach$Elevation[edge.reach$Elevation == -9999] <- NA
    
  }
  
  ############
  # Clean up #
  ############
  
  do.call(file.remove, list(list.files(paste0(relative.path, "temporary-databases/"), full.names = TRUE)))
  do.call(file.remove, list(list.files(paste0(relative.path, "temporary-shapefiles/"), full.names = TRUE)))
  
  return(list(node.data, edge.data.ref, node.reach, edge.reach))
  
}


##########################
# Stream standardization # Creates a singular stream network for the province with standardized attributes
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stream_standardization <- function(stream.list, workspace) {
  
  require(foreign)
  require(raster)
  require(rgdal)
  require(RPyGeo)
  
  # Define location of Python for ArcGIS processes
  arcpy <- rpygeo_build_env(path = "C:/Python27/ArcGISx6410.4/",
                            workspace = workspace, 
                            x64 = TRUE, 
                            overwrite = TRUE,
                            extensions = c("Spatial", "GeoStats"))
  
  #####################################
  # Standardization of Stream Network #
  #####################################
  
  for (unique.stream in names(stream.list$stream_layer)) {
    
    # Copy Strahler Stream Order layer, delete unnecessary fields
    arcpy$CopyFeatures_management(in_features = stream.list$stream_layer[[unique.stream]], 
                                  out_feature_class = paste0("scratch/", unique.stream, ".shp"))
    
    # Identify fields within the layer to be removed
    field.list <- arcpy$ListFields(dataset = paste0("scratch/", unique.stream, ".shp"),
                                   field_type = "All")
    field.list <- unlist(lapply(field.list, function(x) x$name))
    field.list <- field.list[!field.list %in% c("FID", "Shape")] # Excluding the FID and shape fields
    
    # Rename attributes that are to be maintained
    sso.rename <- c("StrmType", "StrmLength", "Strahler")
    sso.type <- c("TEXT", "Double", "Double")
    
    for(field.name in 1:3) {
      
      # Rename attribute table names for consistency
      arcpy$AddField_management(in_table = paste0("scratch/", unique.stream, ".shp"),
                                field_name = sso.rename[field.name], 
                                field_type = sso.type[field.name])
      
      arcpy$CalculateField_management(in_table = paste0("scratch/", unique.stream, ".shp"),
                                      field = sso.rename[field.name],
                                      expression = paste("!", stream.list$column_id[[unique.stream]][field.name + 2], "!", sep = ""),
                                      expression_type = "PYTHON")
      
    }
    
    # Remove all remaining attributes
    arcpy$DeleteField_management(in_table = paste0("scratch/", unique.stream, ".shp"), 
                                 drop_field = field.list)
    
  }
  
  # Create the merged layer
  stream.layers <- list.files(paste0(workspace, "scratch/"), full.names = TRUE)
  stream.layers <- stream.layers[grep(".shp", stream.layers)]
  stream.layers <- stream.layers[-grep(".shp.xml", stream.layers)]
  
  arcpy$Merge_management(inputs = paste(stream.layers, collapse = ";"), 
                         output = "cleaned-network/stream_network_merged.shp")
  
  # Remove all temporary files
  do.call(file.remove, list(list.files(paste0(workspace, "scratch/"), full.names = TRUE)))
  rm(arcpy)
  
}

#############################
# Linear Feature subsetting # Creates subsets for the stream and road/rail linear features for user defined regions
#############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linearfeature_subsetting <- function(watershed.layer, road.layer, rail.layer, stream.layer, hf.year, workspace, watershed.lookup, huc.scale) {
  
  require(rgdal)
  require(RPyGeo)
  require(raster)
  require(foreign)
  
  # If the merged roadrail-centerlines exists for the HFI, skip.
  if (!file.exists(paste0(workspace, "roadrail-centerlines/", hf.year, "/roadrail_centerlines_", hf.year, ".shp"))) {
    
    arcpy$Merge_management(inputs = paste(road.layer, rail.layer, sep = ";"), 
                           output = paste("roadrail-centerlines/", hf.year, "/roadrail_centerlines_", hf.year, ".shp", sep = ""))

  }
  
  # For each watershed in the lookup, create the appropriate stream and road/rail subsets

  for (HUC in watershed.lookup) {
    
    where.clause <- paste0("\"HUC_", huc.scale, "\" IN ('", HUC, "')")
    
    # Create watershed mask if not found
    if (!file.exists(paste0(workspace, "watersheds/huc-", huc.scale, "/", HUC, "_watershed.shp"))) {
      
      arcpy$Select_analysis(in_features = watershed.layer,
                            out_feature_class = paste0("watersheds/huc-", huc.scale, "/", HUC, "_watershed.shp"), 
                            where_clause = where.clause)
      
    }

    # Subset centerlines based on the watershed mask if not found
    if (!file.exists(paste0(workspace, "roadrail-centerlines/", hf.year, "/huc-", huc.scale, "/", HUC, "_roadrail.shp"))) {
      
      arcpy$Clip_analysis(in_features = paste0("roadrail-centerlines/", hf.year, "/roadrail_centerlines_", hf.year, ".shp"), 
                          clip_features = paste0("watersheds/huc-", huc.scale, "/", HUC, "_watershed.shp"), 
                          out_feature_class = paste0("roadrail-centerlines/", hf.year, "/huc-", huc.scale, "/", HUC, "_roadrail.shp"))
      
    }

    # Subset stream network based on the watershed mask if not found
    if (!file.exists(paste0(workspace, "strahler_stream_order/huc-", huc.scale, "/", HUC, "_stream.shp"))) {
      
      arcpy$Clip_analysis(in_features = "strahler_stream_order/cleaned-network/stream_network_merged.shp", 
                          clip_features = paste0("watersheds/huc-", huc.scale, "/", HUC, "_watershed.shp"), 
                          out_feature_class = paste0("strahler_stream_order/huc-", huc.scale, "/", HUC, "_stream.shp"))
      
    }
    
  }
  
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
