#
# Title: Functions for prepping the linear features
# Created: September 1st, 2021
# Last Updated: June 27th, 2023
# Author: Brandon Allen
# Objectives: Define functions required for merging and subsetting the road and rail networks
# Keywords: Linear Feature Merging, Linear Feature Subsetting
# Note: 
#

##########################
# Linear Feature Merging # Creates the merged road and rail networks for each year
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linearfeature_merging <- function(road.layer, rail.layer, hfi.year, arcpy) {
        
        # If the merged roadrail-centerlines exists for the HFI, skip.
        if (!file.exists(paste0(getwd(), "/data/base/gis/roadrail-centerlines/", hfi.year, 
                                "/roadrail_centerlines_", hfi.year, ".shp"))) {
                
                arcpy$Merge_management(inputs = paste(road.layer, rail.layer, sep = ";"), 
                                       output = paste0(getwd(), "/data/base/gis/roadrail-centerlines/", 
                                                       hfi.year, "/roadrail_centerlines_", 
                                                       hfi.year, ".shp"))
                
        }
        
}

#############################
# Linear Feature Subsetting # Creates subsets for the stream and road/rail linear features for user defined regions
#############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linearfeature_subsetting <- function(watershed.layer, road.rail.layer, stream.layer, 
                                     hfi.year, huc.scale, huc.unit, arcpy) {
        
        # Create geodatabase
        arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/huc-", 
                                                                huc.scale, "/", hfi.year, "/gis/"), 
                                       out_name = paste0(huc.unit, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                      hfi.year, "/gis/", huc.unit, ".gdb")
        
        # Define the where clause for including watersheds
        where.clause <- paste0("\"HUC_", huc.scale, "\" IN ('", huc.unit, "')")
        
        # Create a watershed mask
        arcpy$Select_analysis(in_features = paste0(getwd(), "/", watershed.layer),
                              out_feature_class = paste0("watershed_boundary"), 
                              where_clause = where.clause)
        
        # Clip the road and rail centerlines
        arcpy$PairwiseClip_analysis(in_features = road.rail.layer, 
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "road_rail")
        
        # Clip the stream network
        arcpy$PairwiseClip_analysis(in_features = stream.layer, 
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "stream_network")
        
}

