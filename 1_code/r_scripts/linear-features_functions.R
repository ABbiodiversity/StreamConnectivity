# ---
# title: Functions for standardizing the linear features
# author: Brandon Allen
# created: 2025-01-11
# notes: Define functions that are used for standardizing the linear featuers network
# ---

#' [Linear Feature Standardization]
#'
#' [Creates the standardized and merged road and rail network for the defined years.]
#'
#' @param [hfi.year] [Year of the human footprint inventory.]
#' @param [hfi.lookup] [Lookup table for paths to HFI geodatabases.]
#' @param [workspace] [Workspace of the geodatabase storing the cleaned linear features]
#' @param [arcpy] [arcpy object used for calling ArcPro functions.]
#' @return [Generates standardize linear features network and saves it to the file.name location.]
#' 

linearfeature_standardization <- function(hfi.year, hfi.lookup, workspace, arcpy) {
        
        # Select hfi path
        hfi.path <- hfi.lookup$Path[hfi.lookup$HFI == hfi.year]
        
        # Set the geodatabase to the HFI path to identify possible linear features
        arcpy$env$workspace <- hfi.path
        
        # Merge all linear features in the available database
        candidate.features <- arcpy$ListFeatureClasses()
        
        arcpy$Merge_management(inputs = paste(candidate.features, collapse = ";"), 
                               output = paste0(workspace, "centerlines_temp_", hfi.year, ".shp"))
        
        # Clip the two versions
        arcpy$PairwiseClip_analysis(in_features = paste0(workspace, "centerlines_2023.shp"), 
                                    clip_features = paste0(workspace, "centerlines_temp_", hfi.year, ".shp"), 
                                    out_feature_class = paste0(workspace, "centerlines_", hfi.year, ".shp"))
        
        # Remove the temporary version
        arcpy$Delete_management(in_data = paste0(workspace, "centerlines_temp_", hfi.year, ".shp"))
        
}

#' [Linear Feature Subsetting]
#'
#' [Creates subsets for the stream and road/rail linear features for user defined regions.]
#'
#' @param [centerline.layer] [File path for the standardized centerlines.]
#' @param [stream.layer] [File path for the standardized stream network.]
#' @param [hfi.year] [Year of the human footprint inventory.]
#' @param [watershed.layer] [Watershed layer used for defining the HUC boundaries.]
#' @param [huc.scale] [HUC watershed scale used for the boundaries.]
#' @param [huc.unit] [Individual HUC watershed codes. They need to be valid watershed codes for the HUC scale.]
#' @param [folder.name] [folder path used for saving geodatabases. Creates subfolders based on huc scale and year]
#' @param [arcpy] [arcpy object used for calling ArcPro functions.]
#' @return [Generates the geodatabases used for processing individual watersheds.]
#' 

linearfeature_subsetting <- function(centerline.layer, stream.layer, hfi.year, 
                                     watershed.layer, huc.scale, huc.unit, folder.name, arcpy) {
        
        # Generate the folder for storing results
        analysis.path <- paste0(getwd(), "/", folder.name, "/huc-", huc.scale, "/", hfi.year, "/gis/")
        if(!dir.exists(analysis.path)) {
                
                dir.create(analysis.path,
                           recursive = TRUE)
                
        }

        # Create geodatabase
        arcpy$CreateFileGDB_management(out_folder_path = analysis.path, 
                                       out_name = paste0(huc.unit, ".gdb"))
        
        # Define workspace
        arcpy$env$workspace <- paste0(analysis.path, huc.unit, ".gdb")
        
        # Define the where clause for including watersheds
        where.clause <- paste0("\"HUC_", huc.scale, "\" IN ('", huc.unit, "')")
        
        # Create a watershed mask
        arcpy$Select_analysis(in_features = paste0(getwd(), "/", watershed.layer),
                              out_feature_class = paste0("watershed_boundary"), 
                              where_clause = where.clause)
        
        # Clip the road and rail centerlines
        arcpy$PairwiseClip_analysis(in_features = centerline.layer, 
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "road_rail")
        
        # Clip the stream network
        arcpy$PairwiseClip_analysis(in_features = stream.layer, 
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "stream_network")
        
}

