# ---
# title: Functions for standardizing the linear features
# author: Brandon Allen
# created: 2025-01-11
# notes: Define functions that are used for standardizing the linear featuers network
# ---

#' [Linear Feature Merging]
#'
#' [Creates the merged road and rail network for the defined years.]
#'
#' @param [road.layer] [File path for the rail centerlines files.]
#' @param [rail.layer] [File path for the rail centerlines files.]
#' @param [hfi.year] [Year of the human footprint inventory.]
#' @param [file.name] [File name used to define where the standardized stream network is saved.]
#' @param [arcpy] [arcpy object used for calling ArcPro functions.]
#' @return [Generates standardize linear features network and saves it to the file.name location.]
#' 

linearfeature_merging <- function(road.layer, rail.layer, hfi.year, file.name, arcpy) {
        
        # If the merged roadrail-centerlines exists for the HFI, skip.
        if (!file.exists(paste0(getwd(), "/", file.name, 
                                "centerlines_", hfi.year, ".shp"))) {
                
                arcpy$Merge_management(inputs = paste(road.layer, rail.layer, sep = ";"), 
                                       output = paste0(getwd(), "/", file.name, "centerlines_", 
                                                       hfi.year, ".shp"))
                
        }
        
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

