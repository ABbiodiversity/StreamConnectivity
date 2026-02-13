# ---
# title: Stream standardization functions
# author: Brandon Allen
# created: 2025-01-11
# notes: Define functions that are used for standardizing the stream network
# ---


#' [Stream standardization]
#'
#' [Creates a singular stream network for the province with standardized attributes.]
#'
#' @param [stream.list] [List of stream networks and the field attribute names to extract object ID, shape, and strahler stream order information.]
#' @param [workspace] [Folder where the partial stream networks are stored to allow ArcPro to merge them.]
#' @param [file.name] [File name used to define where the standardized stream network is saved.]
#' @param [arcpy] [arcpy object used for calling ArcPro functions.]
#' @return [Generates standardize stream network and saves it to the file.name location.]
#' 

stream_standardization <- function(stream.list, workspace, file.name, arcpy) {
                
        #####################################
        # Standardization of Stream Network #
        #####################################
        
        # Create the scratch folder
        dir.create(paste0(workspace, "scratch"))
        
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
                               output = paste0(file.name))
        
        # Remove all temporary files
        do.call(file.remove, list(list.files(paste0(workspace, "scratch/"), full.names = TRUE)))
        unlink(paste0(workspace, "scratch"), recursive = TRUE)
                
}
