#
# Title: Stream standardization functions
# Created: September 1st, 2021
# Last Updated: June 27th, 2024
# Author: Brandon Allen
# Objectives: Functions required for standardizing the stream network.
# Keywords: Stream Standardization
# Note: 
#

##########################
# Stream standardization # Creates a singular stream network for the province with standardized attributes
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
