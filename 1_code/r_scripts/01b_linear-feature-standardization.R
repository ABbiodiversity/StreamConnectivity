# ---
# title: "Linear feature standardization"
# author: "Brandon Allen"
# created: "2025-01-11"
# inputs: ["0_data/external/roadrail-centerlines/2010-2023 HFI centrelines"]
# outputs: ["0_data/processed/centerline-network/centerline_hfi.shp"]
# notes: 
#   "This script standardizes the roads and rail centreline features available in the ABMI HFI inventories."
# ---

# 1.0 Clear memory ----
rm(list=ls())
gc()

# 1.1 Load libraries and source functions ----
library(foreach)
library(foreign)
library(parallel)
library(reticulate)
source("1_code/r_scripts/linear-features_functions.R")

# 1.2 Define the focal years that HFI are available for processing
# We are ignore 2010 as it requires separate processing and 2023 is our reference year
hfi.series <- c(2014, 2016, 2018, 2019, 2020, 2021, 2022) 

# Load the hfi lookup
hfi.lookup <- read.csv("0_data/external/lookup/hfi-path-lookup.csv")

# 2.0 Linear feature standardization ----

# 2.1 Define the 2023 Road and Rail centerline classifications

# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
arcpy <- import('arcpy') 
arcpy$env$parallelProcessingFactor <- "100%"

# Define HFI 2023 path
hfi.2023.path <- hfi.lookup$Path[hfi.lookup$HFI == 2023]

# Merge roads and rails
arcpy$Merge_management(inputs = paste(paste0(hfi.2023.path, "/o03_Roads_Centerlines_HFI_2023"), 
                                      paste0(hfi.2023.path, "/o04_Railways_Centerlines_HFI_2023"), sep = ";"), 
                       output = paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2023_temp.shp"))

# Remove vegetated roads
arcpy$Select_analysis(in_features = paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2023_temp.shp"), 
                      out_feature_class = paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2023.shp"), 
                      where_clause = "FEATURE_TY NOT IN ('Road - Vegetated', 'Road - Vegetated - OSE')")

arcpy$Delete_management(paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2023_temp.shp"))

rm(arcpy)

# 2.2 Define the cores and objects required for for parallel processing ----
n.clusters <- length(hfi.series)
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("hfi.series", "linearfeature_standardization", "hfi.lookup"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreach)
        library(foreign)
        library(reticulate)
        
        # Set python 
        use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 2.3 Standardize the linear features networks ----
# 2010 is handled separately as we are clipping the cleaned 2014 version to the boundaries
# of HFI 2010 old centrelines (not part of the version 2.0 geodatabase)

parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(linearfeature_standardization(workspace = paste0(getwd(), "/0_data/processed/centerline-network/"),  
                                                             hfi.year = hfi,
                                                             hfi.lookup = hfi.lookup,
                                                             arcpy = arcpy), error = function(e) e)
)

stopCluster(core.input)

# Using the approximated 2010 centrelines, clip to the 2014 centerlines that have been standardized
arcpy <- import('arcpy') 
arcpy$env$parallelProcessingFactor <- "100%"

# Define HFI 2010 path
hfi.2010.path <- hfi.lookup$Path[hfi.lookup$HFI == 2010]

# Create the standardized 2010 centerline
arcpy$Merge_management(inputs = paste(paste0(hfi.2010.path, "/road_centerlines_2010.shp"), 
                                      paste0(hfi.2010.path, "/rail_centerlines_2010.shp"), sep = ";"), 
                       output = paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2010_temp.shp"))

arcpy$PairwiseClip_analysis(in_features = "0_data/processed/centerline-network/centerlines_2014.shp", 
                            clip_features = "0_data/processed/centerline-network/centerlines_2010_temp.shp", 
                            out_feature_class = "0_data/processed/centerline-network/centerlines_2010.shp")

arcpy$Delete_management(paste0(getwd(), "/0_data/processed/centerline-network/centerlines_2010_temp.shp"))
rm(arcpy)

# 3.0 Subsetting centrelines and stream network for all years ----

# 3.1 Define HUC scale and valid watershed ids
huc.scale <- 6
if(huc.scale == 6) {
        
        huc.layer <- paste0("0_data/external/watersheds/boundary/HUC_",
                            8,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
        
} else {
        
        huc.layer <- paste0("0_data/external/watersheds/boundary/HUC_",
                            huc.unit,
                            "_EPSG3400.dbf")
        watershed.ids <- read.dbf(huc.layer)
}

watershed.ids <- unique(as.character(watershed.ids[, paste0("HUC_", huc.scale)]))

# 3.2 Define the path for the standardized stream network
stream.path <- "0_data/processed/stream-network/stream_network_standardized.shp"

# 3.3 Create to-do list for parallel processing ----
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021, 2022, 2023) 
todo.list <- expand.grid(hfi = hfi.series, huc = watershed.ids)

# 3.4 Define the cores and objects required for for parallel processing ----
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "todo.list",
                            "huc.layer", "stream.path", "linearfeature_subsetting"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        
        # Set python 
        use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 3.5 Generate the geodatabases for processing ----

parLapply(core.input, 
          as.list(rownames(todo.list)), 
          fun = function(task) {
                  
                  # Identify HUC and HFI for the task
                  hfi <- todo.list[task, "hfi"]
                  huc <- todo.list[task, "huc"]
                  
                  tryCatch(linearfeature_subsetting(centerline.layer = paste0(getwd(), 
                                                                              "/0_data/processed/centerline-network/centerlines_", 
                                                                              hfi, ".shp"),
                                                    stream.layer = paste0(getwd(),
                                                                          "/", stream.path),
                                                    hfi.year = hfi,
                                                    watershed.layer = huc.layer,
                                                    huc.scale = huc.scale,
                                                    huc.unit = huc,
                                                    folder.name = "2_pipeline", 
                                                    arcpy = arcpy), error = function(e) e)}
)

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()

