# ---
# title: "Linear feature standardization"
# author: "Brandon Allen"
# created: "2025-01-11"
# inputs: ["0_data/external/roadrail-centerlines/2010-2021 HFI centrelines"]
# outputs: ["0_data/processed/stream-network/stream_network_standardized.shp"]
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
source("1_code/r-scripts/linear-features_functions.R")

# 1.2 Define the focal years that HFI are available for processing
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) 

# 2.0 Linear feature standardization ----

# 2.1 Define the cores and objects required for for parallel processing ----
n.clusters <- length(hfi.series)
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("hfi.series", "linearfeature_merging"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreach)
        library(foreign)
        library(reticulate)
        
        # Initialize arcpy
        py_discover_config() # We need version 3.9
        py_config() # Double check it is version 3.9
        
        # Set python 
        use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 2.2 Merge linear features network ----

parLapply(core.input, 
          as.list(hfi.series), 
          fun = function(hfi) tryCatch(linearfeature_merging(road.layer = paste0("0_data/external/roadrail-centerlines/", hfi, 
                                                                                 "/road_centerlines_", hfi, ".shp"),
                                                             rail.layer = paste0("0_data/external/roadrail-centerlines/", hfi, 
                                                                                 "/rail_centerlines_", hfi, ".shp"), 
                                                             hfi.year = hfi,
                                                             file.name = "0_data/processed/centerline-network/",
                                                             arcpy = arcpy), error = function(e) e)
)

stopCluster(core.input)

# 3.0 Subsetting centrelines and stream network ----

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

# 3.3 Define the cores and objects required for for parallel processing ----
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series",
                            "huc.layer", "stream.path", "linearfeature_subsetting"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        
        # Initialize arcpy
        py_discover_config() # We need version 3.9
        py_config() # Double check it is version 3.9
        
        # Set python 
        use_python(python = "C:/Users/ballen/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 3.4 Generate the geodatabases for processing ----

foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(linearfeature_subsetting(centerline.layer = paste0(getwd(), 
                                                                                                 "/0_data/processed/centerline-network/centerlines_", 
                                                                                                 hfi, ".shp"),
                                                                        stream.layer = paste0(getwd(),
                                                                                              "/", stream.path),
                                                                        hfi.year = hfi,
                                                                        watershed.layer = huc.layer,
                                                                        huc.scale = huc.scale,
                                                                        huc.unit = huc,
                                                                        folder.name = "0_data/processed/", 
                                                                        arcpy = arcpy), error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()

