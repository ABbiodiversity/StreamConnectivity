# ---
# title: "Network creation"
# author: "Brandon Allen"
# created: "2025-01-11"
# inputs: ["0_data/processed/stream-network/stream_network_standardized.shp";
#          "0_data/processed/centerline-network/centerlines-hfi.shp";
#          "0_data/external/dams/Alberta_Dams_3400_2021-05-11.shp";
#          "0_data/external/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp";
#          "0_data/external/topographic/Slope_LiDAR.tif";
#          "0_data/external/topographic/ALOS/DEM.tif";
#          "0_data/external/climate/MAP.tif";
#          "0_data/external/climate/Eref.tif";
#          "0_data/external/topographic/nsr/Natural_Regions_Subregions_of_Alberta.shp"
#          "0_data/external/bridges/access-layer/railway-bridges-np-20m_2020.shp";
#          "0_data/external/bridges/alberta-transportation/Bridges-20m-3400_2019.shp";
#          "0_data/external/bridges/access-layer/road-bridges-np-20m_2020.shp"]
# outputs: ["network_HUC.Rdata - One for each year and HUC watershed"]
# notes: 
#   "This script creates the network files (edges and nodes) required for calculating stream connectivity"
# ---

# 1.0 Clear memory ----
rm(list=ls())
gc()

# 1.1 Load libraries and source functions ----
library(foreach)
library(foreign)
library(parallel)
source("1_code/r-scripts/network-creation_functions.R")

# 1.2 Define the focal years that HFI are available for pro
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

# 1.3 Define HUC scale and valid watershed ids
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

# 1.4 Define the cores and objects required for for parallel processing ----
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series",
                            "huc.layer", "network_creation"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        library(sf)
        
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

# 3.4 Generate the networks for each watershed ----

foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  as.list(watershed.ids), 
                  fun = function(huc) tryCatch(network_creation(watershed.geodatabase = paste0(getwd(), "/0_data/processed/huc-", huc.scale, "/", 
                                                                                                 hfi, "/gis/", huc, ".gdb"),
                                                                huc.scale = huc.scale,
                                                                huc.unit = huc,
                                                                hfi.year = hfi,
                                                                dam.layer = paste0(getwd(), "/0_data/external/dams/Alberta_Dams_3400_2021-05-11.shp"), 
                                                                mineable.boundary = paste0(getwd(), "/0_data/external/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp"),
                                                                Slope = paste0(getwd(), "/0_data/external/topographic/Slope_LiDAR.tif"),
                                                                DEM = paste0(getwd(), "/0_data/external/topographic/ALOS/DEM.tif"), 
                                                                MAP = paste0(getwd(), "/0_data/external/climate/MAP.tif"), 
                                                                Eref = paste0(getwd(), "/0_data/external/climate/Eref.tif"),
                                                                NR = paste0(getwd(), "/0_data/external/topographic/nsr/Natural_Regions_Subregions_of_Alberta.shp"),
                                                                AT.bridges = paste0(getwd(), "/0_data/external/bridges/alberta-transportation/Bridges-20m-3400_2019.shp"), 
                                                                NP.rail.bridges = paste0(getwd(), "/0_data/external/bridges/access-layer/railway-bridges-np-20m_2020.shp"),
                                                                NP.road.bridges = paste0(getwd(), "/0_data/external/bridges/access-layer/road-bridges-np-20m_2020.shp"),
                                                                folder.name = "2_pipeline",
                                                                arcpy = arcpy), error = function(e) e)

        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()

