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
source("1_code/r_scripts/network-creation_functions.R")

# 1.2 Define the focal years that HFI are available for pro
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021, 2022) # Define HFI years 

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

# 1.4 Create to-do list for parallel processing ----
todo.list <- expand.grid(hfi = hfi.series, huc = watershed.ids)

# 1.5 Define the cores and objects required for for parallel processing ----
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "todo.list",
                            "huc.layer", "network_creation"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(foreign)
        library(reticulate)
        library(sf)
        
        # Set python 
        use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")
        
        # Load arcpy
        arcpy <- import('arcpy') 
        
        # Define parallel processing factor
        # This needs to be set to 0 when performing parallel processing on workers.
        # If not set to 0, processes get jumbled and may fail.
        arcpy$env$parallelProcessingFactor <- "0%"
        
})

# 2.0 Generate the networks for each watershed ----
parLapply(core.input, 
          as.list(rownames(todo.list)), 
          fun = function(task) {
                  
                  # Identify HUC and HFI for the task
                  hfi <- todo.list[task, "hfi"]
                  huc <- todo.list[task, "huc"]
                  
                  tryCatch(network_creation(watershed.geodatabase = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                           hfi, "/gis/", huc, ".gdb"),
                                            huc.scale = huc.scale,
                                            huc.unit = huc,
                                            hfi.year = hfi,
                                            dam.layer = paste0(getwd(), "/0_data/external/dams/Alberta_Dams_3400_2021-05-11.shp"), 
                                            mineable.boundary = paste0(getwd(), "/0_data/external/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp"),
                                            Slope = paste0(getwd(), "/0_data/external/topographic/Slope_LiDAR.tif"),
                                            DEM = paste0(getwd(), "/0_data/external/topographic/fab_dem/fab_dem_us_canada.tif"), 
                                            MAP = paste0(getwd(), "/0_data/external/climate/MAP.tif"), 
                                            Eref = paste0(getwd(), "/0_data/external/climate/Eref.tif"),
                                            NR = paste0(getwd(), "/0_data/external/topographic/nsr/Natural_Regions_Subregions_of_Alberta.shp"),
                                            AT.bridges = paste0(getwd(), "/0_data/external/bridges/alberta-transportation/Bridges-20m-3400_2019.shp"), 
                                            NP.rail.bridges = paste0(getwd(), "/0_data/external/bridges/access-layer/railway-bridges-np-20m_2020.shp"),
                                            NP.road.bridges = paste0(getwd(), "/0_data/external/bridges/access-layer/road-bridges-np-20m_2020.shp"),
                                            folder.name = "2_pipeline",
                                            arcpy = arcpy), error = function(e) e)}
          
)

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()

