# ---
# title: "Packaging regional summaries"
# author: "Brandon Allen"
# created: "2026-01-23"
# inputs: ["3_output/summaries/shapefiles/connectivity_hfi_[year].shp"]
# outputs: ["3_output/shapefiles/geodiscover/stream_connectivity_2010-2022.gdb"]
# notes: 
#   "This script generates the geodatabase to be released on Alberta Geodiscover."
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(sf)
library(reticulate)

# 1.2 Initialize reticulate to communicate with ArcPro
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# 1.3 Define output file name ----
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/3_output/shapefiles/geodiscover/"), 
                               out_name = "stream_connectivity_2010-2022.gdb")


# 1.3 Define parameters ----
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021, 2022)

# 2.0 Yearly Summaries ----
for (year in hfi.series) {
        
        # If first year, load the shapefile
        if (year == min(hfi.series)) {
                
                watershed.results <- read_sf(dsn = paste0("3_output/shapefiles/summaries/watershed_status_", year, ".shp"))
                watershed.results <- watershed.results[, c("HUC_8", "HUC_6", "HUC_4", "HUC_2", "Connect")]
                colnames(watershed.results)[5] <- paste0("Connectivity_", year)
    
        } else {
                
                watershed.temp <- as.data.frame(read_sf(dsn = paste0("3_output/shapefiles/summaries/watershed_status_", year, ".shp")))
                watershed.temp <- watershed.temp[, c("HUC_8", "Connect")]
                colnames(watershed.temp)[2] <- paste0("Connectivity_", year)
                
                watershed.results <- merge(watershed.results, watershed.temp, by = "HUC_8")
                
        }
        
}


write_sf(watershed.results, 
         dsn = paste0(getwd(), "/3_output/shapefiles/geodiscover/stream_connectivity_2010-2022.gdb"),
         layer = "Stream_Connectivity")

rm(list=ls())
gc()
