# ---
# title: "Create regional summaries"
# author: "Brandon Allen"
# created: "2025-01-21"
# inputs: ["network_HUC.Rdata - One for each year and HUC watershed"]
# outputs: ["3_output/tables/summaries/connectivity_hfi_[year].Rdata";
#           "3_output/summaries/shapefiles/connectivity_hfi_[year].shp"]
# notes: 
#   "This script generates the final summaries (tables and shapfiles) for the stream connectivity."
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(sf)

# 1.3 Define parameters ----
boundary.poly <- read_sf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.shp")
watershed.ids <- unique(as.character(boundary.poly$HUC_6))
boundary.template <- boundary.poly[, c("HUC_8", "HUC_6", "HUC_4", "HUC_2", "geometry")]
boundary.poly <- boundary.poly
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021)
huc.scale <- 6

# 2.0 Yearly Summaries ----
for (hfi.year in hfi.series) {
        
        results.list <- list.files(paste0("2_pipeline/huc-", huc.scale, "/", 
                                          hfi.year, "/connectivity/"), full.names = TRUE)
        
        # Create temporary geospatial object for storing results
        boundary.poly <- boundary.template
        boundary.poly$ConnectMin <- NA
        boundary.poly$Connect <- NA
        boundary.poly$ConnectMax <- NA
        
        for(HUC in watershed.ids) {
                
                # Load a stream layer
                # Load file
                load(results.list[grep(HUC, results.list)])
                
                # If no connectivity results, default to 100%
                if(is.null(watershed.network[["ModelMean"]])) {
                        
                        boundary.poly$Connect[boundary.poly$HUC_6 == HUC] <- 100
                        boundary.poly$ConnectMin[boundary.poly$HUC_6 == HUC] <- 100
                        boundary.poly$ConnectMax[boundary.poly$HUC_6 == HUC] <- 100
                        
                } else {
                        
                        
                        
                        # Subset connectivity results
                        connecitivty.results <- watershed.network[["ModelMean"]]
                        
                        # Calculate connectivity and merge with shapefile
                        boundary.poly$Connect[boundary.poly$HUC_6 == HUC] <- 100 * (sum(connecitivty.results$StreamConnect * 
                                                                                                connecitivty.results$StreamLength) / sum(connecitivty.results$StreamLength))
                        
                        # Subset connectivity results
                        connecitivty.results <- watershed.network[["ModelLower"]]
                        
                        # Calculate connectivity and merge with shapefile
                        boundary.poly$ConnectMin[boundary.poly$HUC_6 == HUC] <- 100 * (sum(connecitivty.results$StreamConnect * 
                                                                                                   connecitivty.results$StreamLength) / sum(connecitivty.results$StreamLength))
                        
                        # Subset connectivity results
                        connecitivty.results <- watershed.network[["ModelUpper"]]
                        
                        # Calculate connectivity and merge with shapefile
                        boundary.poly$ConnectMax[boundary.poly$HUC_6 == HUC] <- 100 * (sum(connecitivty.results$StreamConnect * 
                                                                                                   connecitivty.results$StreamLength) / sum(connecitivty.results$StreamLength))
                        
                        rm(connecitivty.results)
                        
                }
                
                rm(watershed.network)
                
        }
        
        # Save the results
        write_sf(boundary.poly, dsn = paste0("3_output/shapefiles/summaries/watershed_status_", hfi.year, ".shp"))
        save(boundary.poly, file = paste0("3_output/tables/summaries/watershed_status_", hfi.year, ".Rdata"))
        rm(boundary.poly)
        
        print(hfi.year)
        
}

rm(list=ls())
gc()
