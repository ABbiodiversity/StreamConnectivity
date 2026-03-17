# ---
# title: "Provincial stream scores"
# author: "Brandon Allen"
# created: "2025-12-12"
# inputs: ["2_pipeline/huc-6/connectivity/";
#          "2_pipeline/huc-6/gis/"]
# outputs: ["2_pipeline/streams/stream-scores-hfi", hfi.year, ".shp""]
# notes: 
#   "This script stitches together the individual stream scores and mergers them into a single shapefile."
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(sf)

# 1.3 Define parameters ----
analysis.year <- 2022
huc.scale <- 6
watershed.ids <- read_sf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.shp")
watershed.ids <- as.character(unique(watershed.ids$HUC_6)) 

# 2.0 Stitching results ----
for (hfi.year in analysis.year) {
        
        # Identify the watershed list
        results.list <- list.files(paste0("2_pipeline/huc-", huc.scale, "/", 
                                          hfi.year, "/connectivity/"), full.names = TRUE)
        
        for(id in 1:length(watershed.ids)) {
                
                # Load a stream layer
                HUC <- watershed.ids[id]
                load(results.list[grep(HUC, results.list)])
                
                #  Load the stream layer
                stream.layer <- read_sf(dsn =  paste0("2_pipeline/huc-", huc.scale, "/", 
                                                      hfi.year, "/gis/", HUC, ".gdb/"),
                                        layer = "StreamSeg")
                
                if(is.null(watershed.network$ModelMean)){
                        
                        connectivity <- data.frame(StreamID = stream.layer$StreamID,
                                                     StreamLength = stream.layer$StrmLength,
                                                     StreamConnect = 1,
                                                     Numerator = 1,
                                                     Denominator = 1)
                        
                } else {
                        
                        connectivity <- watershed.network$ModelMean
                       
                }
                
                # Merge the connectivity information with the polygon
                stream.layer <- merge(stream.layer, connectivity, by = "StreamID")
                stream.layer <- stream.layer[, c("StreamID", "StreamLength", "StreamConnect", "Numerator", "Denominator")]
                

                if(id == 1) {
                        
                        merge.stream.layer <- stream.layer
                        
                } else {
                        
                        merge.stream.layer <- rbind(merge.stream.layer, stream.layer)
                        
                }
                
                print(HUC)
                
        }
        
        # Confirm everything is numeric
        merge.stream.layer$StreamConnect <- as.numeric(merge.stream.layer$StreamConnect)
        
        write_sf(merge.stream.layer, 
                 dsn = paste0("2_pipeline/streams/stream-scores-hfi", hfi.year, ".shp"))
        
        rm(merge.stream.layer)
        
}

# Clear memory
rm(list=ls())
gc()