#
# Title: Merging culvert results
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Merging the predicted passability scores into a single file and merge with the appropriate shapefiles.
# Keywords: Notes, Data Merging, Indicator Summarization, BMF Summarization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# 1) All paths defined in this script are local
#
################
# Data Merging #
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load Librarys
library(foreign)
library(rgdal)
library(sf)

# Define analysis year and list of folders to start the compilation from
analysis.year <- 2010
huc.unit <- 6

huc.list <- list.dirs(paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year), full.names = TRUE)
huc.name <- list.dirs(paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year), full.names = FALSE)
huc.list <- huc.list[-c(1, length(huc.list))] # remove the folders that have no information
huc.name <- huc.name[-c(1, length(huc.name))] # remove the folders that have no information
names(huc.name) <- huc.list

for(folder.id in huc.list) {
        
        temp.file.list <- list.files(folder.id, full.names = TRUE)
        compiled.results <- NULL
        
        for( file.id in temp.file.list ) {
                
                temp.file <- read.csv(file.id)
                
                if(ncol(temp.file) == 6) {
                        
                        temp.file <- temp.file[, -6]
                        
                }
                compiled.results <- rbind(compiled.results, temp.file)
                rm(temp.file)
                
        }
        
        # If StreamConnect is na, fix to 1, if Numerator or Denominator is NA, fix to 0
        
        compiled.results$StreamConnect[is.na(compiled.results$StreamConnect)] <- 1
        compiled.results$Numerator[is.na(compiled.results$Numerator)] <- 0
        compiled.results$Denominator[is.na(compiled.results$Denominator)] <- 0
        
        # Save the results
        write.csv(compiled.results, file = paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year, "/compiled/", huc.name[folder.id], "-compiled_", Sys.Date(), ".csv"), row.names = FALSE)
        print(huc.name[folder.id][[1]])
        rm(compiled.results)
        
}

# Now that the result files have been compiled, begin the merge with the original shapefiles for visualization

# Define huc folder IDs
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
results.list <- list.files(paste0("results/tables/probability/huc-", huc.unit, "/", analysis.year, "/compiled"), full.names = TRUE)

for (huc in watershed.ids) {
        
        # Load data
        temp.shapefile <- readOGR(paste0("data/processed/huc-6/", analysis.year, "/gis/", huc, "/StreamSeg.shp"))
        
        # If there was a connectivity score calculated for a watershed (at least 1 culvert present)
        # Proceed as normal, otherwise assume connectivity of 1
        
        if( length(paste0(results.list[grep(paste0(huc, "-"), results.list)])) == 0 ) {
                
                temp.shapefile@data["Numerator"] <- 0
                temp.shapefile@data["Denominator"] <- 0
                temp.shapefile@data["StreamConnect"] <- 1
                
        } else {
          
          # Load results
          temp.compiled <- read.csv(paste0(results.list[grep(paste0(huc, "-"), results.list)]))
          
          # Merge files
          temp.shapefile@data <- merge.data.frame(temp.shapefile@data, temp.compiled, by = "StreamID", all.x = TRUE)

          rm(temp.compiled)
          
        }
        
        # Clean up shapefile data, rename columns (less than 7 characters)
        temp.shapefile@data <- temp.shapefile@data[, c("StreamID", "StrmType", "LENGTH", "Strahler", "Numerator", "Denominator", "StreamConnect")]
        colnames(temp.shapefile@data) <- c("StrmID", "Class", "Length", "Strhler", "Num", "Dem", "Connect")
        
        # There are instances where single streams were not given a score, update to a value of 1 for connectivity and 0 for num + dem
        na.store <- temp.shapefile@data$Connect[is.na(temp.shapefile@data$Connect)]
        
        if (length(na.store) != 0) {
                
                temp.shapefile@data$Connect[is.na(temp.shapefile@data$Connect)] <- 1
                temp.shapefile@data$Num[is.na(temp.shapefile@data$Num)] <- 0
                temp.shapefile@data$Dem[is.na(temp.shapefile@data$Dem)] <- 0
                
        }

        writeOGR(obj = temp.shapefile, 
                 dsn = paste0("results/shapefiles/probabilities/huc-", huc.unit, "/", analysis.year, "/", huc, "-connectivity_", Sys.Date(), ".shp"),
                 layer = huc,
                 driver = "ESRI Shapefile")
        
        rm(temp.shapefile, na.store)
        
}

###########################
# Indicator Summarization #
###########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(sf)
library(rgdal)

# Definet the analysis year
analysis.year <- 2018
huc.unit <- 6

# Identify paths for each of the summarized shapefiles
results.list <- list.files(paste0("results/shapefiles/probabilities/huc-", huc.unit, "/", analysis.year), full.names = TRUE)
results.list <- results.list[grep(".shp", results.list)]

# Load the boundary layer for summarization
boundary.poly <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
watershed.ids <- unique(as.character(boundary.poly$HUC_6))

# Clean up names 
boundary.poly <- boundary.poly[, c("HUC_8", "HUC_6", "HUC_4", "HUC_2", "geometry")]

# Create the blank data frame that will store the results
boundary.poly$Connect <- NA

for( huc in watershed.ids ) {
  
  # Load a stream layer
  stream.layer <- read_sf(results.list[grep(paste0(huc, "-"), results.list)])
  
  # Calculate connectivity and merge with shapefile
  boundary.poly$Connect[boundary.poly$HUC_6 == huc] <- sum(stream.layer$Connect * stream.layer$Length) / sum(stream.layer$Length)
  
  # Merge stream network
  if (huc == "170502") {
  
    stream.network <- stream.layer
    
  } else {
    
    stream.network <- rbind(stream.layer, stream.network)
    
  }

  
  print(huc)
  
}

# Rename stream IDS
stream.network$StrmID <- 1:nrow(stream.network)

# Save watershed results
write_sf(boundary.poly, dsn = paste0("results/shapefiles/probabilities/huc-6/summaries/watershed-status_", analysis.year, "_", Sys.Date(), ".shp"))

# Save watershed results
write_sf(stream.network, dsn = paste0("results/shapefiles/probabilities/huc-6/summaries/stream-status_", analysis.year, "_", Sys.Date(), ".shp"))

rm(list=ls())
gc()

####################
# Final formatting #
####################

# Clear memory
rm(list=ls())
gc()

# Load libraries and source scripts
library(sf)

# Road stream intersections

# Culverts
sampled.culverts <- read.csv("data/processed/bws-project/culverts/culverts-gis-summaries-aggregate_2021-06-24.csv")

for (watershed in watershed.ids) {
  
  # Load the watershed of interest 
  edge.data <- read.csv(paste0("data/processed/bws-project/predicted/passability/", watershed, "-predicted-culverts_2021-06-24.csv"))
  resistance <- read.csv(paste0("data/processed/bws-project/resistance/", watershed, "-resistance_2021-06-24.csv"))
  culvert.data <- read_sf(paste0("data/processed/bws-project/gis/", watershed, "/Culverts.shp"))
  
  # Subset the sampled culverts by watershed
  watershed.culvert <- sampled.culverts[sampled.culverts$WatershedArea %in% unique(edge.data$WatershedArea), ]
  
  # Remove all culvert info except InterID and geometry
  culvert.data <- culvert.data[, c("InterID", "geometry")]
  
  # Format the resistance data
  resistance$Status <- resistance$Status - resistance[resistance$CulvertID == "complete", "Status"]
  resistance <- resistance[resistance$CulvertID != "complete", ]
  
  # Format the node information to relevant information
  edge.data <- edge.data[, c("TARGET_FID", "Node", "UpstreamSeg", "DownstreamSeg",
                             "Class", "BridgeDate", "Dam", "DamStatus", "DamType", "FeatureType",
                             "Up", "BoundedD", "UnboundedD")]
  edge.data <- edge.data[edge.data$Class != "Split", ]
  
  # Format sampled culverts
  watershed.culvert <- watershed.culvert[, c("TARGET_FID", "InspectionDate")]
  
  # Standardize names and merge
  colnames(culvert.data)[1] <- "Culvert"
  colnames(resistance) <- c("Culvert", "Resist")
  colnames(watershed.culvert) <- c("Culvert", "InsDate")
  colnames(edge.data) <- c("Culvert",  "Node", "Seg1", "Seg2",
                           "Class", "DateB", "Dam", "DamSt", "DamType", "HFType",
                           "Hang", "DistB", "DistUn")
  
  culvert.data <- merge(culvert.data, edge.data, by = "Culvert")
  culvert.data <- merge(culvert.data, resistance, by = "Culvert", all = TRUE)
  culvert.data <- merge(culvert.data, watershed.culvert, by = "Culvert", all = TRUE)
  
  # Organize column names
  culvert.data <- culvert.data[, c("Culvert", "Node", "Seg1", "Seg2",
                                   "Class", "InsDate", "DateB", "Dam", "DamSt", "DamType", "HFType",
                                   "Hang", "Resist", "DistB", "DistUn", "geometry")]
  
  st_write(obj = culvert.data, dsn = paste0("results/bws-project/shapefiles/", watershed, "-road-stream-intersections.shp"))
  
}

# Clear memory
rm(list=ls())
gc()

# Load libraries and source scripts
library(sf)

# Watersheds
data.2010 <- read_sf(paste0("results/shapefiles/probabilities/huc-6/summaries/watershed-status_2010_2021-06-28.shp"))
data.2014 <- read_sf(paste0("results/shapefiles/probabilities/huc-6/summaries/watershed-status_2014_2021-06-28.shp"))
data.2016 <- read_sf(paste0("results/shapefiles/probabilities/huc-6/summaries/watershed-status_2016_2021-06-28.shp"))
data.2018 <- read_sf(paste0("results/shapefiles/probabilities/huc-6/summaries/watershed-status_2018_2021-06-28.shp"))

# Create a single combined results

watershed.connectivity <- data.2010
colnames(watershed.connectivity)[1:5] <- c("HUC8", "HUC6", "HUC4", "HUC2", "Con2010")

data.2014 <- data.2014[, c("HUC_8", "Connect")]
st_geometry(data.2014) <- NULL
colnames(data.2014)[1:2] <- c("HUC8", "Con2014")

watershed.connectivity <- merge(watershed.connectivity, data.2014, by = "HUC8")

data.2016 <- data.2016[, c("HUC_8", "Connect")]
st_geometry(data.2016) <- NULL
colnames(data.2016)[1:2] <- c("HUC8", "Con2016")

watershed.connectivity <- merge(watershed.connectivity, data.2016, by = "HUC8")

data.2018 <- data.2018[, c("HUC_8", "Connect")]
st_geometry(data.2018) <- NULL
colnames(data.2018)[1:2] <- c("HUC8", "Con2018")

watershed.connectivity <- merge(watershed.connectivity, data.2018, by = "HUC8")

# These are teh results presented on GeoDiscover Alberta
st_write(obj = watershed.connectivity, 
         dsn = "results/shapefiles/probabilities/huc-6/summaries/provincial-watershed-connectivity_2021-07-14.shp")

rm(list=ls())
