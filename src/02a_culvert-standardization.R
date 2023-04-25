#
# Title: Standardizing the culvert data
# Created: March 31st, 2023
# Last Updated: April 24th, 2023
# Author: Brandon Allen
# Objectives: Standardize the culvert data from various sources in preparation for modeling
# Keywords: Notes, Standardization, Extraction from matching, Culvert Predictions
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
###################
# Standardization # Clean and stitch surveys together
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(readxl)
library(sf)

#
# Woodlands North
#

woodlands.north <- read.csv("data/base/culvert-surveys/woodlands-north/woodlands-sampling_2019-11-07.csv")
woodlands.north$SurveyID <- 1:nrow(woodlands.north)
woodlands.north$SurveyDate <- as.Date("2019-10-01") # Performed fall of 2019
woodlands.north <- woodlands.north[, c("SurveyID", "SurveyDate", "Lat", "Long", "Fish.Passage.Concern", "Fish.Passage.Concern.Reason")]
colnames(woodlands.north) <- c("SurveyID", "SurveyDate", "Latitude", "Longitude", "Passability", "PassabilityConcern")
woodlands.north$Passability[woodlands.north$Passability == "NULL"] <- NA
woodlands.north$PassabilityConcern[woodlands.north$PassabilityConcern == "NULL"] <- NA
woodlands.north$PassabilityConcern[woodlands.north$PassabilityConcern == "Hanging culvert"] <- "Hanging Culvert"
woodlands.north <- woodlands.north[!is.na(woodlands.north$Passability), ]
woodlands.north <- woodlands.north[!duplicated(woodlands.north[, -1]), ] # Remove duplicated calls

#
# National Parks
#

national.parks <- read.csv("data/base/culvert-surveys/national-park/parks-canada-culverts_2021-04-07.csv")
national.parks$SurveyDate <- as.Date("2008-08-04") # Performed summer of 2008
national.parks <- national.parks[, c("OBJECTID", "SurveyDate", "ycoord", "xcoord", "Passability", "PassabilityConcern")]
colnames(national.parks) <- c("SurveyID", "SurveyDate", "Latitude", "Longitude", "Passability", "PassabilityConcern")
national.parks <- national.parks[!duplicated(national.parks[, -1]), ] # Remove duplicated calls

#
# Watercourse Crossing Program
# 

wcp <- read_xlsx("data/base/culvert-surveys/watercourse-crossing-program/inspections_2023-03-31.xlsx", sheet = "inspections_2023-03-31")
wcp <- wcp[wcp$POINT_X > -120, ] # Remove stray locations
wcp <- wcp[wcp$POINT_Y > 49, ]
wcp$INSP_DATE <- as.Date(wcp$INSP_DATE) # Keep only inspection dates post 2010
wcp <- wcp[wcp$INSP_DATE > as.Date("2010-01-01"), ] 
wcp <- wcp[-grep("ABMI", wcp$REMARKS), ] # Remove the duplicated ABMI surveys
wcp$Reason[wcp$CV_OUT_TY == "Hanging"] <- "Hanging Culvert" # If we have a culvert outlet type call, define the reason as Hanging
wcp$Passability <- wcp$FISHPCONC 
wcp$Passability[wcp$Reason != "NA"] <- "Serious Concerns" # If we have a reason for fish passability concern, update the original FISHPCONC call to "Serious Concerns"
wcp$Passability[wcp$Passability %in% c("concern", "Concerns", "inadequate / unsatisfactory", "y")] <- "Some Concerns" # Create the "Some concerns" class
wcp$Passability[wcp$Reason == "Hanging Culvert"] <- "Serious Concerns" # If culvert is hanging, classify it has Serious Concerns
wcp <- wcp[!is.na(wcp$Passability), ] # Remove culverts with NA passability calls, null, or unknown
wcp <- wcp[wcp$Passability %in% c("No Concerns", "Some Concerns", "Serious Concerns"), ] 
wcp$Reason[wcp$Reason == "NA"] <- NA
wcp <- wcp[, c("FID", "INSP_DATE", "POINT_Y", "POINT_X", "Passability", "Reason")] # Filter survey columns
colnames(wcp) <- c("SurveyID", "SurveyDate", "Latitude", "Longitude", "Passability", "PassabilityConcern")
wcp <- wcp[!duplicated(wcp[, -1]), ] # Remove duplicated calls

# Stitch the data together
culvert.data <- rbind.data.frame(woodlands.north, national.parks)
culvert.data <- rbind.data.frame(culvert.data, wcp)

# Save as R object and spatial file
comment(culvert.data) <- "Culvert data was cleaned and filtered on April 5th, 2023"
save(culvert.data, file = "data/processed/culverts/culvert-surveys-cleaned.Rdata")
rm(list=ls())
gc()

############################
# Extraction from matching # Extracts need to occur for each version of the HFI analyzed
############################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(reticulate)
library(sf)

# Load the culvert data
load("data/processed/culverts/culvert-surveys-cleaned.Rdata")

# Initialize reticulate to communicate with ArcPro
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python version
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# If there are locations with multiple samples (duplicated lat/long combinations) select the most recent year
# If there multiple surveys were done at the same location, pick the first record
duplicate.list <- culvert.data[duplicated(culvert.data[, c("Latitude", "Longitude")]), ]
duplicate.list <- duplicate.list[!duplicated(duplicate.list[, c("Latitude", "Longitude")]), ]

for (location.id in 1:nrow(duplicate.list)) {
        
        # Pull out all values with matching lat/long
        duplicate.point <- culvert.data[culvert.data$Latitude == duplicate.list$Latitude[location.id] & culvert.data$Longitude == duplicate.list$Longitude[location.id], ]
        
        # Identify which to remove (oldest records)
        duplicate.point <- duplicate.point[duplicate.point$SurveyDate != max(duplicate.point$SurveyDate),]
        
        # If we have no points, it means there were multiple surveys at the location
        if(nrow(duplicate.point) == 0) {
               
                # Pull out all values with matching lat/long
                duplicate.point <- culvert.data[culvert.data$Latitude == duplicate.list$Latitude[location.id] & culvert.data$Longitude == duplicate.list$Longitude[location.id], ]
                
                
        } 
        
        # In case there were multiple values
        duplicate.point <- duplicate.point[1, ] # Pick first record

        # Remove duplicates from base geometry
        culvert.data <- culvert.data[!(culvert.data$SurveyID %in% duplicate.point$SurveyID), ]
        
}

# Create the spatial file
head(culvert.data)
culvert.spatial <- st_as_sf(x = culvert.data, coords = c("Longitude", "Latitude"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
culvert.spatial <- st_transform(culvert.spatial, crs = st_crs(3400))
write_sf(culvert.spatial, dsn = "data/processed/culverts/culvert-surveys-cleaned.shp")

# Loop through each watershed
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
huc.scale <- 6
hfi.year <- 2018
culvert.attributes <- NULL

for(HUC in watershed.ids) {
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                    hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        # If there are culverts in the watershed, proceed with the match
        if(is.null(watershed.network[["Edge_Cleaned"]])) {
                
                next()
                
        }
        
        # Define the workspace
        arcpy$env$workspace <- paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                      hfi.year, "/gis/", HUC, ".gdb")
        
        # Clip culvert to the boundary of interest
        arcpy$PairwiseClip_analysis(in_features = paste0(getwd(), "/data/processed/culverts/culvert-surveys-cleaned.shp"),  
                                    clip_features = "watershed_boundary", 
                                    out_feature_class = "survey_temp.shp")
        
        # Join the two culvert data sets
        arcpy$SpatialJoin_analysis(target_features = "Culverts", 
                                   join_features = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                          hfi.year, "/gis/survey_temp.shp"), 
                                   join_operation = "JOIN_ONE_TO_ONE", 
                                   join_type = "KEEP_COMMON",
                                   match_option = "WITHIN_A_DISTANCE_GEODESIC",
                                   search_radius = "50 Meters",
                                   out_feature_class = "Culverts_temp.shp")
        
        # Read the data in culvert data and merge with Rdata file
        matching.culverts <- read.dbf(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                             hfi.year, "/gis/Culverts_temp.dbf"))
        
        # Add catch for no surveys in the watershed
        if(nrow(matching.culverts) == 0) {
                
                # Remove the temporary data
                arcpy$Delete_management(in_data = c( paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                            hfi.year, "/gis/survey_temp.shp"),
                                                     paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                            hfi.year, "/gis/Culverts_temp.shp")))
                
                print(HUC)
                
                # Skip
                next()
                
        }
        
        matching.culverts$Node <- paste0(matching.culverts$StreamID_1, "-", matching.culverts$StreamID)
        matching.culverts <- matching.culverts[, c("Node", "SurvyDt", "Pssblty", "PssbltC")]
        colnames(matching.culverts) <- c("Node", "SurveyDate", "Passability", "PassabilityConcern")
        watershed.network$Edge_Cleaned <- merge.data.frame(watershed.network$Edge_Cleaned, 
                                                           matching.culverts, by = "Node", all = TRUE)
        
        save(watershed.network, file = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                              hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        # Create subset for culvert modeling
        culvert.temp <- watershed.network$Edge_Cleaned[!is.na(watershed.network$Edge_Cleaned$SurveyDate), ]
        culvert.temp$HUC <- HUC
        
        culvert.attributes <- rbind(culvert.attributes, culvert.temp)
        
        rm(culvert.temp)

        # Remove the temporary data
        arcpy$Delete_management(in_data = c( paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                    hfi.year, "/gis/survey_temp.shp"),
                                             paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                    hfi.year, "/gis/Culverts_temp.shp")))
        
        print(HUC)
        
}

comment(culvert.attributes) <- "Culvert data was matched with GIS attributes on April 24th, 2023"
save(culvert.attributes, file = "data/processed/culverts/culvert-model-attributes.Rdata")
