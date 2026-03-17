# ---
# title: "Standardizing the culvert survey data"
# author: "Brandon Allen"
# created: "2025-01-11"
# inputs: ["0_data/external/culvert-surveys/woodlands-north/woodlands-sampling_2019-11-07.csv";
#          "0_data/external/culvert-surveys/national-park/parks-canada-culverts_2021-04-07.csv";
#          "0_data/external/culvert-surveys/watercourse-crossing-program/inspections_2023-03-31.xlsx";
#          "0_data/external/mapping/provincial-boundary.Rdata";
#          "0_data/external/culvert-surveys/literature/published-literature.csv"]
# outputs: ["2_pipeline/culverts/culvert-surveys-cleaned.Rdata";
#           "2_pipeline/culverts/culvert-model-attributes.Rdata";
#           "2_pipeline/culverts/culvert-surveys-cleaned.shp"]
# notes: 
#   "Standardize the culvert data from various sources in preparation for modeling."
# ---

# 1.0 Standardize culvert survey information ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(ggplot2)
require(ggnewscale)
library(readxl)
library(sf)

# 1.3 Woodlands North surveys ----
woodlands.north <- read.csv("0_data/external/culvert-surveys/woodlands-north/woodlands-sampling_2019-11-07.csv")
woodlands.north$SurveyID <- 1:nrow(woodlands.north)
woodlands.north$SurveyDate <- as.Date("2019-10-01") # Performed fall of 2019
woodlands.north$Project <- "WoodlandsNorth"
woodlands.north <- woodlands.north[, c("SurveyID", "SurveyDate", "Project", "Lat", "Long", "Stream.Class", "Crossing.Type", "Fish.Passage.Concern", "Fish.Passage.Concern.Reason")]
colnames(woodlands.north) <- c("SurveyID", "SurveyDate", "Project", "Latitude", "Longitude", "StreamClass", "CrossingType", "Passability", "PassabilityConcern")
woodlands.north$CrossingType <- "Culvert" # All crossing are culverts, so relabel for alignment with other sources.
woodlands.north$Passability[woodlands.north$Passability == "NULL"] <- NA
woodlands.north$PassabilityConcern[woodlands.north$PassabilityConcern == "NULL"] <- NA
woodlands.north$PassabilityConcern[woodlands.north$PassabilityConcern == "Hanging culvert"] <- "Hanging Culvert"
woodlands.north <- woodlands.north[!is.na(woodlands.north$Passability), ]
woodlands.north <- woodlands.north[!duplicated(woodlands.north[, -1]), ] # Remove duplicated calls
woodlands.north <- woodlands.north[woodlands.north$StreamClass %in% c("Large permanent", "Small permanent"), ]

# 1.4 National Parks surveys ----
national.parks <- read.csv("0_data/external/culvert-surveys/national-park/parks-canada-culverts_2021-04-07.csv")
national.parks$SurveyDate <- as.Date("2008-08-04") # Performed summer of 2008
national.parks$StreamClass <- "Small permanent" # Assumed permanent 
national.parks$Project <- "NationalParks"
national.parks <- national.parks[, c("OBJECTID", "SurveyDate", "Project", "ycoord", "xcoord", "StreamClass", "CROSSINGTY", "Passability", "PassabilityConcern")]
colnames(national.parks) <- c("SurveyID", "SurveyDate", "Project", "Latitude", "Longitude", "StreamClass", "CrossingType", "Passability", "PassabilityConcern")
national.parks <- national.parks[!duplicated(national.parks[, -1]), ] # Remove duplicated calls

# 1.5 Watercourse Crossing Program  surveys (up to 2023) ----
wcp <- read_xlsx("0_data/external//culvert-surveys/watercourse-crossing-program/inspections_2023-03-31.xlsx", sheet = "inspections_2023-03-31")
wcp <- wcp[wcp$POINT_X > -120, ] # Remove stray locations
wcp <- wcp[wcp$POINT_Y > 49, ]
wcp$INSP_DATE <- as.Date(wcp$INSP_DATE) # Keep only inspection dates post 2010
wcp <- wcp[wcp$INSP_DATE > as.Date("2010-01-01"), ] 
wcp <- wcp[-grep("ABMI", wcp$REMARKS), ] # Remove the duplicated ABMI surveys performed by Woodlands North
wcp <- wcp[!is.na(wcp$STR_CLASS), ] # Remove culverts with no stream information
wcp <- wcp[!(wcp$STR_CLASS %in% c("null", "Unknown")), ]
wcp <- wcp[wcp$FISHPCONC %in% c("Concerns", "No Concerns"), ] # Extract only surveys with fish passability
wcp$Passability <- wcp$FISHPCONC
wcp$PassabilityConcern <- wcp$FISHPCONC
wcp$PassabilityConcern[wcp$CV_OUT_TY %in% "Hanging"] <- "Hanging Culvert" # Define fish passability as culvert if culvert outlet is hanging
wcp$CV_OTGAP[is.na(wcp$CV_OTGAP)] <- 0 # If no outlet hap score, assume 0
wcp$CV_OTGAP[wcp$CV_OTGAP == "null"] <- 0
wcp$CV_OTGAP[wcp$CV_OTGAP == "0.10M"] <- 0.10 # Type
wcp$CV_OTGAP <- as.numeric(wcp$CV_OTGAP)
wcp$PassabilityConcern[wcp$PassabilityConcern %in% "Hanging Culvert" & wcp$CV_OTGAP < 10] <- "Concerns" # If hang is less than 10cm, classify as normal blockage 
wcp$BLOCKAGE[is.na(wcp$BLOCKAGE)] <- "No" # General blockages
wcp$PassabilityConcern[wcp$PassabilityConcern %in% "Hanging Culvert" & wcp$BLOCKAGE %in% c("Potential", "Yes")] <- "Concerns" 
wcp$BLK_BEAVR[is.na(wcp$BLK_BEAVR)] <- "No" # Beaver blockages
wcp$PassabilityConcern[wcp$PassabilityConcern %in% "Hanging Culvert" & !(wcp$BLK_BEAVR %in% c("no", "No", "null"))] <- "Concerns" 
wcp$BLK_TRASH[is.na(wcp$BLK_TRASH)] <- "No" # Trash blockages
wcp$PassabilityConcern[wcp$PassabilityConcern %in% "Hanging Culvert" & !(wcp$BLK_TRASH %in% c("no", "No", "null"))] <- "Concerns"
wcp$Project <- "WCP"
wcp <- wcp[, c("FID", "INSP_DATE", "Project", "POINT_Y", "POINT_X", "STR_CLASS", "CROSS_TYPE", "Passability", "PassabilityConcern")] # Filter survey columns
colnames(wcp) <- c("SurveyID", "SurveyDate", "Project", "Latitude", "Longitude", "StreamClass", "CrossingType", "Passability", "PassabilityConcern")
wcp <- wcp[!duplicated(wcp[, -1]), ] # Remove duplicated calls

# 1.6 Watercourse Crossing Program  surveys (Post 2023-01-24) ----
wcp.new <- read_xlsx("0_data/external/culvert-surveys/watercourse-crossing-program/2024/inspections_2025-01-13.xlsx", sheet = "Inspections_short.shp")
wcp.new <- wcp.new[wcp.new$POINT_X > -120, ] # Remove stray locations
wcp.new <- wcp.new[wcp.new$POINT_Y > 49, ]
wcp.new$INSP_DATE <- as.Date(wcp.new$INSP_DATE) # Keep only inspection dates post "2023-01-24"
wcp.new <- wcp.new[wcp.new$INSP_DATE > "2023-01-24", ]
wcp.new <- wcp.new[wcp.new$INSP_DATE != "2101-04-08", ]
wcp.new <- wcp.new[!is.na(wcp.new$FISHPCONC), ] # Keep only inspections with fish passability information
wcp.new <- wcp.new[!is.na(wcp.new$FISHPCONC), ] # Keep only information on culverts
wcp.new <- wcp.new[wcp.new$FISHPCONC %in% c("Concerns", "No Concerns"), ]
wcp.new$Project <- "WCP"
wcp.new$STR_CLASS <- NA
wcp.new$Passability <- wcp.new$FISHPCONC # If "Hanging" is found in the comments field, treat it as such and override the passability field
wcp.new$PassabilityConcern <- NA
wcp.new$PassabilityConcern[grep("Hanging", wcp.new$REMARKS)] <- "Hanging Culvert"
wcp.new$Passability[wcp.new$PassabilityConcern == "Hanging Culvert" & !is.na(wcp.new$PassabilityConcern)] <- "Concerns"
wcp.new <- wcp.new[, c("FID", "INSP_DATE", "Project", "POINT_Y", "POINT_X", "STR_CLASS", "CROSS_TYPE", "Passability", "PassabilityConcern")] # Filter survey columns
colnames(wcp.new) <- c("SurveyID", "SurveyDate", "Project", "Latitude", "Longitude", "StreamClass", "CrossingType", "Passability", "PassabilityConcern")
wcp.new <- wcp.new[!duplicated(wcp.new[, -1]), ] # Remove duplicated calls

# 1.7 Published Literature surveys ----
published.surveys <- read.csv("0_data/external/culvert-surveys/literature/published-literature.csv")
published.surveys$InspectionDate <- as.Date(published.surveys$InspectionDate, format = "%m/%d/%Y")
published.surveys <- published.surveys[, c("CulvertID", "InspectionDate", "Project", "Lat", "Long", "StreamType", "CulvertType", "Passability", "PassabilityConcern")]
colnames(published.surveys) <- c("SurveyID", "SurveyDate", "Project", "Latitude", "Longitude", "StreamClass", "CrossingType", "Passability", "PassabilityConcern")
published.surveys <- published.surveys[!is.na(published.surveys$Latitude), ] # Remove one survey without location

# 1.8 Stitch the data together, remove blank surveys, and save ----
culvert.data <- rbind.data.frame(woodlands.north, national.parks)
culvert.data <- rbind.data.frame(culvert.data, published.surveys)
culvert.data <- rbind.data.frame(culvert.data, wcp)
culvert.data <- rbind.data.frame(culvert.data, wcp.new)
culvert.data <- culvert.data[!(culvert.data$CrossingType %in% c("No crossing present", "Unknown")), ]
comment(culvert.data) <- "Culvert data was cleaned and filtered on April 8th, 2025"
save(culvert.data, file = "2_pipeline/culverts/culvert-surveys-cleaned.Rdata")

# 1.9 Visualization ----
load("0_data/external/mapping/provincial-boundary.Rdata")

# Create basic figure structure
culvert.geom <- st_as_sf(x = culvert.data,                         
               coords = c("Longitude", "Latitude"),
               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

occ.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
        scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
        new_scale_color() +
        new_scale_fill() +
        geom_sf(data = culvert.geom, show.legend = FALSE)

ggsave(filename = "3_output/figures/culvert-surveys.jpeg",
       plot = occ.plot,
       height = 800,
       width = 600,
       dpi = 72,
       quality = 100,
       units = "px")


# 2.0 Extract surveys from matching inventory ----

# 2.1 Clear memory ----
rm(list=ls())
gc()

# 2.2 Load libraries and source data ----
library(foreign)
library(reticulate)
library(sf)
load("2_pipeline/culverts/culvert-surveys-cleaned.Rdata")

# 2.3 Initialize arcpy ----
# py_discover_config() # We need version 3.9
# py_config() # Double check it is version 3.9

# Set python 
use_python(python = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe")

# Load arcpy
arcpy <- import('arcpy') 

# Define parallel processing factor
arcpy$env$parallelProcessingFactor <- "100%"

# 2.4 Identify target year for extracting culvert information ----
culvert.data$YearGroup <- format(culvert.data$SurveyDate,"%Y")
culvert.data$YearGroup[culvert.data$YearGroup %in% c(2003:2010)] <- 2010
culvert.data$YearGroup[culvert.data$YearGroup %in% c(2011:2013)] <- 2014
culvert.data$YearGroup[culvert.data$YearGroup %in% 2015] <- 2016
culvert.data$YearGroup[culvert.data$YearGroup %in% 2017] <- 2018
culvert.data$YearGroup[culvert.data$YearGroup %in% c(2022, 2023, 2024)] <- 2022

# 2.5 Create the spatial file for matching ----
culvert.spatial <- st_as_sf(x = culvert.data, coords = c("Longitude", "Latitude"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
culvert.spatial <- st_transform(culvert.spatial, crs = st_crs(3400))
write_sf(culvert.spatial, dsn = "2_pipeline/culverts/culvert-surveys-cleaned.shp")

# 2.6 Loop through each watershed using the most recent HFI inventory ----
watershed.ids <- read.dbf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
huc.scale <- 6
hfi.year <- sort(unique(culvert.data$YearGroup))
culvert.attributes <- NULL
search.radius <- "100 Meters"

for (year in hfi.year) {
        
        for(HUC in watershed.ids) {
                
                # Load the appropriate Rdata with the culvert information
                load(paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                            year, "/connectivity/network_", HUC, ".Rdata"))
                
                # If there are culverts in the watershed, proceed with the match
                if(is.null(watershed.network[["Edge_Cleaned"]])) {
                        
                        next()
                        
                }
                
                # Define the workspace
                arcpy$env$workspace <- paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                              year, "/gis/", HUC, ".gdb")
                
                # Clip culvert to the boundary of interest
                arcpy$PairwiseClip_analysis(in_features = paste0(getwd(), "/2_pipeline/culverts/culvert-surveys-cleaned.shp"),  
                                            clip_features = "watershed_boundary", 
                                            out_feature_class = "survey_temp.shp")
                
                # Join the two culvert data sets
                arcpy$SpatialJoin_analysis(join_features = "Culverts", 
                                           target_features = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                  year, "/gis/survey_temp.shp"), 
                                           join_operation = "JOIN_ONE_TO_MANY", 
                                           join_type = "KEEP_COMMON",
                                           match_option = "CLOSEST_GEODESIC",
                                           search_radius = search.radius,
                                           out_feature_class = "Culverts_temp.shp", 
                                           distance_field_name = "Distance")
                
                # Read the data in culvert data and merge with Rdata file
                matching.culverts <- read.dbf(paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                     year, "/gis/Culverts_temp.dbf"))
                
                # Subset to include only the years up until the focal year (i.e., completed surveys)
                matching.culverts <- matching.culverts[as.numeric(as.character(matching.culverts$YearGrp)) <= year, ]
                
                # Add catch for no surveys in the watershed
                if(nrow(matching.culverts) == 0) {
                        
                        # Remove the temporary data
                        arcpy$Delete_management(in_data = c( paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                    year, "/gis/survey_temp.shp"),
                                                             paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                    year, "/gis/Culverts_temp.shp")))
                        
                        print(HUC)
                        
                        # Skip
                        next()
                        
                }
                
                matching.culverts$Node <- paste0(matching.culverts$StreamID_1, "-", matching.culverts$StreamID)
                matching.culverts <- matching.culverts[, c("Node", "Project", "SurvyDt", "StrmCls", "CrssngT", "Pssblty", "PssbltC", "YearGrp", "SurvyID", "Distance")]
                colnames(matching.culverts) <- c("Node", "Project","SurveyDate", "StreamClass", "CrossingType", "Passability", "PassabilityConcern", "YearGroup", "SurveyID", "SurveyDistance")
                
                # Each culvert is assigned the closest feature. However, multiple culverts
                # may be assigned.
                
                # There will be some culverts with multiple potential surveys. 
                # Use the closest and most recent culvert survey within the viable subset.
                
                duplicated.surveys <- unique(matching.culverts$Node[duplicated(matching.culverts$Node)])
                
                for(survey in duplicated.surveys) {
                        
                        # Store copy of surveys
                        copy.surveys <- matching.culverts[matching.culverts$Node == survey, ]
                        
                        # Remove from main file
                        matching.culverts <- matching.culverts[matching.culverts$Node != survey, ]
                        
                        # Isolate the closest survey
                        copy.surveys <- copy.surveys[copy.surveys$SurveyDistance == min(copy.surveys$SurveyDistance), ]
                        
                        # If multiple surveys with the same distance, sort by date and pick the most recent
                        copy.surveys <- copy.surveys[order(copy.surveys$SurveyDate, decreasing = TRUE), ]
                        copy.surveys <- copy.surveys[1, ]
                        
                        # Rebind to the data
                        matching.culverts <- rbind(matching.culverts, copy.surveys)
                        
                }
                
                # Merge with the edge data
                # Saving to a separate data frame so we can adjust the matching process
                watershed.network$Edge_Surveys <- merge.data.frame(watershed.network$Edge_Cleaned, 
                                                                   matching.culverts, by = "Node", all = TRUE)
                
                save(watershed.network, file = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/",
                                                      year, "/connectivity/network_", HUC, ".Rdata"))
                
                # Create subset for culvert modeling
                # This needs to avoid duplicate years. So only select culverts within the matching YearGroup
                culvert.temp <- watershed.network$Edge_Surveys[!is.na(watershed.network$Edge_Surveys$SurveyDate), ]
                culvert.temp$HUC <- HUC
                culvert.temp <- culvert.temp[culvert.temp$YearGroup == year, ]

                culvert.attributes <- rbind(culvert.attributes, culvert.temp)
                
                rm(culvert.temp)
                
                # Remove the temporary data
                arcpy$Delete_management(in_data = c( paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                            year, "/gis/survey_temp.shp"),
                                                     paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                            year, "/gis/Culverts_temp.shp")))
                
                print(HUC)
                
        }
        
        print(year)
        
}

# 2.7 If culverts have multiple surveys, pick the most recent year for this modeling process ----
duplicated.surveys <- unique(culvert.attributes$Node[duplicated(paste0(culvert.attributes$Node, "_",
                                                                       culvert.attributes$HUC))])
for(survey in duplicated.surveys) {
        
        # Store copy of surveys
        copy.surveys <- culvert.attributes[culvert.attributes$Node == survey, ]
        
        # Remove from main file
        culvert.attributes <- culvert.attributes[culvert.attributes$Node != survey, ]
        
        # Isolate most recent survey. If multiple, pick the first one
        copy.surveys <- copy.surveys[copy.surveys$SurveyDate == max(copy.surveys$SurveyDate), ]
        copy.surveys <- copy.surveys[1, ]
        
        # Rebind to the data
        culvert.attributes <- rbind(culvert.attributes, copy.surveys)
        
        rm(copy.surveys)
        
}

# 2.8 Save results
comment(culvert.attributes) <- "Culvert data was matched with GIS attributes on June 20th, 2025"
save(culvert.attributes, file = "2_pipeline/culverts/culvert-model-attributes.Rdata")

rm(list=ls())
gc()
