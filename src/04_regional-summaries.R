#
# Title: Merging culvert results
# Created: September 1st, 2021
# Last Updated: May 12th, 2021
# Author: Brandon Allen
# Objectives: Merging the predicted passability scores into a single file and merge with the appropriate shapefiles.
# Keywords: Notes, Data Merging, Indicator Summary
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
################
# Data Merging # Need to think of a new way to merge the results with the geodatabases
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####################
# Indicator Summary #
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(MetBrewer)
library(sf)
library(rgdal)

# Definet the analysis year
hfi.year <- 2010
huc.scale <- 6

# Identify the watershed list
results.list <- list.files(paste0("data/processed/huc-", huc.scale, "/", 
                           hfi.year, "/connectivity/"), full.names = TRUE)

# Load the watershed boundaries
boundary.poly <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
watershed.ids <- unique(as.character(boundary.poly$HUC_6))

# Clean up names 
boundary.poly <- boundary.poly[, c("HUC_8", "HUC_6", "HUC_4", "HUC_2", "geometry")]

# Create the blank data frame that will store the results
boundary.poly$Connect <- NA

for(HUC in watershed.ids) {
        
        # Load a stream layer
        load(results.list[grep(HUC, results.list)])
        
        # If no connectivity results, default to 100%
        if(is.null(watershed.network[["Connectivity"]])) {
                
                boundary.poly$Connect[boundary.poly$HUC_6 == HUC] <- 100
                
        } else {
                
                # Subset connectivity results
                connecitivty.results <- watershed.network[["Connectivity"]]
                
                # Calculate connectivity and merge with shapefile
                boundary.poly$Connect[boundary.poly$HUC_6 == HUC] <- 100 * (sum(connecitivty.results$StreamConnect * 
                                                                                 connecitivty.results$StreamLength) / sum(connecitivty.results$StreamLength))
                rm(connecitivty.results)
                
        }
        
        print(HUC)
        rm(watershed.network)
 
}

# Save watershed results
write_sf(boundary.poly, dsn = paste0("results/shapefiles/summaries/favorability/watershed_status_", hfi.year, ".shp"))

rm(list=ls())
gc()

##################################
# Comparison to previous version #
##################################

new.results.2018 <- read_sf(paste0("results/shapefiles/summaries/favorability/watershed_status_2018.shp"))
new.results.2010 <- read_sf(paste0("results/shapefiles/summaries/favorability/watershed_status_2010.shp"))
old.results <- read_sf(paste0("results/shapefiles/old/provincial-watershed-connectivity_2021-07-14.shp"))
old.results$Con2018 <- old.results$Con2018 * 100
old.results$Con2010 <- old.results$Con2010 * 100
colnames(new.results.2018)[c(1, 5)] <- c("HUC8", "Connect2018")
colnames(new.results.2010)[c(1, 5)] <- c("HUC8", "Connect2010")

combined <- merge.data.frame(new.results.2018, old.results, by = "HUC8")
combined <- merge.data.frame(combined, new.results.2010, by = "HUC8")

combined.plot <- ggplot(combined, aes(x = Con2018, y = Connect2018)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlim(c(0,100)) +
        ylim(c(0,100)) +
        labs(x = "Connectivity (version 1)", y = "Connectivity (version 2)") +
        ggtitle(paste0("Correlation = ", round(cor(combined$Connect2018, combined$Con2018), 2))) +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "results/figures/version-comparison-2018HFI.jpeg",
       plot = combined.plot,
       height = 800,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")

combined.plot <- ggplot(combined, aes(x = Con2010, y = Connect2010)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlim(c(0,100)) +
        ylim(c(0,100)) +
        labs(x = "Connectivity (version 1)", y = "Connectivity (version 2)") +
        ggtitle(paste0("Correlation = ", round(cor(combined$Connect2010, combined$Con2010), 2))) +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "results/figures/version-comparison-2010HFI.jpeg",
       plot = combined.plot,
       height = 800,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")

version.1.plot <- ggplot() + 
        geom_sf(data = old.results, aes(color = Con2018, fill = Con2018), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "none") +
        ggtitle("Version 1") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

ggsave(filename = "results/figures/version-1-2018HFI.jpeg",
       plot = version.1.plot,
       height = 1200,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

version.2.plot <- ggplot() + 
        geom_sf(data = new.results.2018, aes(color = Connect2018, fill = Connect2018), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "none") +
        ggtitle("Version 2") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

ggsave(filename = "results/figures/version-2-2018HFI.jpeg",
       plot = version.2.plot,
       height = 1200,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

rm(list=ls())
gc()

####################
# Final formatting # OLD
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
