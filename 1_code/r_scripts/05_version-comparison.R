# ---
# title: "Version comparison"
# author: "Brandon Allen"
# created: "2025-01-21"
# inputs: ["3_output/summaries/shapefiles/connectivity_hfi_2018.shp";
#          "3_output/shapefiles/previous-version/provincial-watershed-connectivity_2021-07-14.shp"]
# outputs: ["3_output/figures/spatial-version-comparison-2018HFI.jpeg";
#           "3_output/figures/version-comparison-2018HFI.jpeg";
#           "3_output/figures/version-temporal-change-2018HFI.jpeg";
#           "3_output/figures/survey-effort-comparison.jpeg";
#           "3_output/figures/new-culvert-data.jpeg";
#           "3_output/figures/connectivity-difference.jpeg"
#           "3_output/figures/connectivity-stream.jpeg"
#           "3_output/figures/resistance-culvert.jpeg"
#           "3_output/figures/resistance-location.jpeg"]
# notes: 
#   "This script performs the comparison between the current version and any previous version of the indicator.
#    We use the 2018 HFI as our "baseline" year as it is available for all versions and has some culvert data available for it".
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries and functions ----
library(ggplot2)
require(ggnewscale)
library(ggpubr)
library(igraph)
library(MetBrewer)
library(sf)
source("1_code/r_scripts/connectivity-status_functions.R")

# 1.3 Load shapefiles and standardize ----
original.version <- read_sf("3_output/shapefiles/previous-version/provincial-watershed-connectivity_2021-07-14.shp")
updated.2010 <- read_sf("3_output/shapefiles/summaries/watershed_status_2010.shp")
updated.2018 <- read_sf("3_output/shapefiles/summaries/watershed_status_2018.shp")

original.version$Con2010V1 <- original.version$Con2010 * 100
original.version$Con2018V1 <- original.version$Con2018 * 100
colnames(original.version)[1] <- "HUC_8"
updated.2010$Con2010V2 <- updated.2010$Connect
updated.2018$Con2018V2 <- updated.2018$Connect

combined.version <- merge(updated.2010, as.data.frame(original.version)[, c("HUC_8", "Con2010V1", "Con2018V1")], by = "HUC_8")
combined.version <- merge(combined.version, as.data.frame(updated.2018)[ , c("HUC_8", "Con2018V2")], by = "HUC_8")

combined.version$NewDifference <- combined.version$Con2018V2 - combined.version$Con2010V2
combined.version$OldDifference <- combined.version$Con2018V1 - combined.version$Con2010V1
combined.version$AcrossDifference <- combined.version$Con2018V2 - combined.version$Con2018V1

# 1.4 Load culvert information to help inform change assessment ----
load("2_pipeline/culverts/culvert-model-attributes.Rdata") # New effort
survey.effort <- as.data.frame(table(culvert.attributes$HUC))
colnames(survey.effort) <- c("HUC_6", "Effort")
old.survey <- read.csv("3_output/hanging-culvert-model/version-1/culverts-gis-summaries-aggregate_2021-06-21.csv")
old.survey <- as.data.frame(table(old.survey$HUC6))
colnames(old.survey) <- c("HUC_6", "OldEffort")
old.survey$HUC_6 <- as.character(old.survey$HUC_6)
old.survey$HUC_6[1:11] <- paste0("0", old.survey$HUC_6[1:11])
combined.version <- merge(combined.version, survey.effort, by = "HUC_6", all = TRUE)
combined.version$Effort[is.na(combined.version$Effort)] <- 0
combined.version <- merge(combined.version, old.survey, by = "HUC_6", all = TRUE)
combined.version$OldEffort[is.na(combined.version$OldEffort)] <- 0
combined.version$EffortDifference <- combined.version$Effort - combined.version$OldEffort

# 2.0 Visualization ----

# 2.1 Non-spatial ----
combined.plot <- ggplot(combined.version, aes(x = Con2018V1, y = Con2018V2)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlim(c(0,100)) +
        ylim(c(0,100)) +
        labs(x = "Connectivity (version 1)", y = "Connectivity (version 2)") +
        ggtitle(paste0("Correlation = ", round(cor(combined.version$Con2018V1, combined.version$Con2018V2), 3))) +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "3_output/figures/version-comparison-2018HFI_update.jpeg",
       plot = combined.plot,
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

# 2.2. Change over time ----

change.plot <- ggplot(combined.version, aes(x = OldDifference, y = NewDifference)) + 
        geom_point() +
        geom_abline(slope = 1) +
        xlim(c(-15,15)) +
        ylim(c(-15,15)) +
        labs(x = "Connectivity (version 1)", y = "Connectivity (version 2)") +
        ggtitle(paste0("Correlation = ", round(cor(combined.version$OldDifference, combined.version$NewDifference), 3))) +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "3_output/figures/version-temporal-change-2018HFI_update.jpeg",
       plot = change.plot,
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

# 2.3 Spatial ----

version.1.plot <- ggplot() + 
        geom_sf(data = combined.version, aes(color = Con2018V1, fill = Con2018V1), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "none") +
        ggtitle("Version 1") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

version.2.plot <- ggplot() + 
        geom_sf(data = combined.version, aes(color = Con2018V2, fill = Con2018V2), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "none") +
        ggtitle("Version 2") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

version.difference.plot <- ggplot() + 
        geom_sf(data = combined.version, aes(color = AcrossDifference, fill = AcrossDifference), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Delta Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(-50,50), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(-50,50), guide = "none") +
        ggtitle("Difference") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

ggsave(filename = "3_output/figures/spatial-difference-2018HFI_update.jpeg",
       plot = ggarrange(version.1.plot, version.2.plot, version.difference.plot, ncol = 3),
       height = 1200,
       width = 2400,
       dpi = 72,
       quality = 100,
       units = "px")

# 2.4 Survey Effort ----

survey.plot <- ggplot(combined.version, aes(x = EffortDifference, y = AcrossDifference)) + 
        geom_point() +
        xlim(c(0,600)) +
        ylim(c(-35, 35)) +
        labs(x = "New Culvert Surveys", y = "Delta Connectivity") +
        ggtitle("Connectivity versus Survey Effort Increase") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(), 
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))

ggsave(filename = "3_output/figures/survey-effort-comparison_update.jpeg",
       plot = survey.plot,
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

# 3.0 Visualizations of all barrier types ----
load("0_data/external/mapping/provincial-boundary.Rdata")

# Bridge 
bridge.np <- read_sf("0_data/external/bridges/access-layer/road-bridges-np_2020.shp")
bridge.rail <- read_sf("0_data/external/bridges/access-layer/railway-bridges-np_2020.shp")
bridge.ab <- read_sf("0_data/external/bridges/alberta-transportation/Bridges 2019.shp")

bridge.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.15)) +
        labs(fill = "Natural Region") +
        ggtitle("Bridges") +
        new_scale_fill() +
        geom_sf(data = bridge.np,show.legend = FALSE) +
        new_scale_fill() +
        geom_sf(data = bridge.rail, show.legend = FALSE) +
        new_scale_fill() +
        geom_sf(data = bridge.ab, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

# Culverts
load("2_pipeline/culverts/culvert-surveys-cleaned.Rdata")
culvert.data <- culvert.data[!(culvert.data$Longitude < -116 & culvert.data$Latitude < 50), ]
culvert.geom <- st_as_sf(x = culvert.data,                         
                         coords = c("Longitude", "Latitude"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

culvert.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
        labs(fill = "Natural Region") +
        ggtitle("Watercourse Crossings") +
        new_scale_fill() +
        geom_sf(data = culvert.geom, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

# Old culverts
old.culverts <- read_sf("3_output/hanging-culvert-model/version-1/sampled-culverts_EPSG-3400_2021-05-05.shp")
old.culverts <- old.culverts[!(old.culverts$Long < -116 & old.culverts$Lat < 50), ]
old.culvert.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.15)) +
        labs(fill = "Natural Region") +
        ggtitle("Version 1.0 Crossings") +
        new_scale_fill() +
        geom_sf(data = old.culverts, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

# Mineable
minable.shape <- read_sf("0_data/external/minable/MINEABLE_OIL_SANDS_SCHEME_APPROVALS_MASTER_1.shp")

minable.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.15)) +
        labs(fill = "Natural Region") +
        ggtitle("Minable Region") +
        new_scale_fill() +
        geom_sf(data = minable.shape, linewidth = 1, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))


# Dam
dam.shape <- read_sf("0_data/external/dams/Alberta_Dams_3400_2021-05-11.shp")

dam.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.15)) +
        ggtitle("Dams") +
        labs(fill = "Natural Region") +
        new_scale_fill() +
        geom_sf(data = dam.shape, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/intersection-data.jpeg",
       plot = ggarrange(bridge.plot, dam.plot,
                        culvert.plot, minable.plot, nrow = 2, ncol = 2),
       height = 1600,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")

culvert.plot <- ggplot() + 
        geom_sf(data = province.shapefile, aes(fill = NRNAME), show.legend = TRUE) +
        scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
        labs(fill = "Natural Region") +
        ggtitle("Version 2.0 Crossings") +
        new_scale_fill() +
        geom_sf(data = culvert.geom, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              legend.position = "inside", 
              legend.position.inside = c(0.22, 0.1),
              legend.box.background = element_rect(colour = "black"),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/new-culvert-data.jpeg",
       plot = ggarrange(old.culvert.plot, culvert.plot, nrow = 1, ncol = 2),
       height = 800,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")

# 4.0 Visualizations of 2010 and 2022 results----

updated.2010 <- read_sf("3_output/shapefiles/summaries/watershed_status_2010.shp")
updated.2022 <- read_sf("3_output/shapefiles/summaries/watershed_status_2022.shp")

connect.2010 <- ggplot() + 
        geom_sf(data = updated.2010, aes(color = Connect, fill = Connect), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = "#000000", guide = "none") +
        ggtitle("2010") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))


connect.2022 <- ggplot() + 
        geom_sf(data = updated.2022, aes(color = Connect, fill = Connect), show.legend = TRUE) +
        scale_fill_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(colors = "#000000", guide = "none") +
        ggtitle("2022") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/connectivity-summary.jpeg",
       plot = ggarrange(connect.2010, connect.2022, nrow = 1, ncol = 2),
       height = 800,
       width = 1200,
       dpi = 72,
       quality = 100,
       units = "px")

updated.2022$Difference <- updated.2022$Connect - updated.2010$Connect

connect.diff <- ggplot() + 
        geom_sf(data = updated.2022, aes(color = Difference, fill = Difference), show.legend = TRUE) +
        scale_fill_gradientn(name = expression(Delta*Connectivity), colors = c("#e66101", "#fdb863", "#f7f7f7", "#b2abd2", "#5e3c99"), limits = c(-25,25), guide = "colourbar") +
        scale_color_gradientn(colors = "#000000", guide = "none") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/connectivity-difference.jpeg",
       plot = connect.diff,
       height = 800,
       width = 600,
       dpi = 72,
       quality = 100,
       units = "px")

# 5.0 Visualizations of the individual stream scores ----
# Load a stream layer
hfi.year <- 2022
huc.scale <- 6
HUC <- "040102"

results.list <- list.files(paste0("2_pipeline/huc-", huc.scale, "/", 
                                  hfi.year, "/connectivity/"), full.names = TRUE)
load(results.list[grep(HUC, results.list)])

#  Load the stream layer
stream.layer <- read_sf(dsn =  paste0("2_pipeline/huc-", huc.scale, "/", 
                                      hfi.year, "/gis/", HUC, ".gdb/"),
                        layer = "StreamSeg")

culvert.layer <- read_sf(dsn =  paste0("2_pipeline/huc-", huc.scale, "/", 
                                       hfi.year, "/gis/", HUC, ".gdb/"),
                         layer = "Culverts")

boundary <- read_sf(dsn =  paste0("2_pipeline/huc-", huc.scale, "/", 
                                  hfi.year, "/gis/", HUC, ".gdb/"),
                    layer = "watershed_boundary")

if(is.null(watershed.network$ModelMean)){
        
        connectivity <- data.frame(StreamID = stream.layer$StreamID,
                                   StreamLength = stream.layer$StrmLength,
                                   StreamConnect = 1,
                                   Numerator = 1,
                                   Denominator = 1)
        
} else {
        
        connectivity <- watershed.network$ModelMean
        
}

# 5.1 Load the resistance information and subset to the main watershed network (remove stray features) ----
load(paste0("2_pipeline/resistance/", HUC, "-resistance-culverts.RData"))
results.store$CRP <- 100 - results.store$Status

# Identify base network structure
# Load the watershed of interest 
node.data.raw <- watershed.network$Node_Cleaned
edge.data.cur <- watershed.network$Edge_Predicted

base.network <- network_visualization(edge.network = edge.data.cur$Node, 
                                      conversion = TRUE) 

# Identify streams in the main network
membership.check <- components(base.network)$membership

if(nrow(node.data.raw) == length(membership.check) ) {
        
        node.data.raw["Membership"] <- membership.check
        
} else{
        
        stream.extra <- node.data.raw[-(1:length(membership.check)), ]
        node.data.raw <- node.data.raw[1:length(membership.check), ]
        node.data.raw["Membership"] <- membership.check
        
}
dominant.network <- names(sort(table(node.data.raw$Membership), decreasing = TRUE)[1])
valid.streams <- node.data.raw[node.data.raw$Membership == dominant.network, "Stream"]
results.store <- results.store[results.store$StrmID %in% valid.streams, ]
results.store <- results.store[!is.na(results.store$CRP), ]
results.store <- results.store[order(results.store$CRP), ]
results.store$Index <- 1:nrow(results.store)

# 5.2 Merge the connectivity information with the polygon ----
stream.layer <- merge(stream.layer, connectivity, by = "StreamID")
stream.layer <- stream.layer[, c("StreamID", "StreamLength", "StreamConnect", "Numerator", "Denominator")]
stream.layer$Connect <- stream.layer$StreamConnect * 100

connect.2022 <- ggplot() + 
        geom_sf(data = boundary, aes(color = "White", fill = "White"), show.legend = FALSE) +
        scale_color_manual(values = "#000000") +
        scale_fill_manual(values = "#FFFFFF") +
        new_scale_colour() +
        new_scale_fill() +
        geom_sf(data = stream.layer, aes(color = Connect, fill = Connect)) +
        scale_fill_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

connect.culvert.2022 <- ggplot() + 
        geom_sf(data = boundary, aes(color = "White", fill = "White"), show.legend = FALSE) +
        scale_color_manual(values = "#000000") +
        scale_fill_manual(values = "#FFFFFF") +
        new_scale_colour() +
        new_scale_fill() +
        geom_sf(data = stream.layer, aes(color = Connect, fill = Connect)) +
        scale_fill_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        geom_sf(data = culvert.layer, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/connectivity-stream.jpeg",
       plot = ggarrange(connect.2022, connect.culvert.2022, nrow = 2),
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

# Simple sorted values plot
threshold.value <- quantile(results.store$CRP, 0.9)

crp.potential <- ggplot() + 
        geom_point(data = results.store, aes(x = Index, y = CRP), show.legend = FALSE) +
        geom_hline(yintercept=threshold.value, linetype="dashed") +
        xlab("Culvert Index") +
        ylab("Culvert Resistance Potential (%)") +
        theme_light() +
        theme(axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

# Filter by the culverts that are identified
threshold.culverts <- results.store$StrmID[results.store$CRP >= threshold.value]
threshold.culverts <- culvert.layer[culvert.layer$StreamID %in% threshold.culverts |
                                            culvert.layer$StreamID_1 %in% threshold.culverts, ]
resist.culvert.2022 <- ggplot() + 
        geom_sf(data = boundary, aes(color = "White", fill = "White"), show.legend = FALSE) +
        scale_color_manual(values = "#000000") +
        scale_fill_manual(values = "#FFFFFF") +
        new_scale_colour() +
        new_scale_fill() +
        geom_sf(data = stream.layer, aes(color = Connect, fill = Connect)) +
        scale_fill_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        scale_color_gradientn(name = paste0("Stream\nConnectivity"), colors = (met.brewer(name = "VanGogh3", n = 100, type = "continuous")), limits = c(0,100), guide = "colourbar") +
        geom_sf(data = threshold.culverts, show.legend = FALSE) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=18),
              axis.text.y = element_text(size=18),
              title = element_text(size=18),
              legend.title = element_text(size=18),
              legend.text = element_text(size=16),
              panel.grid.major.y = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = "3_output/figures/resistance-culvert.jpeg",
       plot = crp.potential,
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

ggsave(filename = "3_output/figures/resistance-culvert-spatial.jpeg",
       plot = resist.culvert.2022,
       height = 600,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

rm(list=ls())
gc()
