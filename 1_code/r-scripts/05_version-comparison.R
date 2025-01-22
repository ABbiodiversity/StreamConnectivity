# ---
# title: "Version comparison"
# author: "Brandon Allen"
# created: "2025-01-21"
# inputs: ["3_output/summaries/shapefiles/connectivity_hfi_2018.shp";
#          "3_output/shapefiles/previous-version/provincial-watershed-connectivity_2021-07-14.shp"]
# outputs: ["3_output/figures/spatial-version-comparison-2018HFI.jpeg";
#           "3_output/figures/version-comparison-2018HFI.jpeg";
#           "3_output/figures/version-temporal-change-2018HFI.jpeg";
#           "3_output/figures/survey-effort-comparison.jpeg"]
# notes: 
#   "This script performs the comparison between the current version and any previous version of the indicator.
#    We use the 2018 HFI as our "baseline" year as it is available for all versions and has some culvert data available for it".
# ---

# 1.0 Initializing environment ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries ----
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(sf)

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
load("0_data/processed/culverts/culvert-model-attributes.Rdata") # New effort
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

ggsave(filename = "3_output/figures/version-comparison-2018HFI.jpeg",
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

ggsave(filename = "3_output/figures/version-temporal-change-2018HFI.jpeg",
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
        scale_fill_gradientn(name = paste0("Delta Connectivity"), colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(-35,35), guide = "colourbar") +
        scale_color_gradientn(colors = (met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(-35,35), guide = "none") +
        ggtitle("Difference") +
        theme_light() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())

ggsave(filename = "3_output/figures/spatial-difference-2018HFI.jpeg",
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

ggsave(filename = "3_output/figures/survey-effort-comparison.jpeg",
       plot = survey.plot,
       height = 800,
       width = 800,
       dpi = 72,
       quality = 100,
       units = "px")

rm(list=ls())
gc()
