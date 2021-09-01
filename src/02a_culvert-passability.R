#
# Title: Modeling Culvert Passability
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Using the available culvert data, construct a hanging passability model
# Keywords: Notes, Extraction from matching, Culvert Predictions
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# 1) All paths defined in this script are local
#
############################
# Extraction from matching # Extracts need to occur for each version of the HFI analyzed
############################ 

# Clear memory
rm(list=ls())
gc()

# Load library, lookup, and sampled culverts
library(foreign)
library(nngeo)
library(rgdal)
library(rgeos)
library(sf)
library(sp)

sampled.culverts <- st_read("data/processed/culverts/gis/sampled-culverts_EPSG-3400_2021-05-05.shp")
sampled.culverts$Node <- 1:nrow(sampled.culverts)
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

# Filter out the sampled culverts to include only a relevant subset for modeling
sampled.culverts <- sampled.culverts[!is.na(sampled.culverts$StreamType), ] # Remove NA stream typles
#sampled.culverts <- sampled.culverts[sampled.culverts$StreamType != "Ephemeral", ] # Remove ephemeral streams
sampled.culverts <- sampled.culverts[sampled.culverts$CulvertTyp == "Culvert - Single", ] # Single culverts
sampled.culverts <- sampled.culverts[sampled.culverts$Passabilit != "NA", ] # Remove NA passability rank

# If there are locations with multiple samples (duplicated lat/long combinations) select the most recent year
duplicate.list <- sampled.culverts[duplicated(sampled.culverts[, c("Lat", "Long")]), ]
duplicate.list <- duplicate.list[!duplicated(duplicate.list[, c("Lat", "Long")]), ]

for (location.id in 1:nrow(duplicate.list)) {
        
        # Pull out all values with matching lat/long
        duplicate.point <- sampled.culverts[sampled.culverts$Lat == duplicate.list$Lat[location.id] & sampled.culverts$Long == duplicate.list$Long[location.id], ]
        
        # Identify which to remove (oldest records)
        duplicate.point <- duplicate.point[duplicate.point$Inspection != max(duplicate.point$Inspection),]
        
        # Remove duplicates from base geometry
        sampled.culverts <- sampled.culverts[!(sampled.culverts$CulvertID %in% duplicate.point$CulvertID), ]
        
}

rm(location.id, duplicate.list, duplicate.point)

# Create the four HFI year subsets
sampled.culverts$Inspection <- as.Date(sampled.culverts$Inspection, format = "%m/%d/%Y")
sampled.culverts$Inspection > as.Date("01/01/2018", format = "%m/%d/%Y")
sampled.culverts$YearGroup <- ifelse(sampled.culverts$Inspection > as.Date("01/01/2018", format = "%m/%d/%Y"), 2018,
                                     ifelse(sampled.culverts$Inspection > as.Date("01/01/2015", format = "%m/%d/%Y") & sampled.culverts$Inspection < as.Date("01/01/2018", format = "%m/%d/%Y"), 2016,
                                            ifelse(sampled.culverts$Inspection > as.Date("01/01/2013", format = "%m/%d/%Y") & sampled.culverts$Inspection < as.Date("01/01/2015", format = "%m/%d/%Y"), 2014, 2010)))

# For each watershed, Identify which culverts have samples
joined.results <- NULL

for (hfi.year in unique(sampled.culverts$YearGroup)) {

        culvert.subset <- sampled.culverts[sampled.culverts$YearGroup == hfi.year, ]
        
        for (watershed in watershed.ids) {
                
                # Load the watershed of interest
                temp.culverts <- try(st_read(paste0("data/processed/huc-6/", hfi.year, "/gis/", watershed, "/Culverts.shp")))
                if(class(temp.culverts)[1] == "try-error") {
                        
                        next
                        
                } 
                
                if(nrow(temp.culverts) == 0) {
                        
                        next
                        
                } else {
                        
                        # Join points within 50m buffer
                        joined.temp <- st_join(x = culvert.subset, y = temp.culverts, join = nngeo::st_nn, maxdist = 50, k = 1)
                        
                        # Remove values without a join
                        joined.temp <- joined.temp[!is.na(joined.temp$InterID), ]
                        
                        # Identify instances where a single node in the network is assigned two sampling events
                        if(length(joined.temp$InterID) != length(unique(joined.temp$InterID))) {
                                
                                # Identify duplicates
                                duplicate.joins <- joined.temp$InterID[duplicated(joined.temp$InterID)]
                                
                                for(duplicate.id in duplicate.joins) {
                                        
                                        duplicate.temp <- joined.temp[joined.temp$InterID == duplicate.id, ]
                                        culvert.distance <- st_distance(x = duplicate.temp, y = temp.culverts[temp.culverts$InterID == duplicate.id, ])
                                        
                                        if(table(culvert.distance[,1] != min(culvert.distance[,1 ]))["FALSE"] == nrow(culvert.distance)) {
                                                
                                                # Take the first value if all points are equal distance
                                                culvert.distance <- duplicate.temp$CulvertID[1]
                                                
                                        } else {
                                                
                                                culvert.distance <- duplicate.temp$CulvertID[culvert.distance[,1] != min(culvert.distance[,1 ])]
                                                
                                        }
                                        
                                        # Remove from main join
                                        joined.temp <- joined.temp[joined.temp$CulvertID != culvert.distance, ]
                                        
                                }
                                
                        }
                        
                        # Remove geometry and filter columns
                        st_geometry(joined.temp) <- NULL
                        joined.temp <- joined.temp[, c(51, 1:7, 52, 8, 9)]
                        joined.temp["HUC6"] <- rep(watershed, nrow(joined.temp))
                        
                        # Store the results
                        joined.results <- rbind(joined.results, joined.temp)
                        
                        rm(joined.temp)
                        
                }
                
        }
        
}

joined.results$YearGroup <- ifelse(joined.results$Inspection > as.Date("01/01/2018", format = "%m/%d/%Y"), 2018,
                                     ifelse(joined.results$Inspection > as.Date("01/01/2015", format = "%m/%d/%Y") & joined.results$Inspection < as.Date("01/01/2018", format = "%m/%d/%Y"), 2016,
                                            ifelse(joined.results$Inspection > as.Date("01/01/2013", format = "%m/%d/%Y") & joined.results$Inspection < as.Date("01/01/2015", format = "%m/%d/%Y"), 2014, 2010)))

joined.original <- joined.results

# Using this list, pull the relevant information from edge data sets
culvert.summaries <- NULL

for (hfi.year in unique(joined.original$YearGroup)) {
        
        joined.results <- joined.original[joined.original$YearGroup == hfi.year, ] 
        
        for (watershed in unique(joined.results$HUC6)) {
                
                temp.culverts <- read.csv(paste0("data/processed/huc-6/", hfi.year, "/predicted/stream/", watershed, "-edge-connectivity-cleaned.csv"))
                temp.culverts <- temp.culverts[match(joined.results[joined.results$HUC6 == watershed, "InterID"], temp.culverts$TARGET_FID, nomatch = 0),  ]
                temp.culverts["HUC6"] <- watershed
                
                temp.results <- joined.results[joined.results$HUC6 == watershed, ] # Subset to watershed of interest as some InterID are duplicated across watersheds
                
                colnames(temp.results)[1] <- "TARGET_FID"
                temp.culverts <- merge.data.frame(temp.culverts, temp.results[, c("TARGET_FID", "Passabilit", "Passabil_1", "Inspection")], by = "TARGET_FID")
                colnames(temp.culverts)[39:41] <- c("Passability", "Pass_Reason", "Inspection")
                culvert.summaries <- rbind(culvert.summaries, temp.culverts)
                
                rm(temp.culverts, temp.results)
                
        }
        
}

write.csv(culvert.summaries, file = paste0("data/processed/culverts/sampled/culverts-gis-summaries-aggregate_", Sys.Date(), ".csv"), row.names = FALSE)

rm(list=ls())
gc()

#################
# Culvert Model # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries and data
library(dismo)
library(gbm)
library(PresenceAbsence)
library(pROC)

culvert.data <- read.csv("data/processed/culverts/sampled/culverts-gis-summaries-aggregate_2021-05-26.csv")

# Filter the data set to remove bridges and only include hanging culverts as the blockage type
culvert.data <- culvert.data[culvert.data$Class == "Culvert", ] # Only use culverts

# Format hanging culverts
hanging.culvert <- culvert.data[culvert.data$Pass_Reason == "Hanging Culvert" & !is.na(culvert.data$Pass_Reason), ]
hanging.culvert$Passability <- 0 # If identified as a hanging culvert, passability is assumed zero 

# Format passable culverts
pass.culverts <- culvert.data[culvert.data$Passability %in% c("No Concerns", "Remediated") & is.na(culvert.data$Pass_Reason), ]
pass.culverts$Passability <- ifelse(pass.culverts$Passability == "Serious Concerns", 0,
                                    ifelse(pass.culverts$Passability == "Some Concerns", 0.0, 1))

# Some culverts that were identified as passable for fish have other barriers in place such as beaver dams, gates, or culvert damage.
# Keep only passable culverts that are either Reclaimed or have no passability reason

pass.culverts <- pass.culverts[is.na(pass.culverts$Pass_Reason) | pass.culverts$Pass_Reason == "Reclaimed", ]

# Merge the datasets
culvert.data <- rbind(hanging.culvert, pass.culverts)
rm(hanging.culvert, pass.culverts)

# Create the new Climate Moisture Index (MAP - Eref)
culvert.data$CMI <- culvert.data$MAP_Point - culvert.data$Eref_Point

# Convert distance from m to km
culvert.data$Distance <- culvert.data$Distance / 1000

# Make strahler a factor
culvert.data$Strahler <- factor(culvert.data$Strahler)

# Truncate distance

# Assess correlations between variable sets
library(ggplot2)
library(reshape2)

cor.data <- melt(cor(culvert.data[, c(14:28, 30:37, 41)]))
ggplot(data = cor.data, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1))

# Perform the following steps for assigning culvert assessment:
# 1) Create boosted regression tree using the complete data set
# 2) Define list of node data sets that need predictions.
# 3) For each node data set: Load data, make prediction, binarize prediction, mask known passability scores
# Tree complexity should be defined based on how many interactions we want the model to have (1 == additive, 2 = 2 way interactions, etc.)

# Create boosted regression tree using the complete data set
brt.model <- gbm.step(data=culvert.data, gbm.x = c(15, 28, 33, 34, 35, 36, 37, 41), gbm.y = 39,
                      family = "bernoulli", tree.complexity = 5,
                      learning.rate = 0.005, bag.fraction = 0.5, n.folds = 10)

# Visualize the responses to see if they are reasonable.
summary(brt.model)
gbm.plot(brt.model)

# Assess if the model should be simplified (i.e., removes variables don't impact the predicted deviance)
brt.simp <- gbm.simplify(brt.model)

# Update the boosted regression tree based on the simplified list.
# Final variable set includes CMI, Confluence, Drainage Density, Upstream Distance, Slope, and Basin Relief
brt.model <- gbm.step(data=culvert.data, gbm.x = brt.simp$pred.list[[2]], gbm.y = 39,
                      family = "bernoulli", tree.complexity = 6,
                      learning.rate = 0.001, bag.fraction = 0.5, n.folds = 10)

gbm.plot(brt.model)
gbm.plot.fits(brt.model, mask.presence = TRUE)

# Interactions
model.interactions <- gbm.interactions(brt.model)
model.interactions$rank.list
gbm.perspec(brt.model, 6, 3)
gbm.perspec(brt.model, 6, 2)

# Load model (Original run June 1st, 2021)
load("results/hanging-culvert-model/boosted-regression-tree_2021-05-26.Rdata")

# Make prediction
culvert.data$Prediction <- predict.gbm(brt.model, culvert.data,
                            n.trees = brt.model$gbm.call$best.trees, type="response")

pass.threshold <- optimal.thresholds(culvert.data[, c(1, 39, 42)],
                                     threshold = 101,
                                     opt.methods = "MaxSens+Spec")$Prediction

####################
# Model Assessment #
####################

# Define analysis year
analysis.year <- 2018

# Define list of node data sets that need predictions.
node.list <- list.files(paste0("data/processed/huc-6/", analysis.year, "/predicted/stream/"), full.names = TRUE)
node.list <- node.list[grep("edge", node.list)]

watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
watershed.average <- data.frame(Watershed = watershed.ids,
                                NCulverts = NA,
                                MeanPass = NA,
                                BinaryPass = NA)

# Loop through each watershed to assess the distribution of hanging culverts throughout the province
for(watershed in 1:length(watershed.ids)) {
        
        # Load node.list
        node.name <- node.list[grep(watershed.ids[watershed], node.list)]
        
        if(length(node.name) == 0) {
                
                next
                
        }
        temp.node <- read.csv(node.name)
        
        # Add CMI
        temp.node$CMI <- temp.node$MAP_Point - temp.node$Eref_Point
        
        # Convert distance from m to km
        temp.node$Distance <- temp.node$Distance / 1000
        
        # Make prediction
        temp.node$Up <- predict.gbm(brt.model, temp.node,
                                    n.trees = brt.model$gbm.call$best.trees, type="response")
        
        # Subset anything the model would have predicted 
        temp.node <- temp.node[temp.node$Class != "Split", ]
        
        watershed.average[watershed.average$Watershed == watershed.ids[watershed], c("NCulverts", "MeanPass", "BinaryPass")] <- c(nrow(temp.node), 
                                                                                                                                  mean(temp.node$Up), 
                                                                                                                                  mean(ifelse(temp.node$Up >= pass.threshold, 1, 0)))
        
        rm(temp.node)
        
        print(watershed)
        
}

# Save the predictions

watershed.average$NCulverts[is.na(watershed.average$NCulverts)] <- 0
watershed.average$MeanPass[is.na(watershed.average$MeanPass)] <- 1
watershed.average$BinaryPass[is.na(watershed.average$BinaryPass)] <- 1
colnames(watershed.average)[1] <- "HUC_6"

culvert.gis <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
culvert.gis <- merge.data.frame(culvert.gis, watershed.average, by = "HUC_6")
write_sf(culvert.gis, dsn = "results/hanging-culvert-model/provincial-culvert-model_2021-06-04.shp")

#save(brt.model, file = "results/hanging-culvert-model/boosted-regression-tree_2021-05-26.Rdata")

#######################
# Culvert Predictions # 
#######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries and data
library(dismo)
library(foreign)
library(gbm)
library(PresenceAbsence)
library(pROC)

culvert.org <- read.csv("data/processed/culverts/sampled/culverts-gis-summaries-aggregate_2021-06-21.csv")
load("results/hanging-culvert-model/boosted-regression-tree_2021-05-26.Rdata")
pass.threshold <- 0.56

# Filter the data set to remove bridges and only include hanging culverts as the blockage type
culvert.org <- culvert.org[culvert.org$Class == "Culvert", ] # Only use culverts

# Format culverts, if no passability reason, assume passable
culvert.org$Passability <- ifelse(is.na(culvert.org$Pass_Reason), 1, 0)

hfi.year <- c(2010, 2014, 2016, 2018)

for (analysis.year in hfi.year) {

        # Define list of node data sets that need predictions.
        node.list <- list.files(paste0("data/processed/huc-6/", analysis.year, "/predicted/stream/"), full.names = TRUE)
        node.list <- node.list[grep("edge", node.list)]
        
        watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
        watershed.ids <- unique(as.character(watershed.ids$HUC_6))
        watershed.average <- data.frame(Watershed = watershed.ids,
                                        NCulverts = NA,
                                        MeanPass = NA,
                                        BinaryPass = NA)
        
        # Subset the culvert data by survey year
        culvert.data <- culvert.org[culvert.org$Inspection < as.Date(paste0("01/01/", analysis.year), format = "%m/%d/%Y"), ]
        
        for(watershed in 1:length(watershed.ids)) {
                
                # Load node.list
                node.name <- node.list[grep(watershed.ids[watershed], node.list)]
                
                if(length(node.name) == 0) {
                        
                        next
                        
                }
                
                temp.node <- read.csv(node.name)
                
                # Add CMI
                temp.node$CMI <- temp.node$MAP_Point - temp.node$Eref_Point
                
                # Convert distance from m to km
                temp.node$Distance <- temp.node$Distance / 1000
                
                # Make prediction
                temp.node$Up <- predict.gbm(brt.model, temp.node,
                                            n.trees = brt.model$gbm.call$best.trees, type="response")
                
                # Binarize predictions ( No longer used )
                # temp.node$Up <- ifelse(temp.node$Up >= pass.threshold, 1, 0)
                
                # If culvert is a split, fix to 1
                temp.node$Up[temp.node$Class == "Split"] <- 1
                
                # If a bridge and date is less than the construction date, fix to 1
                temp.node$Up[temp.node$Class == "Bridge" & temp.node$BridgeDate <= as.numeric(analysis.year)] <- 1
                
                # If a dam is present, define as 0
                temp.node$Up[!is.na(temp.node$Dam)] <- 0
                temp.node$Class[!is.na(temp.node$Dam)] <- "Dam"
                
                # If culvert has been sampled, fix to the known value
                if(watershed.ids[watershed] %in% culvert.data$HUC6) {
                        
                        temp.sampled <- culvert.data[culvert.data$HUC6 == watershed.ids[watershed], ]
                        match.id <- match(temp.sampled$TARGET_FID, temp.node$TARGET_FID, nomatch = 0)
                        temp.node[match.id, "Up"] <- temp.sampled[which(temp.sampled$TARGET_FID %in% temp.node[match.id, "TARGET_FID"]), "Passability"]
                        rm(temp.sampled, match.id)
                        
                }
                
                # If crossing or stream intersection occurs in the minable region, fix to 0
                temp.node$Up[temp.node$Mineable == "Inside"] <- 0
                
                write.csv(temp.node,
                          file = paste0("data/processed/huc-6/", analysis.year, "/predicted/passability/", watershed.ids[watershed],
                                        "-predicted-culverts_", Sys.Date(), ".csv"),
                          row.names = FALSE)
                
                # Subset all barriers
                temp.node <- temp.node[temp.node$Class != "Split", ]
                
                # Save mean passability
                watershed.average[watershed.average$Watershed == watershed.ids[watershed], c("NCulverts", "MeanPass", "BinaryPass")] <- c(nrow(temp.node), 
                                                                                                                                          mean(temp.node$Up), 
                                                                                                                                          mean(ifelse(temp.node$Up >= pass.threshold, 1, 0)))
                
                rm(temp.node)
                
                print(watershed)
                
        }
        
        if (analysis.year == 2018) {
                
                watershed.average$NCulverts[is.na(watershed.average$NCulverts)] <- 0
                watershed.average$MeanPass[is.na(watershed.average$MeanPass)] <- 1
                watershed.average$BinaryPass[is.na(watershed.average$BinaryPass)] <- 1
                colnames(watershed.average)[1] <- "HUC_6"
                
                culvert.gis <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
                culvert.gis <- merge.data.frame(culvert.gis, watershed.average, by = "HUC_6")
                write_sf(culvert.gis, dsn = paste0("results/hanging-culvert-model/provincial-culvert-model-masking-", analysis.year, "_2021-06-04.shp"))
                
                
        }

        print(analysis.year)
}

rm(list=ls())
gc()
