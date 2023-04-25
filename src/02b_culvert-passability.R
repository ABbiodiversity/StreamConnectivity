#
# Title: Modeling Culvert Passability
# Created: September 1st, 2021
# Last Updated: April 23, 2023
# Author: Brandon Allen
# Objectives: Using the available culvert data, construct a hanging passability model
# Keywords: Notes, Culvert Model, Culvert Predictions
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# 1) All paths defined in this script are local
#
#################
# Culvert Model # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries and data
library(dismo)
library(gbm)
library(ggplot2)
library(pROC)
library(reshape2)

load("data/processed/culverts/culvert-model-attributes.Rdata")

# Filter the data set to remove bridges and only include hanging culverts as the blockage type
culvert.attributes <- culvert.attributes[culvert.attributes$Class == "Culvert", ] # Only use culverts

# Format hanging culverts
hanging.culvert <- culvert.attributes[!is.na(culvert.attributes$PassabilityConcern.x), ]
hanging.culvert <- hanging.culvert[hanging.culvert$PassabilityConcern.x == "Hanging Culvert", ]
hanging.culvert$Passability <- 0 # If identified as a hanging culvert, passability is assumed zero 

# Format passable culverts
pass.culverts <- culvert.attributes[!is.na(culvert.attributes$Passability.x), ]
pass.culverts <- pass.culverts[pass.culverts$Passability.x == "No Concerns", ]
pass.culverts$Passability <- 1

# Merge the datasets
model.data <- rbind(hanging.culvert, pass.culverts)
rm(hanging.culvert, pass.culverts)

# Create the new Climate Moisture Index (MAP - Eref)
model.data$CMI <- model.data$MAP - model.data$Eref

# Convert distance from m to km
model.data$Distance <- model.data$Distance / 1000

# Make Strahler a factor
model.data$Strahler <- factor(model.data$Strahler)

# Assess correlations between variable sets
cor.data <- melt(cor(model.data[, c(16:24, 26:33, 39)]))
ggplot(data = cor.data, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1))

# Perform the following steps for assigning culvert assessment:
# 1) Create boosted regression tree using the complete data set
# 2) Define list of node data sets that need predictions.
# 3) For each node data set: Load data, make prediction, mask known passability scores
# Tree complexity should be defined based on how many interactions we want the model to have (1 == additive, 2 = 2 way interactions, etc.)

# Create boosted regression tree using the complete data set
brt.model <- gbm.step(data = model.data, gbm.x = c(16, 24, 29, 31, 32, 39), 
                      gbm.y = 38, family = "bernoulli", tree.complexity = 5,
                      learning.rate = 0.01, bag.fraction = 0.5, n.folds = 10, 
                      n.trees = 20000)

# Visualize the responses to see if they are reasonable.
summary(brt.model)
gbm.plot(brt.model)

# Save the model output with the commented fit statistics
comment(brt.model) <- "Training data AUC score = 0.931; Cross-validation AUC score = 0.898; Created April 24, 2023"
save(brt.model, file = "results/hanging-culvert-model/hanging-culvert-model.Rdata")

####################
# Model Assessment # 
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(gbm)

# Load hanging culvert model 
load("results/hanging-culvert-model/hanging-culvert-model.Rdata")

# Define analysis year
huc.scale <- 6
hfi.year <- 2018

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
watershed.average <- data.frame(Watershed = watershed.ids,
                                NCulverts = NA,
                                MeanPass = NA)

# Loop through each watershed to assess the distribution of hanging culverts throughout the province
for(HUC in watershed.ids) {
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                    hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        # If there are culverts in the watershed, proceed with the match
        if(is.null(watershed.network[["Edge_Cleaned"]])) {
                
                next()
                
        }
        
        # Pull out culvert data
        temp.node <- watershed.network[["Edge_Cleaned"]]
        
        # Add CMI
        temp.node$CMI <- temp.node$MAP - temp.node$Eref
        
        # Convert distance from m to km
        temp.node$Distance <- temp.node$Distance / 1000
        
        # Make prediction
        temp.node$Up <- predict.gbm(brt.model, temp.node,
                                    n.trees = brt.model$gbm.call$best.trees, type="response")
        
        # Subset anything the model would have predicted 
        temp.node <- temp.node[temp.node$Class != "Split", ]
        
        # Store the results
        watershed.average[watershed.average$Watershed == HUC, c("NCulverts", "MeanPass")] <- c(nrow(temp.node), mean(temp.node$Up))
        rm(temp.node)
        
        print(watershed)
        
}

# Standardize and save the predictions
watershed.average$NCulverts[is.na(watershed.average$NCulverts)] <- 0
watershed.average$MeanPass[is.na(watershed.average$MeanPass)] <- 1
colnames(watershed.average)[1] <- "HUC_6"

culvert.gis <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
culvert.gis <- merge.data.frame(culvert.gis, watershed.average, by = "HUC_6")
write_sf(culvert.gis, dsn = "results/hanging-culvert-model/provincial-hanging-culvert-model.shp")

#######################
# Culvert Predictions # 
#######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(gbm)

# Load hanging culvert model 
load("results/hanging-culvert-model/hanging-culvert-model.Rdata")

# Define analysis year
huc.scale <- 6
hfi.year <- c(2010, 2018)

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

# Work on this once it the final model is created
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
