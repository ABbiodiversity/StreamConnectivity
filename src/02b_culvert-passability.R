#
# Title: Modeling Culvert Passability
# Created: September 1st, 2021
# Last Updated: July 15th, 2024
# Author: Brandon Allen
# Objectives: Using the available culvert data, construct a hanging passability model
# Keywords: Notes, Standardization, Bootstrap, Model Assessment, Culvert Predictions
#

#########
# Notes # 
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
###################
# Standardization # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreach)
library(foreign)
library(parallel)

# Source functions
source("src/culvert-passability_functions.R")

# Load the culvert data
load("data/processed/culverts/culvert-model-attributes.Rdata")

# Filter the data set to remove bridges and only include hanging culverts as the blockage type
culvert.attributes <- culvert.attributes[culvert.attributes$Class == "Culvert", ] # Only use culverts
culvert.attributes <- culvert.attributes[!(culvert.attributes$StreamClass %in% c("Cross drain", "Cross Drain")), ] # Remove Cross Drains

# Format hanging culverts
hanging.culvert <- culvert.attributes[!is.na(culvert.attributes$PassabilityConcern), ]
hanging.culvert <- hanging.culvert[hanging.culvert$PassabilityConcern == "Hanging Culvert", ]
hanging.culvert$Passability <- 0 # If identified as a hanging culvert, passability is assumed zero 

# Format passable culverts
pass.culverts <- culvert.attributes[!is.na(culvert.attributes$Passability), ]
pass.culverts <- pass.culverts[pass.culverts$Passability == "No Concerns", ]
pass.culverts$Passability <- 1

# Merge the datasets
model.data <- rbind(hanging.culvert, pass.culverts)
rm(hanging.culvert, pass.culverts)

# Create the new Climate Moisture Index (MAP - Eref)
model.data$CMI <- model.data$MAP - model.data$Eref

# Convert distance from m to km
model.data$Distance <- model.data$Distance / 1000

# Reclassify the road data (Gravel, unimproved, paved?)

model.data$RoadClass <- "Paved"
model.data$RoadClass[model.data$FeatureType %in% c("ROAD-GRAVEL-1L", "ROAD-GRAVEL-2L",
                                                   "ROAD-UNPAVED-2L")] <- "Gravel"
model.data$RoadClass[model.data$FeatureType %in% c("FORD-WINTER-XING", "ROAD-UNCLASSIFIED",
                                                   "ROAD-UNIMPROVED", "ROAD-WINTER-ACCESS",
                                                   "ROAD-WINTER-ROAD", "TRAIL-ATV",
                                                   "TRUCK-TRAIL")] <- "Unimproved"
model.data$RoadClass <- factor(model.data$RoadClass)

# Filter the model data to include only surveys post 2018. This is done because the landscape we are using is 
# based on the 2018 road network. Culverts older than that may have been fixed, while newer surveys may still 
# represent that status of the watershed a few years ago seeing as we don't have many reclaimed culverts in the data.
model.data <- model.data[model.data$SurveyDate > "2018-01-01", ]

# Test the model using the full variable list, then use gbm.simplify to simplify the object. 
# After testing is completed, use the single best model for the bootstrap iterations

#############
# Bootstrap # 
#############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define the cores and objects required for for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("model.data", "brt_function"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(dismo)
        library(gbm)

        
})

# Loop through each available HFI inventory
brt.models <- parLapply(core.input, 
                        as.list(1:100),
                        fun = function(boot) tryCatch(brt_function(data = model.data,
                                                                   boot = boot), 
                                                      error = function(e) e))
        


stopCluster(core.input)

# Create the confusion matrix for the first model
brt.model <- brt.models[[1]]
model.data$Prediction <- predict.gbm(brt.model, model.data,
                                     n.trees = brt.model$gbm.call$best.trees, type="response")

# We use the MaxSens+Spec threshold approach as it is a more conservative threshold compared to Kappa.
# It incorrectly predicts more hanging culverts when not present, but has high accuracy is predicting known hanging culverts.
pass.threshold <- optimal.thresholds(model.data[, c("Node", "Passability", "Prediction")],
                                     threshold = 101,
                                     opt.methods = "MaxSens+Spec")$Prediction

# Create the confusion matrix for reporting purposes
confusion.matrix <- confusionMatrix(data = factor(ifelse(model.data$Prediction >= pass.threshold, 1, 0)), 
                                    reference = factor(model.data$Passability))
pROC::auc(model.data$Passability, model.data$Prediction)

# Save the model output with the commented fit statistics
comment(brt.models) <- paste0("100 Bootstrap iterations. First bootstrap uses full dataset; ",
                              "Accuracy = ", round(confusion.matrix$overall["Accuracy"], 3), "; ",
                              "Kappa = ", round(confusion.matrix$overall["Kappa"], 3), 
                              "; Created July 15, 2024")
save(brt.models, pass.threshold, pass.prevalence, confusion.matrix, file = "results/hanging-culvert-model/version-4/hanging-culvert-model.Rdata")

####################
# Model Assessment # 
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(gbm)
library(ggplot2)
library(sf)

# Load hanging culvert model 
load("results/hanging-culvert-model/version-4/hanging-culvert-model.Rdata")

# Define analysis year
huc.scale <- 6
hfi.year <- 2018

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
watershed.average <- data.frame(Watershed = watershed.ids,
                                NCulverts = NA,
                                MeanPass = NA,
                                Favorability = NA,
                                BinaryPass = NA)

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
        
        # Reclassify the road data (Gravel, unimproved, paved?)
        temp.node$RoadClass <- "Paved"
        temp.node$RoadClass[temp.node$FeatureType %in% c("ROAD-GRAVEL-1L", "ROAD-GRAVEL-2L",
                                                           "ROAD-UNPAVED-2L")] <- "Gravel"
        temp.node$RoadClass[temp.node$FeatureType %in% c("FORD-WINTER-XING", "ROAD-UNCLASSIFIED",
                                                           "ROAD-UNIMPROVED", "ROAD-WINTER-ACCESS",
                                                           "ROAD-WINTER-ROAD", "TRAIL-ATV",
                                                           "TRUCK-TRAIL")] <- "Unimproved"
        temp.node$RoadClass <- factor(temp.node$RoadClass)
        
        # Make prediction
        temp.node$Up <- predict.gbm(brt.model, temp.node,
                                    n.trees = brt.model$gbm.call$best.trees, type="response")
        
        # Convert to favorability
        temp.node$Favorability <- plogis(qlogis(temp.node$Up) - qlogis(pass.prevalence))
        
        # Subset the network to include only Culverts
        temp.node <- temp.node[!(temp.node$Class %in% c("Split", "Bridge")), ]
        
        # Store the results
        watershed.average[watershed.average$Watershed == HUC, c("NCulverts", "MeanPass", 
                                                                "Favorability", "BinaryPass")] <- c(nrow(temp.node), 
                                                                                                    mean(temp.node$Up),
                                                                                                    mean(temp.node$Favorability),
                                                                                                    mean(ifelse(temp.node$Up > 0.5, 1, 0)))
        rm(temp.node)
        
        print(HUC)
        
}

# Standardize the watershed results for situations where no culverts are present
watershed.average$NCulverts[is.na(watershed.average$NCulverts)] <- 0
watershed.average$MeanPass[is.na(watershed.average$MeanPass)] <- 1
watershed.average$BinaryPass[is.na(watershed.average$BinaryPass)] <- 1
colnames(watershed.average)[1] <- "HUC_6"

# Save the predictions
culvert.gis <- read_sf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.shp")
culvert.gis <- merge.data.frame(culvert.gis, watershed.average, by = "HUC_6")
write_sf(culvert.gis, dsn = "results/hanging-culvert-model/version-4/provincial-hanging-culvert-model.shp")

# Load the previous hanging culvert model and assess fit between the two versions
old.model <- read_sf("results/hanging-culvert-model/version-1/provincial-culvert-model_2021-06-04.shp")

# Align the two sets of data
old.model <- as.data.frame(st_drop_geometry(old.model[, c("HUC_6", "NCulverts", "MeanPass", "BinaryPass")]))
old.model <- old.model[!duplicated(old.model), ]
new.model <- culvert.gis[, c("HUC_6", "NCulverts", "MeanPass", "BinaryPass")]
new.model <- new.model[!duplicated(new.model), ]
colnames(new.model) <- c("HUC_6", "NCulvertsV3", "MeanPassV3", "BinaryPassV3")
culvert.predictions <- merge.data.frame(old.model, new.model, by = "HUC_6")


# Compare the two types of predictions (mean probability versus threshold probabilities)
# Moderate correlations between the two models (0.817, 0.738).
# New model suggests things aren't as bad as the previous version. With more surveys, we are finding fewer hanging culverts in proportion.
ggplot(data= culvert.predictions, aes(y = MeanPassV3, x = MeanPass)) +
        geom_point() +
        xlim(c(0.5,1)) +
        ylim(c(0.5,1)) +
        ggtitle(paste("Correlation =", round(cor(culvert.predictions$MeanPassV3, culvert.predictions$MeanPass), 3)))

ggplot(data= culvert.predictions, aes(y = BinaryPassV3, x = BinaryPass)) +
        geom_point()  +
        xlim(c(0,1)) +
        ylim(c(0,1)) +
        ggtitle(paste("Correlation =", round(cor(culvert.predictions$BinaryPassV3, culvert.predictions$BinaryPass), 3)))


#######################
# Culvert Predictions # 
#######################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(foreign)
library(gbm)

# Load hanging culvert model and matched data
load("results/hanging-culvert-model/version-2/hanging-culvert-model.Rdata")
load("data/processed/culverts/culvert-model-attributes.Rdata")

# Define analysis year
huc.scale <- 6
hfi.year <- c(2010, 2018)

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

for (analysis.year in hfi.year) {
        
        for(HUC in watershed.ids) {
                
                # Load the appropriate Rdata with the culvert information
                load(paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                            analysis.year, "/connectivity/network_", HUC, ".Rdata"))
                
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
                
                # Convert to favorability
                # temp.node$Up <- plogis(qlogis(temp.node$Up) - qlogis(pass.prevalence))
                
                # If it is a split or bridge, fix to 1
                temp.node$Up[(temp.node$Class %in% c("Split", "Bridge"))] <- 1
                
                # Threshold the remaining predictions (No need to threshold now)
                # temp.node$Up <- ifelse(temp.node$Up >= pass.threshold, 1, 0)
                
                # If there are surveys completed in the region within a match them (2 year window)
                min.logical <- temp.node$SurveyDate > as.Date(paste0(analysis.year - 2, "-01-01"))
                min.logical[is.na(min.logical)] <- FALSE
                max.logical <- temp.node$SurveyDate < as.Date(paste0(analysis.year + 2, "-01-01"))
                max.logical[is.na(max.logical)] <- FALSE
                concerns.logical <- temp.node$Passability %in% c("Serious Concerns", "Concerns", "Some Concerns")
                no.concerns.logical <- temp.node$Passability %in% c("No Concerns")
                
                # Change passability value for blocked culverts
                temp.node$Up[min.logical & max.logical & concerns.logical] <- 0
                
                # Change passability value for functional culverts
                temp.node$Up[min.logical & max.logical & no.concerns.logical] <- 1
                
                # If a dam is present, define as 0
                temp.node$Up[!is.na(temp.node$Dam)] <- 0
                temp.node$Class[!is.na(temp.node$Dam)] <- "Dam"
                
                # If crossing or stream intersection occurs in the minable region, fix to 0
                temp.node$Up[temp.node$Mineable == "Inside"] <- 0
                
                # Replace the with file with the predicted version then save
                watershed.network[["Edge_Cleaned"]] <- temp.node
                save(watershed.network, file = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                               analysis.year, "/connectivity/network_", HUC, ".Rdata"))

                print(HUC)
                
        }
        
        print(analysis.year)
        
}

# Clear memory
rm(list=ls())
gc()
