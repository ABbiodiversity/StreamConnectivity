#
# Title: Modeling Culvert Passability
# Created: September 1st, 2021
# Last Updated: July 16th, 2024
# Author: Brandon Allen
# Objectives: Using the available culvert data, construct a hanging passability model
# Keywords: Notes, Standardization, Bootstrap, Model Assessment, Culvert Surveys
#

#########
# Notes # CLEAN UP FAVORABILITY CALCULATION
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
library(caret)
library(dismo)
library(foreach)
library(foreign)
library(gbm)
library(parallel)
library(PresenceAbsence)

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

# # Now that we have footprint and distance for each year, so all surveys
# # Filter the model data to include only surveys post 2018. This is done because the landscape we are using is 
# # based on the 2018 road network. Culverts older than that may have been fixed, while newer surveys may still 
# # represent that status of the watershed a few years ago seeing as we don't have many reclaimed culverts in the data.
# model.data <- model.data[model.data$SurveyDate > "2018-01-01", ]

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
                                                                   boot = boot,
                                                                   path = "results/hanging-culvert-model/version-5/bootstrap/"), 
                                                      error = function(e) e))
        


stopCluster(core.input)

# Create the confusion matrix for the first model
load("results/hanging-culvert-model/version-5/bootstrap/hanging-culvert-model_1.Rdata")
model.data$Prediction <- predict.gbm(brt.model, model.data,
                                     n.trees = brt.model$gbm.call$best.trees, type="response")

# Assessing if convert to favorability is reasonable
model.data$Favorability <- plogis( qlogis(model.data$Prediction) - qlogis(0.9229339))

# We use the MaxSens+Spec threshold approach as it is a more conservative threshold compared to Kappa.
# It incorrectly predicts more hanging culverts when not present, but has high accuracy is predicting known hanging culverts.
# This approach is minimizing the number of false negatives (hanging culvert is predicted to be passable)
# and is okay if that results in more passable culverts being identified as hanging.
pass.threshold <- optimal.thresholds(model.data[, c("Node", "Passability", "Prediction")],
                                     threshold = 101,
                                     opt.methods = "MaxSens+Spec")$Prediction

# Create the confusion matrix for reporting purposes
confusion.matrix <- confusionMatrix(data = factor(ifelse(model.data$Prediction >= pass.threshold, 1, 0)), 
                                    reference = factor(model.data$Passability))
pROC::auc(model.data$Passability, model.data$Prediction)

# Save the model output with the commented fit statistics
comment(brt.model) <- paste0("100 Bootstrap iterations. First bootstrap uses full dataset; ",
                             "AUC = ", round(pROC::auc(model.data$Passability, model.data$Prediction), 3), "; ",
                              "Accuracy = ", round(confusion.matrix$overall["Accuracy"], 3), "; ",
                              "Kappa = ", round(confusion.matrix$overall["Kappa"], 3), 
                              "; Created September 2, 2024")
save(brt.model, pass.threshold, confusion.matrix, file = "results/hanging-culvert-model/version-5/hanging-culvert-model-stats.Rdata")

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
load("results/hanging-culvert-model/version-5/bootstrap/hanging-culvert-model_1.Rdata")
load("results/hanging-culvert-model/version-5/hanging-culvert-model-stats.Rdata")

# Define analysis year
huc.scale <- 6
hfi.year <- 2018

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
watershed.average <- data.frame(Watershed = watershed.ids,
                                NCulverts = NA,
                                MeanPass = NA,
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
        
        # Reclassify the road data (Gravel, Unimproved, Paved)
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
        
        # Subset the network to include only Culverts
        temp.node <- temp.node[!(temp.node$Class %in% c("Split", "Bridge")), ]
        
        # Store the results
        watershed.average[watershed.average$Watershed == HUC, c("NCulverts", "MeanPass", "BinaryPass")] <- c(nrow(temp.node), 
                                                                                                    mean(temp.node$Up),
                                                                                                    mean(ifelse(temp.node$Up > pass.threshold, 
                                                                                                                1, 0)))
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
write_sf(culvert.gis, dsn = "results/hanging-culvert-model/version-5/provincial-hanging-culvert-model.shp")

# Load the previous hanging culvert model and assess fit between the two versions
old.model <- read_sf("results/hanging-culvert-model/version-1/provincial-culvert-model_2021-06-04.shp")

# Align the two sets of data
old.model <- as.data.frame(st_drop_geometry(old.model[, c("HUC_6", "NCulverts", "MeanPass", "BinaryPass")]))
old.model <- old.model[!duplicated(old.model), ]
new.model <- culvert.gis[, c("HUC_6", "NCulverts", "MeanPass", "BinaryPass")]
new.model <- new.model[!duplicated(new.model), ]
colnames(new.model) <- c("HUC_6", "NCulvertsV3", "MeanPassV3", "BinaryPassV3")
culvert.predictions <- merge.data.frame(old.model, new.model, by = "HUC_6")

# Difference 
culvert.predictions$Difference <- culvert.predictions$MeanPassV3 - culvert.predictions$MeanPass


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


################### NOTES STORE THE BOOTSTRAP PREDICTION (UP_1, UP_2, etc). NEED TO DECIDE ON WINDOW FOR 
# Culvert Surveys # USING SURVEYS. IF PASSABLE, SURVEY WINDOW DEGRADES TO PREDICTION. IF IMPASSABLE, STAYS BROKEN. 
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

# Define analysis year
huc.scale <- 6
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("data/base/gis/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

# Define path for the bootstrap models
boot.list <- list.files("results/hanging-culvert-model/version-5/bootstrap/", full.names = TRUE)

# Define the cores and objects required for for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("huc.scale", "watershed.ids", "hfi.series",
                            "culvert_survey", "boot.list"))
clusterEvalQ(core.input, {
        
        # Load libraries
        library(dismo)
        library(gbm)
        
})

# Loop through each available HFI inventory
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(culvert_survey(path = paste0(getwd(), "/data/processed/huc-", huc.scale, "/", 
                                                                            hfi, "/connectivity/network_", huc, ".Rdata"),
                                                              hfi = hfi,
                                                              boot.path = boot.list), error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()
