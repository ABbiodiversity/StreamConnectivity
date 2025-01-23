# ---
# title: "Modeling Hanging Culvert Passability"
# author: "Brandon Allen"
# created: "2025-01-13"
# inputs: ["2_pipeline/culverts/culvert-model-attributes.Rdata";
#          "0_data/external/watersheds/boundary/HUC_8_EPSG3400.shp;
#          "network_HUC.Rdata - One for each year and HUC watershed"]
# outputs: ["3_output/hanging-culvert-model/version-4/hanging-culvert-model-stats.Rdata";
#           "3_output/hanging-culvert-model/version-4/hanging-culvert-model.Rdata";
#           "3_output/hanging-culvert-model/version-4/bootstrap/bootstrap models"]
# notes: 
#   "Using the available culvert survey information, create a passability model for hanging culverts.
#    As the frequency of hanging culverts will change over time, we standardize the probability predictions
#    as a favorability index. This means regardless of the prevalence of hanging culverts between model versions,
#    we should have a similar range a predictions between versions (e.g., <0.5 means more likely to be hanging)"
# ---

# 1.0 Exploratory Modeling ----

# 1.1 Clear memory ----
rm(list=ls())
gc()

# 1.2 Load libraries and culvert data ----
library(caret)
library(dismo)
library(foreign)
library(gbm)
library(ggplot2)
library(ggpubr)
library(MetBrewer)
library(PresenceAbsence)
library(sf)
load("2_pipeline/culverts/culvert-model-attributes.Rdata")

# 1.3 Filter the culvert data to include only hanging culverts and passable culverts ----
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

# 1.4 Create the variable set used in the model exploration phase ----

# Climate Moisture Index (MAP - Eref)
model.data$CMI <- model.data$MAP - model.data$Eref

# Total basin slope
# Total Difference between max and point elevation, divided by total upstream distance from culvert
model.data$BasinSlope <- (model.data$WatershedElevationMax - model.data$Elevation) / model.data$Distance

# Convert distance from m to km
model.data$Distance <- model.data$Distance / 1000

# # These variables were evaluated, but didn't improve model fit substantially
# # Reclassify the road data (Gravel, unimproved, paved?)
# model.data$RoadClass <- "Paved"
# model.data$RoadClass[model.data$FeatureType %in% c("ROAD-GRAVEL-1L", "ROAD-GRAVEL-2L",
#                                                    "ROAD-UNPAVED-2L")] <- "Gravel"
# model.data$RoadClass[model.data$FeatureType %in% c("FORD-WINTER-XING", "ROAD-UNCLASSIFIED",
#                                                    "ROAD-UNIMPROVED", "ROAD-WINTER-ACCESS",
#                                                    "ROAD-WINTER-ROAD", "TRAIL-ATV",
#                                                    "TRUCK-TRAIL")] <- "Unimproved"
# model.data$RoadClass <- factor(model.data$RoadClass)
# 
# # Factor natural region
# model.data$NaturalRegion <- factor(model.data$NaturalRegion)
# 
# # Strahler Order
# model.data$Strahler <- factor(model.data$Strahler)
# 
# # Elevation difference (Max elevation - point)
# model.data$Elevation.Diff <- model.data$WatershedElevationMax - model.data$Elevation


# 1.5 Boosted Regression Tree ----

# Define coefficients 
response.variable <- "Passability"
coefficients <- c("SlopePoint", "Confluence",
                  "Distance", "CMI", "BasinSlope")

# Calculate the brt model
brt.model <- gbm.step(data = model.data, gbm.x = coefficients, gbm.y = response.variable, 
                      family = "bernoulli", tree.complexity = 5,
                      learning.rate = 0.001, bag.fraction = 0.5, n.folds = 10,
                      prev.stratify = TRUE, max.trees = 20000)
gbm.plot(brt.model)

# Create the confusion matrix for reporting purposes
# Make prediction
model.data$Prediction <- predict.gbm(brt.model, model.data,
                            n.trees = brt.model$gbm.call$best.trees, type="response")

pass.threshold <- optimal.thresholds(model.data[, c("Node", "Passability", "Prediction")],
                                     threshold = 101,
                                     opt.methods = "MaxKappa")$Prediction

confusion.matrix <- confusionMatrix(data = factor(ifelse(model.data$Prediction >= pass.threshold, 1, 0)), 
                                    reference = factor(model.data$Passability))
pROC::auc(model.data$Passability, model.data$Prediction)

# 1.6 Provincial prediction ----

# Define analysis year
huc.scale <- 6
hfi.year <- 2018

# Define list watersheds to make predictions for
watershed.ids <- read.dbf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))
watershed.average <- data.frame(Watershed = watershed.ids,
                                NCulverts = NA,
                                MeanPass = NA,
                                BinaryPass = NA)

# Loop through each watershed to assess the distribution of hanging culverts throughout the province
for(HUC in watershed.ids) {
        
        # Load the appropriate Rdata with the culvert information
        load(paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                    hfi.year, "/connectivity/network_", HUC, ".Rdata"))
        
        # If there are culverts in the watershed, proceed with the match
        if(is.null(watershed.network[["Edge_Cleaned"]])) {
                
                next()
                
        }
        
        # Pull out culvert data
        temp.node <- watershed.network[["Edge_Cleaned"]]
        
        # Add CMI
        temp.node$CMI <- temp.node$MAP - temp.node$Eref
        
        # Total stream slope
        temp.node$BasinSlope <- (temp.node$WatershedElevationMax - temp.node$Elevation) / temp.node$Distance
        
        # Convert distance from m to km
        temp.node$Distance <- temp.node$Distance / 1000
        
        # Elevation difference (Max elevation - point)
        temp.node$Elevation.Diff <- temp.node$WatershedElevationMax - temp.node$Elevation
        
        # Make prediction
        temp.node$Up <- predict.gbm(brt.model, temp.node,
                                    n.trees = brt.model$gbm.call$best.trees, type="link")
        
        # Convert to favorability
        prevalence <- sum(brt.model$data$y) / length(brt.model$data$y)
        temp.node$Up <- plogis(temp.node$Up - qlogis(prevalence))
        
        # Subset the network to include only Culverts
        temp.node <- temp.node[!(temp.node$Class %in% c("Split", "Bridge")), ]
        
        # Store the results
        watershed.average[watershed.average$Watershed == HUC, c("NCulverts", "MeanPass")] <- c(nrow(temp.node), 
                                                                                               mean(temp.node$Up))
        rm(temp.node)
        
        print(HUC)
        
}

# Standardize the watershed results for situations where no culverts are present
watershed.average$NCulverts[is.na(watershed.average$NCulverts)] <- 0
watershed.average$MeanPass[is.na(watershed.average$MeanPass)] <- 1
colnames(watershed.average)[1] <- "HUC_6"

# Save the predictions
culvert.gis <- read_sf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.shp")
culvert.gis <- merge(culvert.gis, watershed.average, by = "HUC_6")

spatial.plot <- ggplot() +
        geom_sf(data = culvert.gis, aes(fill = MeanPass)) +
        scale_fill_gradientn(name = paste0("Mean\nPassability"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle("Hanging Culvert Model (Version 4)") +
        theme_light()

# 1.7 Version Comparison ----
previous.version <- read_sf("3_output/hanging-culvert-model/version-1/provincial-culvert-model_2021-06-04.shp")

previous.version.plot <- ggplot() +
        geom_sf(data = previous.version, aes(fill = MeanPass)) +
        scale_fill_gradientn(name = paste0("Mean\nPassability"), colors = met.brewer(name = "Hiroshige", n = 100, type = "continuous"), guide = "colourbar") +
        ggtitle("Hanging Culvert Model (Version 1)") +
        theme_light()

ggsave(filename = paste0("3_output/figures/hanging-culvert-model-comparison.jpeg"), 
       plot = ggarrange(previous.version.plot, spatial.plot, ncol = 2, nrow = 1),
       height = 900,
       width = 1200, 
       dpi = 72,
       quality = 100,
       units = "px")

rm(list=ls())
gc()

# 2.0 Bootstrapped Models ----

# 2.1 Clear memory ----
rm(list=ls())
gc()

# 2.2 Load libraries, source functions, and culvert data ----
library(caret)
library(dismo)
library(gbm)
library(PresenceAbsence)
source("1_code/r-scripts/culvert-passability_functions.R")
load("2_pipeline/culverts/culvert-model-attributes.Rdata")

# 2.3 Filter the culvert data to include only hanging culverts and passable culverts ----
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

# 2.4 Create the variable set used in the model exploration phase ----

# Climate Moisture Index (MAP - Eref)
model.data$CMI <- model.data$MAP - model.data$Eref

# Total basin slope
# Total Difference between max and point elevation, divided by total upstream distance from culvert
model.data$BasinSlope <- (model.data$WatershedElevationMax - model.data$Elevation) / model.data$Distance

# Convert distance from m to km
model.data$Distance <- model.data$Distance / 1000

# 2.5 Parallel Processing of Boosted Regression Tree ----

# Define coefficients 
response.variable <- "Passability"
coefficients <- c("SlopePoint", "Confluence",
                  "Distance", "CMI", "BasinSlope")

# Define the cores and objects required for for parallel processing
n.clusters <- 14
core.input <- makeCluster(n.clusters)
clusterExport(core.input, c("model.data", "brt_function", "coefficients", "response.variable"))
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
                                                                   response.variable = response.variable, 
                                                                   coefficients = coefficients,
                                                                   path = "3_output/hanging-culvert-model/version-4/bootstrap/"), 
                                                      error = function(e) e))



stopCluster(core.input)

# Create the confusion matrix for the first model
load("3_output/hanging-culvert-model/version-4/bootstrap/hanging-culvert-model_1.Rdata")
model.data$Prediction <- predict.gbm(brt.model, model.data,
                                     n.trees = brt.model$gbm.call$best.trees, type="link")

# Assessing if convert to favorability is reasonable
prevalence <- sum(brt.model$data$y) / length(brt.model$data$y)
model.data$Favorability <- plogis(model.data$Prediction - qlogis(prevalence))
model.data$Prediction <- plogis(model.data$Prediction)

# We use the MaxSens+Spec threshold approach as it is a more conservative threshold compared to Kappa.
# It incorrectly predicts more hanging culverts when not present, but has high accuracy is predicting known hanging culverts.
# This approach is minimizing the number of false negatives (hanging culvert is predicted to be passable)
# and is okay if that results in more passable culverts being identified as hanging.
pass.threshold <- optimal.thresholds(model.data[, c("Node", "Passability", "Prediction", "Favorability")],
                                     threshold = 101,
                                     opt.methods = "MaxKappa")

# Create the confusion matrix for reporting purposes
confusion.matrix <- confusionMatrix(data = factor(ifelse(model.data$Prediction >= pass.threshold$Prediction, 1, 0)), 
                                    reference = factor(model.data$Passability))
pROC::auc(model.data$Passability, model.data$Prediction)

# Save the model output with the commented fit statistics
comment(brt.model) <- paste0("100 Bootstrap iterations. First bootstrap uses full dataset; ",
                             "AUC = ", round(pROC::auc(model.data$Passability, model.data$Prediction), 3), "; ",
                             "Accuracy = ", round(confusion.matrix$overall["Accuracy"], 3), "; ",
                             "Kappa = ", round(confusion.matrix$overall["Kappa"], 3), 
                             "; Created January 14, 2025")
save(brt.model, pass.threshold, confusion.matrix, file = "3_output/hanging-culvert-model/version-4/hanging-culvert-model-stats.Rdata")


# 3.0 Provincial Predictions ----

# 3.1 Clear memory ----
rm(list=ls())
gc()

# 3.2 Load libraries and source functions ----
library(foreach)
library(foreign)
library(parallel)
source("1_code/r-scripts/culvert-passability_functions.R")

# 3.3 Define watersheds and analysis years ----
huc.scale <- 6
hfi.series <- c(2010, 2014, 2016, 2018, 2019, 2020, 2021) # Define HFI years (2010, 2014, 2016, 2018, 2019, 2020, 2021)
watershed.ids <- read.dbf("0_data/external/watersheds/boundary/HUC_8_EPSG3400.dbf")
watershed.ids <- unique(as.character(watershed.ids$HUC_6))

# 3.4 Define bootstrap path and organize parallel processing ----
boot.list <- list.files("3_output/hanging-culvert-model/version-4/bootstrap/", full.names = TRUE)

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

# 3.5 Loop through each HFI and analysis unit ----
foreach(hfi = hfi.series) %dopar% 
        
        parLapply(core.input, 
                  watershed.ids, 
                  fun = function(huc) tryCatch(culvert_survey(path = paste0(getwd(), "/2_pipeline/huc-", huc.scale, "/", 
                                                                            hfi, "/connectivity/network_", huc, ".Rdata"),
                                                              hfi = hfi,
                                                              boot.path = boot.list), error = function(e) e)
        )

stopCluster(core.input)

# Clear memory
rm(list=ls())
gc()
