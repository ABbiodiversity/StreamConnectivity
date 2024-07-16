#
# Title: Culvert Passability Functions
# Created: July 15th, 2024
# Last Updated: July 16th, 2024
# Author: Brandon Allen
# Objectives: Bootstrapping wrapper for the culvert passability model
# Keywords: BRT Bootstrap, Culvert Survey
#

#################
# BRT Bootstrap # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define the bootstrap function
brt_function <- function(data = model.data,
                         boot = boot,
                         path = NA) {
        
        # If path is NA, throw warning
        if(is.na(path)) {
                
                return("No path defined for saving bootstrap iterations")
                
        }
        
        # For the first bootstrap, use the original dataset
        if(boot != 1) {
                
                # Perform the boostrap sampling
                model.data <- model.data[sample(1:nrow(model.data), 
                                                nrow(model.data), replace=TRUE), ]
                
        }
        
        # Calculate the brt model
        brt.model <- gbm.step(data = model.data, gbm.x = c(14,22,28,30,37,38), 
                              gbm.y = 34, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.001, bag.fraction = 0.5, n.folds = 10,
                              prev.stratify = TRUE, max.trees = 20000)
        
        # Save the output
        save(brt.model, file = paste0(path, "hanging-culvert-model_", boot, ".Rdata"))
        
}

##################
# Culvert Survey # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

culvert_survey <- function(path = NA,
                           hfi = NA) {
        
        # If path is NA, throw warning
        if(is.na(path)) {
                
                return("No path defined")
                
        }
        
        # Load the appropriate Rdata with the culvert information
        load(path)
        
        # If there are culverts in the watershed, proceed with the match
        if(!is.null(watershed.network[["Edge_Cleaned"]])) {
                
                # Pull out culvert data
                temp.node <- watershed.network[["Edge_Cleaned"]]
                
                # Add the variables that are relevant to the hanging culvert model
                # CMI
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
                
                # If it is a split or bridge, fix to 1 (passable)
                bridge.split.logical <- temp.node$Class %in% c("Split", "Bridge")
                temp.node$Up[bridge.split.logical] <- 1
                
                # Use all surveys prior to the focal year
                survey.logical <- temp.node$SurveyDate < as.Date(paste0(hfi, "-01-01"))
                survey.logical[is.na(survey.logical)] <- FALSE
                concerns.logical <- temp.node$Passability %in% c("Serious Concerns", "Concerns", "Some Concerns")
                no.concerns.logical <- temp.node$Passability %in% c("No Concerns")
                
                # If there are no surveys present, default to a FALSE logical
                if(length(survey.logical) == 0) {
                        
                        survey.logical <- rep(FALSE, nrow(temp.node))
                        concerns.logical <- rep(FALSE, nrow(temp.node))
                        no.concerns.logical <- rep(FALSE, nrow(temp.node))
                }
                
                # Change passability value for blocked culverts
                temp.node$Up[survey.logical & concerns.logical] <- 0
                
                # Change passability value for functional culverts
                temp.node$Up[survey.logical & no.concerns.logical] <- 1
                
                # If a dam is present, define as 0
                temp.node$Up[!is.na(temp.node$Dam)] <- 0
                temp.node$Class[!is.na(temp.node$Dam)] <- "Dam"
                
                # If crossing or stream intersection occurs in the minable region, fix to 0
                mineable.logical <- temp.node$Mineable == "Inside"
                temp.node$Up[mineable.logical] <- 0
                
                # Define the logical to define which culverts should have predictions applied to them
                # All culverts without a survey or doesn't occur in the mineable region
                temp.node$ModelPassability <- FALSE
                temp.node$ModelPassability[temp.node$Class == "Culvert" &
                                                   !survey.logical & 
                                                   !(mineable.logical)] <- TRUE
                
                # Replace the with file with the predicted version then save
                watershed.network[["Edge_Cleaned"]] <- temp.node
                save(watershed.network, file = path)
                
        }
        
}