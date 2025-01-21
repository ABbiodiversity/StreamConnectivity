# ---
# title: "Culvert passability functions"
# author: "Brandon Allen"
# created: "2025-01-15"
# notes: 
#   "Functions and associated wrappers for modeling and predicting culvert passability."
# ---

#' [BRT Bootstrap]
#'
#' [Wrapper function used for boosted regression tree model to facilitate parallel processing.]
#'
#' @param [data] [Cleaned hanging culvert data used for modeling.]
#' @param [boot] [Number of bootstrap iterations.]
#' @param [response.variable] [Response variable for the model "Passability".]
#' @param [coefficients] [Coefficients used for predicting hanging culverts.]
#' @param [path] [File path for storing the individual models.]
#' @return [Saves individual model objects to the designated file path.]
#' 

brt_function <- function(data = model.data,
                         boot = boot,
                         response.variable = "Passability",
                         coefficients = NA,
                         path = NA) {
        
        # If path is NA, throw warning
        if(is.na(path)) {
                
                return("No path defined for saving bootstrap iterations")
                
        }
        
        # If coefficient is NA, throw warning
        if(is.na(path)) {
                
                return("No coefficients defined for the model")
                
        }
        
        # For the first bootstrap, use the original dataset
        if(boot != 1) {
                
                # Perform the boostrap sampling
                data <- data[sample(1:nrow(data), 
                                                nrow(data), replace=TRUE), ]
                
        }
        
        # Calculate the brt model
        response.variable <- (1:ncol(data))[colnames(data) %in% response.variable]
        model.coef <- (1:ncol(data))[colnames(data) %in% coefficients]
        
        brt.model <- gbm.step(data = data, gbm.x = model.coef, gbm.y = response.variable, 
                              family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.001, bag.fraction = 0.5, n.folds = 10,
                              prev.stratify = TRUE, max.trees = 20000)
        
        # Save the output
        save(brt.model, file = paste0(path, "hanging-culvert-model_", boot, ".Rdata"))
        
}

#' [Culvert Survey]
#'
#' [Function used for making predictions to individual culverts and applying inspection data.]
#'
#' @param [path] [Path to the individual R objects that store the processed network "e.g., network_HUC.Rdata".]
#' @param [hfi] [Analysis year.]
#' @param [boot.path] [File paths for the bootstrap hanging culvert models.]
#' @return [Saves a new data frame to the original object "Edge_Predicted".]
#' 
#' 

culvert_survey <- function(path = NA,
                           hfi = NA,
                           boot.path = NA) {
        
        # If path is NA, throw warning
        if(is.na(path)) {
                
                return("No path defined.")
                
        }
        
        # If file does not exists, throw warning
        if(!file.exists(path)) {
                
                return("File does not exist.")
                
        }
        
        # Load the appropriate Rdata with the culvert information
        load(path)
        
        # If there are culverts in the watershed, proceed with the match
        if(!is.null(watershed.network[["Edge_Cleaned"]])) {
                
                # Pull out culvert data
                # If there are surveys available use the Edge_Survey
                # Otherwise default to Edge_Cleaned
                if(!is.null(watershed.network[["Edge_Surveys"]])) {
                        
                        temp.node <- watershed.network[["Edge_Surveys"]]
                        
                } else {
                        
                        temp.node <- watershed.network[["Edge_Cleaned"]]
                        
                        }
                
                
                # Add the variables that are relevant to the hanging culvert model
                # CMI
                temp.node$CMI <- temp.node$MAP - temp.node$Eref
                
                # Total basin slope
                # Total Difference between max and point elevation, divided by total upstream distance from culvert
                temp.node$BasinSlope <- (temp.node$WatershedElevationMax - temp.node$Elevation) / temp.node$Distance
                
                # Convert distance from m to km
                temp.node$Distance <- temp.node$Distance / 1000
                
                # If it is a split or bridge, fix to 1 (passable)
                bridge.split.logical <- temp.node$Class %in% c("Split", "Bridge")
                temp.node$Up[bridge.split.logical] <- 1
                
                # Use all surveys prior to the focal year
                # As we have already removed duplicate surveys, this is simple a doublecheck 
                # there aren't stray culverts.
                survey.logical <- !is.na(temp.node$SurveyDate)
                
                # Identify culverts of concern
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
                
                # For each of the bootstrap iterations, create the 90% confidence interval
                prediction.matrix <- matrix(data = NA, ncol = 100, nrow = nrow(temp.node))
                for(boot in 1:length(boot.path)) {
                        
                        # Load the boosted regression tree
                        load(boot.path[boot])
                        
                        pass.prob <- predict.gbm(brt.model, 
                                                 temp.node,
                                                 n.trees = brt.model$gbm.call$best.trees, 
                                                 type="link")
                        prevalence <- sum(brt.model$data$y) / length(brt.model$data$y)
                        prediction.matrix[, boot] <- plogis(pass.prob - qlogis(prevalence))
                        
                }
                
                prediction.boot <- data.frame(Mean = apply(prediction.matrix, MARGIN = 1, FUN = mean),
                                              Lower = apply(prediction.matrix, MARGIN = 1, FUN = function(x) quantile(x, 0.1)),
                                              Upper = apply(prediction.matrix, MARGIN = 1, FUN = function(x) quantile(x, 0.9)))
                
                # Add the Mean, Upper, and Lower predictions for the relevant culverts
                temp.node$ModelMean <- NA
                temp.node$ModelLower <- NA
                temp.node$ModelUpper <- NA
                
                temp.node$ModelMean[temp.node$ModelPassability] <- prediction.boot$Mean[temp.node$ModelPassability]
                temp.node$ModelLower[temp.node$ModelPassability] <- prediction.boot$Lower[temp.node$ModelPassability]
                temp.node$ModelUpper[temp.node$ModelPassability] <- prediction.boot$Upper[temp.node$ModelPassability]
                
                # Replace the with file with the predicted version then save
                watershed.network[["Edge_Predicted"]] <- temp.node
                save(watershed.network, file = path)
                
        }
        
}