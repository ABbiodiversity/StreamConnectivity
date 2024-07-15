#
# Title: Culvert Passability Functions
# Created: July 15th, 2024
# Last Updated: July 15th, 2024
# Author: Brandon Allen
# Objectives: Bootstrapping wrapper for the culvert passability model
# Keywords: BRT Bootstrap
#

#################
# BRT Bootstrap # 
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define the bootstrap function
brt_function <- function(data = model.data,
                         boot = boot) {
        
        # For the first bootstrap, use the original dataset
        if(boot != 1) {
                
                # Perform the boostrap sampling
                model.data <- model.data[sample(1:nrow(model.data), 
                                                nrow(model.data), replace=TRUE), ]
                
        }
        
        
        brt.model <- gbm.step(data = model.data, gbm.x = c(14,22,28,30,37,38), 
                              gbm.y = 34, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.001, bag.fraction = 0.5, n.folds = 10,
                              prev.stratify = TRUE, max.trees = 20000)
        
        return(brt.model)
        
}

