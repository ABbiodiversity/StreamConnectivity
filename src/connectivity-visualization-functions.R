#
# Title: Functions required for visualizing connectivity
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Define functions required for visualizing connectivity using ggplot
# Keywords: Watershed visualization
#

###########################
# Watersehd Visualization #
###########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

watershed_visualization <- function(watershed, hfi.year, watershed.unit) {
  
  require(ggplot2)
  require(RColorBrewer)
  require(sf)
  
  # Create the column for which ggplot will call from
  watershed["Connectivity"] <- watershed[[paste0(watershed.unit, "_C")]]

  # Create plot
  watershed.plot <- ggplot(data = watershed) +
    geom_sf(aes(fill = Connectivity)) +
    scale_fill_gradientn(colours = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                                      "#e0f3f8","#abd9e9", "#74add1", "#4575b4", "#313695"),
                         na.value = "grey50",
                         breaks = seq(0, 1, 0.2),
                         limits = c(0,1)) +
    theme(axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=10),
          axis.title.y = element_text(size=10),
          axis.title.x = element_text(size=10),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  return(watershed.plot)
  
}



