#
# Title: Stream standardization procedure
# Created: September 1st, 2021
# Last Updated: September 1st, 2021
# Author: Brandon Allen
# Objectives: Standardization of the implemented stream network
# Keywords: Notes, Stream Network
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All paths defined in this script are local
#
##################
# Stream Network #
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Source data cleaning scripts
source("src/data-cleaning-functions.R")

# Create a list of the stream layers that are included in the current analysis region.

stream.layer.list <- list(column_id = list(SSO0 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO1 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO2 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO3 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO4 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO5 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO6 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO7 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO8 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO9 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO10 = c("FID", "Shape", "Environm_4", "Shape_STLe", "Environ_16"),
                                           SSO11 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO12 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO13 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO14 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO15 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),                                           
                                           SSO11 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO16 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO17 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO18 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO19 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15"),
                                           SSO20 = c("FID", "Shape", "Environm_3", "Shape_STLe", "Environ_15")), 
                          stream_layer = list(SSO0 = "data/base/gis/strahler_stream_order/archydro2/SSO0.shp",
                                              SSO1 = "data/base/gis/strahler_stream_order/archydro2/SSO1.shp",
                                              SSO2 = "data/base/gis/strahler_stream_order/archydro2/SSO2.shp",
                                              SSO3 = "data/base/gis/strahler_stream_order/archydro2/SSO3.shp",
                                              SSO4 = "data/base/gis/strahler_stream_order/archydro2/SSO4.shp",
                                              SSO5 = "data/base/gis/strahler_stream_order/archydro2/SSO5.shp",
                                              SSO6 = "data/base/gis/strahler_stream_order/archydro2/SSO6.shp",
                                              SSO7 = "data/base/gis/strahler_stream_order/archydro2/SSO7.shp",
                                              SSO8 = "data/base/gis/strahler_stream_order/archydro2/SSO8.shp",
                                              SSO9 = "data/base/gis/strahler_stream_order/archydro2/SSO9.shp",
                                              SSO10 = "data/base/gis/strahler_stream_order/archydro2/SSO10.shp",
                                              SSO11 = "data/base/gis/strahler_stream_order/archydro2/SSO11.shp",
                                              SSO12 = "data/base/gis/strahler_stream_order/archydro2/SSO12.shp",
                                              SSO13 = "data/base/gis/strahler_stream_order/archydro2/SSO13.shp",
                                              SSO14 = "data/base/gis/strahler_stream_order/archydro2/SSO14.shp",
                                              SSO15 = "data/base/gis/strahler_stream_order/archydro2/SSO15.shp",
                                              SSO16 = "data/base/gis/strahler_stream_order/archydro2/SSO16.shp",
                                              SSO17 = "data/base/gis/strahler_stream_order/archydro2/SSO17.shp",
                                              SSO18 = "data/base/gis/strahler_stream_order/archydro2/SSO18.shp",
                                              SSO19 = "data/base/gis/strahler_stream_order/archydro2/SSO19.shp",
                                              SSO20 = "data/base/gis/strahler_stream_order/archydro2/SSO20.shp"))

# Run the stream standardization if the stream layer has not already been created

if(!file.exists("data/base/gis/strahler_stream_order/cleaned-network/stream_network_merged.shp")) {
        
        stream_standardization(stream.list = stream.layer.list, 
                               workspace = "data/base/gis/strahler_stream_order/")
        
}

rm(list=ls())
gc()