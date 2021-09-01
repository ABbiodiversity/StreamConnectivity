# Stream Connectivity Indicator for Alberta

> Version 1.0 2021-09-01

## Introduction

The Alberta Environment and Parks (AEP), in partnership with the Alberta Biodiversity Monitoring Institute (ABMI), developed a stream connectivity indicator to help inform the environmental planning of Alberta’s watersheds. The goals of this indicator are to:

1)	Quantify the degree of connectivity within our stream systems;
2)	Identify and monitor changes in anthropogenic barriers (i.e., culverts, dams, bridges) as they relate to biodiversity either directly or indirectly; and
3)	Incorporate variation in the location and probability of movement through different types of barriers.

The selection and design of the indicator also considered some key requirements related to indicator implementation, such as use at multiple spatial scales and cost-effectiveness for monitoring and reporting on a regular basis.

For this indicator, we chose to use the Connectivity Status Index proposed by Diebel et al. (2014). This is a biological indicator developed to identify patterns of functional connectivity on the landscape. This index provides us with the flexibility to include measures such as habitat type, quality, and species dispersal limitations, while also incorporating variable resistance values to each barrier on the stream network

## Repository Contents

This repository contains the R code (**src/**) required for reproducing the results presented on GeoDiscover Alberta (https://geodiscover.alberta.ca/geoportal/rest/metadata/item/7025fe90840a4680b4011b7100c21644/html). The Stream Connectivity Indicator for Alberta is a geospatial product that presents the connectivity status of all HUC 8 watersheds in Alberta at four timesteps: 2010, 2014, 2016, and 2018. The base GIS layers used to calculate this indicator are not available due to their size and sensitivity. For those who are interested, we have created a technical document describing the methodology of this indicator (LINK). For any questions regarding the scripts presented in this repository, GeoDiscover product, technical documentation, or about other applications of this indicator, please contact Brandon Allen (ABMI Applied Ecologist) at ballen@ualberta. 

## Literature cited

* Diebel, M.W., Fedora, M., Cogswell, S., and Ohanley, J.R. 2014. Effects of road crossings on habitat connectivity for stream-resident fish. River Research and Applications DOI:10.1002/rra.2822


## Example outputs

![GitHub Logo](/examples/example-watershed.png)
Figure 1 - The connectivity status for each stream segment within a watershed and the location of all barriers (black dots).

![GitHub Logo](/examples/example-connectivity.png)
Figure 1 - The 2010 connectivity status of all HUC 8 watersheds within Alberta.