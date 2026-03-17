<!--
<img src="https://drive.google.com/uc?id=1fgYuG7jpnekZrkoL_PdVUnSiUFBFX-vI" alt="Logo" width="150" style="float: left; margin-right: 10px;">
-->

<img src="https://drive.google.com/uc?id=1szqLViKqTX5C1XF8uV7HbIst0i6Xvv7g" alt="Logo" width="300">


# Stream Connectivity Indicator for Alberta
> Version 2.0 2026-03-216

![Maintenance](https://img.shields.io/badge/Status-Maintenance-green)
![Languages](https://img.shields.io/badge/Languages-R%20%7C%20Python-blue)
![Programs](https://img.shields.io/badge/Programs-R%20%7C%20ArcPro-blue)


## Introduction

The Alberta Environment and Protected Areas (AEPA), in partnership with the Alberta Biodiversity Monitoring Institute (ABMI), developed a stream connectivity indicator to help inform the environmental planning of Alberta’s watersheds. The goals of this indicator are to:

- Quantify the degree of connectivity within our stream systems.
- Identify and monitor changes in anthropogenic barriers (i.e., culverts, dams, bridges) as they relate to biodiversity either directly or indirectly.
- Incorporate variation in the location and probability of movement through different types of barriers.
- Can be implemented at multiple spatial scales.
- Cost effective in monitoring and reporting.

For this indicator, we chose to use the Connectivity Status Index proposed by Diebel et al. (2014). This is a biological indicator developed to identify patterns of functional connectivity on the landscape. This index provides us with the flexibility to include measures such as habitat type, quality, and species dispersal limitations, while also incorporating variable resistance values to each barrier on the stream network.

## Repository Contents

This repository contains the R code (**1_code/**) required for reproducing the results presented on GeoDiscover Alberta (https://open.alberta.ca/opendata/gda-eb11e484-0654-4e2a-8a31-54e0f5fab2d0). The Stream Connectivity Indicator for Alberta is a geospatial product that presents the connectivity status of all HUC 8 watersheds in Alberta at eight timesteps: 2010, 2014, 2016, 2018, 2019, 2020, 2021, and 2022. The base GIS layers used to calculate this indicator are not available due to their size and sensitivity. We have created a technical document describing the methodology of this indicator (https://open.alberta.ca/publications/stream-connectivity-indicator-for-alberta). 

For any questions regarding the scripts presented in this repository, GeoDiscover product, technical documentation, or about other applications of this indicator, please contact Brandon Allen (ABMI Senior Terrestrial Ecologist) at brandon.allen@ualberta. 

## Updates to Version 2.0
### Culvert Surveys

 - increased the amount of culvert surveys used to calculate both the hanging culvert model and final stream connectivity scores from 3,800 surveys to 40,650 surveys.

- Updated our model validation procedure for the hanging culvert model. In version 1.0, model validation of the boosted regression trees was assessed using a 10-fold cross validation procedure with a bag fraction of 0.5. In version 2.0, we partitioned the data into training (80%) and testing (20%) before performing the cross-validation procedure.

- Implemented a bootstrap procedure for the hanging culvert model. This allows us to calculate the uncertainty in our predictions of culvert passability. These results are not presented but are available to support supplementary analyses.

- The underlying R code base was updated to improve reproducibility, clarify parallel processing procedures, and evaluate changes between version 1.0 and version 2.0 of the indicator.

## Literature cited

* Diebel, M.W., Fedora, M., Cogswell, S., and Ohanley, J.R. 2014. Effects of road crossings on habitat connectivity for stream-resident fish. River Research and Applications DOI:10.1002/rra.2822
