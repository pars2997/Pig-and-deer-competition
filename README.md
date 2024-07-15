## README

Metadata for the manuscript 'Leveraging multiple data sources to assess competition bewteen introduced wild pigs and native deer' published in XXXX by MA Parsons and JK Young.

The data and R code provided here can be used to recreate all results from this manuscript. The data files included several raster layers of topographic and landcover variables, photo data from a camera grid, cougar kill site information, and isotope data for deer and pigs.

The R scripts conduct all analyses including estimating density of deer and pigs, calculating dietary overlap via isotopes, and estimating what landscape variables are related to deer body condition and reproduction.

### DATA FILES

The datafiles include:

1. Raster layers of landcover and topographic data
2. Camera station and photo data from a camera grid used to monitor deer and pigs
3. Cougar kill site locations and data
4. Isotope data

#### GIS Data
#### Vegetation
##### log_forest_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to forest cover. This layer was derived from the 2021 National Landcover Database (https://www.mrlc.gov/data; classes 41, 42, and 43).

##### log_grass_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to herbaceous cover. This layer was derived from the 2021 National Landcover Database (classes 71, 72, 73, 74, 81, 82).

##### log_riparian_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to riparian cover. This layer was derived from the 2021 National Landcover Database (classes 11, 12, 90, 95).

##### log_shrub_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to shrub cover. This layer was derived from the 2021 National Landcover Database (classes 51, 52).

##### agri_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to agricultural cover. This layer was derived from the 2021 National Landcover Database.

##### propforested_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of forest cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propgrass_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of herbaceous cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propriparian_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of riparian cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propshrub289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of shrub cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

#### NDVI***_289cell.tif

These rasters are 30mx30m resolution rasters of the average seasonal NDVI value for four seasons: summer (May-July), fall (August-October), winter (November-January), and spring (February-April). These data are derived from MODIS 16-day data at the 250m scale.

#### Topographic

##### resampled_DEM.tiff

This raster layer is a 30x30m resolution raster of elevation. This layer was derived from the USGS 1/3 arc second digital elevation model (https://apps.nationalmap.gov/downloader/)

##### resampled_Slope.tiff

This raster layer is a 30x30m resolution raster of slope calculated from resampled_DEM.tiff using the terra package in R.
##### resampled_TRI.tiff

This raster layer is a 30x30m resolution raster of the terrain ruggedness index calculated from resampled_DEM.tiff using the terra package in R.

##### resampled_TPI.tiff

This raster layer is a 30x30m resolution raster of the topographic position index calculated from resampled_DEM.tiff using the terra package in R.

##### TRI_500m.tif

This raster layer is a 30x30m resolution raster of the terrain ruggedness index averaged over a 17x17 cell moving window (510 m).

##### TPI_500m.tif

This raster layer is a 30x30m resolution raster of the topographic position index averaged over a 17x17 cell moving window (510m).

#### Other

##### HuntingRaster.tif

This raster layer is a 30mx30m resolution raster that defines the subset of areas withing the study area that are closed to hunting.

##### FHLBoundary.shp
##### FHLRivers.shp
##### FHLBoundNoHoles.shp

These three shape files are the boundary and rivers of the Fort Hunter Liggett and are used for mapping and figure making.

#### Camera Data

##### GridProblems_correctedseasons.csv

This csv file contains the the camera station locations and deployment dates. Each row represents a camera deployment and columns provide information on the deployment. There are 26 columns:

1. Station ID - the name of the camera location, each location was sampled multiple times
2. CSY - the camera-season-year combination of the deployment
3. CSY2 - the camera-season-year combination plus a unique identifier when there were multiple deployments at a location within a season. This occurred when cameras were moved or replace because of technical issues. This is a unique identifier for each deployment.
4. UTM_X - the UTM easting of the camera deployment
5. UTM_Y - the UTM northing of the camera deployment
6. Habitat - a broad habitat category for the camera location
7. Distance - the distance of the 100% detection zone in meters. All detection zones used a 30 degree field of view
8. Set - was this location part of the "even" set of cameras or "odd" set of cameras during our camera rotations
9. Season - season of the camera deployment
10. Year - year of the camera deployment
11. Set Date - the date the camera was deployed
12. Pull - the date the camera was removed from the location
13. Deployment_days - the total length of the deployment 
14. Camera Type - The type of camera that was used: Browning (Brown), Bushnell (Bush), Reconyx. Old (O), New (N), UCD (from project partner)
15. Problem*_From - The start date of a problem with the camera that prevented detection (e.g., card filled, batteries died, animal damaged/displaced). Max of 4 problems for a single deployment
16. Probem*_to - The end date of a problem with the camera
17. Problem*_days - The number of days the problem persisted

##### Deer_and_pig_visits.csv

This csv file contains the information of visits to cameras by deer and pigs and is used to run the Random Encoutner Staying Time model. Each row represents a visit by a single animal to the camera. There are 10 columns:

1. CSY - the camera-season-year combination of the camera deployment where the visit occurred
2. CameraID - the station ID of the camera where the visit occurred
3. Date - the date of the visit
4. Time - the time the visit began
5. season - the season of the visit
6. year - the year of the visit
7. Species - the species for each visit. either deer or pig
8. Start - the start time of the visit in seconds (origin 1/1/1970)
9. Stop - the stop time of the visit in seconds (origin 1/1/1970)
10. visit_length - the duration of the visit in seconds

##### BodyCond_DoeFawn3.csv

This csv file contains information on deer sex, age, and body condition. Each row is data from a single photograph and there are 21 columns:

1. File - the name of the photo file
2. Camera - the camera station where the photo was taken
3. Season - the season when the photo was taken
4. Year - the year when the photo was taken
5. RelativePath - the relative path to access the photo
6. Folder - the working folder during photo processing
7. Date - the date the photo was taken
8. Time - the time the photo was taken
9. ImageQuality - autogenerated value of image quality produced by the software
10. DeleteFlag - autogeneraged value by the software used for processing photos
11. Doe_count - the number of adult female deer in the photo
12. Buck_count - the number of adult female deer in the photo
13. Fawn_count - the number of deer fawns in the photo
14. Comments - any comments about the photo
15. Animal_*_Condition - the body condition score (0-4) of each individual adult deer in the photo. Followed methods from Smiley et al. (2020) https://doi.org/10.1002/wsb.1070
16. Confidence_Condition - the confidence score (0-3) for the body condition score of the deer in the photo



#### Cougar kill site information


##### cougar_cluster_investigations

We are unable to share these raw data because of their sensitive nature. We uploaded a censored version of this data that excludes GPS locations (cougar_clusters_noGPS.csv)

This csv file includes information on cougar kill site investigations. Each row represents a cluster of cougar GPS points that was investigated because it met a criteria to be a possible kill site. Each column provides information on the cluster. There are 62 columns.

1. cluster_id - the name of the GPS cluster investigated
2. cougar_id - the ID of the individual cougar associated with the cluster
3. type - what type of cluster was investigated. The majority are "current" clusters indicated that they were investigated soon after formation. Other options include April (clusters investigated long after formation), day (presumed daybed locations), Den (confirmed den locations), and trapping (area where we deployed bait to trap animals). Only 'current' clusters were used in analysis.
4. collar_serial - the serial number of the cougar's GPS collar
5. n_points - the number of points in the GPS cluster
6. radius - the radius of the GPS cluster
7. duration_h - the duration between the first and last point at the GPS cluster
8. form_date - the date of the first point at the GPS cluster
9. centroid_lat - the latitude of the centroid of the GPS cluster
10. centroid lon - the longitude of the centroid of the GPS cluster
11. inv_date - the date we investigated the GPS cluster
12. personnel - initials of the investigators
13. time_200m - the time of day we reached 200m from the cluster centroid
14. habitat - broad habitat category that the cluster was in
15. habitat2 - broad secondary habitat category that the cluster was in
16. canopy - whether the site had open, moderate, or closed canopy cover
17. carcass_found - whether or not a carcass was found at the site
18. time_found - the time of day the carcass was found
19. carcass_lat - the latitude the carcass was located at
20. carcass_lon - the latitude the carcass was located at
21. species - the species of the located carcass
22. how_id - what features were used to identify the carcass
23. sex - the sex of the carcass
24. how_sex - what features were used to determine the sex of the carcass
25. age - the age of the carcass. Neonate (<6 mo), Fawn/Calf (6 - 18 mo), yearling (18 - 30 mo), adult (>30 mo)
26. how_age - what features were used to determine the age of the carcass
27. est_age - the estimated age of the carcass
28. cacehd - whether or not the caracss was cached currently, previously, or not at all
29. n_prey - the number of prey items at the cluster
30. n_piles - the number of piles of remains
31. condition - the condition of the carcass (intact, dismantled, dispersed)
32. parts - a description of what parts of the carcass were located
33. bite_wounds - whether bite wounds were located
34. hemorrhaging - whether hemorrhaging was located and if so the location of hemorrhaging
35. fed_on - whether the carcass had been fed on
36. per_rem - the rough percentage of biomass remaining on the carcass
37. feeding_use - a description of the observed feeding sign on the carcass
38. marrow - the color and texture of the bone marrow from long bones
39. drag_trail - whether a drag trail was located
40. length - the length of the drag trail
41. kill_site - whether a kill site was located
42. kill_lat - the latitude of the kill site
43. kill_lon - the longitude of the kill site
44. evidence_kill_site - the evidence used to identify the kill site
45. consistent_date - whether the condition of the carcass is consistent with the cluster formation date
46. confidence_cougar_kill - confidence level in whether a cougar killed or scavened the prey. positive, probable, unknown, or scavenge
47. other_sign - sign of other species that was observed at the carcass
48. scavenged - whether or not the carcass was scavenged by other species. Y, N, or Likely
49. scavened_by - what other species scavenged the carcass
50. kill_2 - wether there was a second kill at the cluster
51. species - the species of the second prey item
52. cached - whether the second prey item was cached
53. notes - any notes about the cluster investigation
54. cache_photo - whether photos were taken of the cache
55. carcass_photo - whetehr photos were taken of the carcass
56. bite_photo - whether photos were taken of the bite wounds
57. hem_photo - whether photos were taken of the hemorrhaging
58. drag_photo - whether photos were taken of the drag trail
59. kill_photo - whether photos were taken of the kill site
60. coug_sign_photo - whether photos were taken of cougar sign
61. scav_sign_photo - whether photos were taken of scavenger sign
62. time_leave - the time of day we left the GPS cluster

#### Isotopes
##### UNM_isotopes2.csv

This csv file contains the raw isotope data for deer and pig hair samples. Each row represents an individual hair sample. There are 13 columns.

1. Sample ID - the ID of the hair sample
2. Species - the species of the sample. pig = SUSC and deer = ODHE
3. Tray # - the tray number the sample was in for analysis
4. TrayID - the samples location within the tray
5. Weight (mg) - the mass of the sample in mg
6. d15N - the nitrogen isotope value in parts per mille
7. d13C - the carbon isotope value in parts per mille
8. %N (wt.) - the nitrogen percent mass of the sample
9. %C (wt.) - the carbon percent mass of the sample
10. C:N - the carbon to nitrogen ratio
11. ClusterID - the location each sample came from. Samples that begin with F/M were collected from live captured pigs. All others were collected from cougar-killed prey
12. Season - the season each sample was collected
13. Year - the year each sample was collected

### CODE FILES

1. Extracting camera covariates. Also used later to make layers of predicted deer and pig density
2. Run the random encounter staying time model for deer and pigs. Seperate file for each species
3. Analyze deer body condition and fawn to doe ratios to evaluate if there is evidence of competition with pigs
4. Analyze the isotope data

##### 01_CameraCovariates.R 

This file uses the camera station data and GIS data to extract covariates for each camera location
It creates and saves a new file (CameraCovariates_NDVI_289_log.csv) that is needed by the random encounter staying time model
Then, starting on Line 174, this file creates rasters of predicted deer and pig density needed for the deer body condition models
The second half of this script cannot be run until the REST model has been run and output saved.

##### 02_RESTAnalysis_NoVS_Randomeffect_deer_20240405_logunscale.R
##### 03_RESTAnalysis_NoVS_Randomeffect_pig_20240405_logunscale.R

These files use the camera station data, camera covariate data (created from previous script), and deer and pig visit data.
It processes data and runs the random encounter staying time model for the species.
It then runs a cross validation of the model to test model performance
Note that these models can take several hours to run depending on the number of iterations, chains, and computer specs
The beginning of this code is also used to estiamte deer and pig temporal overlap

##### 04_Body Condition.R
This file test is deer fawn to doe ratios or body condition are related to pig denisty
It uses the deer body condition data from cameras, GIS data, the camera station data, and kill site data (not necessary for the manuscript)
It processes the data and incorporates covariates from the GIS data
It then runs generalized linear models to test if deer fawn to doe ratios or body condition are related to landscape covariates, seasonal covarites, or estimated pig density 


##### 05_Isotopes_new.R
This file estimates the isotopic niche overlp of deer and pigs
It uses the raw isotope data, creates visualizations of the overlap, and estimates overlap using Bayesian standard ellipses for each species