
library(targets)
library(tarchetypes)

# Stop annoying messaging when it downloads each file
options(googledrive_quiet = TRUE) 

tar_option_set(packages = c(
  'googledrive',
  'ncdf4',
  'prism',
  'raster',
  'tidyverse',
  'yaml'
), format='qs')

source('1_download.R')
source('2_process.R')
# source('3_model.R') # Leaving 3 as a placeholder for future work
source('4_visualize.R')

# Define what each value in the rasters mean
bloom_plume_class_xwalk <- tibble::tibble(
  val = 0:4,
  nm = c(
    "out of area/masked",
    "openWater",
    "lightNearShoreSediment",
    "offShoreSediment",
    "darkNearShoreSediment"
  ),
  color = c("white", "#9fc5e8", "#edbd95", "#db7b2b", "#41240c")
)


# All outputs
c(p1_download, p2_process,
  p4_visualize)
