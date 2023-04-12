
library(targets)
library(tarchetypes)

# Stop annoying messaging when it downloads each file
options(googledrive_quiet = TRUE) 

tar_option_set(packages = c(
  'googledrive',
  'yaml'
), format='qs')

source('1_download.R')

# All outputs
c(p1_download)
