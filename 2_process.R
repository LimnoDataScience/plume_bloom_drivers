
source('2_process/src/process_helpers.R')

p2_process <- list(
  
  ##### Process NetCDF rasters #####
  
  # TODO: this will not be a sustainable way to store
  # processed data when we move beyond a handful of years
  
  # For each annual file, create a list with the date, mission, 
  # actual raster values layer.
  tar_target(p2_raster_list_nested, 
             stacked_netcdf_to_raster_list(p1_netcdfs),
             pattern = map(p1_netcdfs),
             iteration = 'list'),
  
  # Get all dates + mission lists at the same unnested level
  tar_target(p2_raster_list, 
             reduce(p2_raster_list_nested, c), 
             iteration='list'),
  
  # Calculate the frequency for each class category within the raster
  # by date and mission.
  tar_target(p2_daily_summaries, 
             summarize_raster_class_counts(p2_raster_list),
             pattern = map(p2_raster_list)),
  tar_target(p2_daily_summaries_clean, 
             p2_daily_summaries %>% 
               mutate(date = as.Date(date),
                      year = format(date, '%Y')) %>% 
               filter(class != 0, class != 5)
             # TODO: show class counts as % pixels to handle
             # Landsat 7 striping issue.
  ),
  
  ##### Process GEE mission-dates to prepare for PRISM query #####
  
  tar_target(p2_mission_dates_aprnov, 
             read_csv(p1_gd_missiondates_csv) %>% 
               # Keep only dates between April and November, as B does here:
               # https://github.com/rossyndicate/Superior-Plume-Bloom/blob/main/eePlumB/B_process_LS_mission-date/2_processMissionDateList.Rmd#L48-L55
               mutate(month = lubridate::month(DATE_ACQUIRED)) %>% 
               filter(month >=4, month <= 11) %>% 
               pull(DATE_ACQUIRED) %>% 
               # Only need the unique dates, not duplicates per mission
               unique() %>% 
               # Sort is needed because B randomized the mission-dates for eePlumb workflow
               sort()),
  
  ##### Read PRISM files and load into tibbles #####
  
  # TODO: some grid cells return NAs because the centroid is over 
  # the water (and I assume there is some sort of water masking?)
  # Also, need to see grid cell size compared to PRISM resolution
  # because some seem like they are duplicates.
  tar_target(p2_lake_superior_watershed_filt, 
             p1_lake_superior_watershed_sf %>% 
               # Transform to 4326 (should match PRISM that way)
               st_transform(crs=st_crs(p1_lake_superior_sf)) %>% 
               st_make_valid() %>% 
               # Filter to only subwatersheds within 5 miles of the AOI bbox
               st_filter(p1_lake_superior_sf, .predicate = st_is_within_distance, 
                         dist = 1609*5)),
  
  # For a given lat/long, use `prism` fxns to extract timeseries
  tar_target(p2_prism_plots, {
    # Make this target dependent on the prism files so that it will
    # if they change.
    p1_prism_files
    extract_prism_at_location(
      lat = p1_lake_superior_grid_centers$latitude,
      lon = p1_lake_superior_grid_centers$longitude,
      prism_var = p1_prism_vars,
      prism_dates = p1_prism_download_batches$date,
      prism_dir = p1_prism_dir)
  }, 
  pattern = cross(p1_lake_superior_grid_centers, p1_prism_vars,
                  p1_prism_download_batches),
  iteration = "list"),
  
  # Convert the `prism` plot objects into a single data frame
  # with all PRISM vars
  tar_target(p2_prism_data, p2_prism_plots$data, 
             pattern = map(p2_prism_plots))
  
)
