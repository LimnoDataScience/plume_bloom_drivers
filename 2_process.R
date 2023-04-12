
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
  )
  
)
