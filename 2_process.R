
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
  tar_target(p2_prism_dates, {
    # Create vector of dates for which to download PRISM data. Only want 
    # to download the mission dates and 2 preceding weeks
    purrr::map(p2_mission_dates_aprnov, function(date) {
      seq(from = date - 14, to = date, by = "days")
    }) %>% reduce(c) %>% unique()
  })
  
)
