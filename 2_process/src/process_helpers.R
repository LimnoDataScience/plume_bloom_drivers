
stacked_netcdf_to_raster_list <- function(nc_file) {
  nc <- nc_open(nc_file)
  nc_atts <- attributes(nc$var)$names # List dates
  
  # For each date, extract the values and save as a list of rasters
  out_raster_list <- purrr::map(nc_atts, function(date) {
    var_atts <- ncatt_get(nc, date)
    list(
      mission = var_atts$mission,
      date = var_atts$date,
      raster_vals = raster(ncvar_get(nc, date))
    )
  })
  
  nc_close(nc)
  return(out_raster_list)
}

summarize_raster_class_counts <- function(raster_list) {
  
  # Extract the raster object from the list
  raster_vals <- raster_list$raster_vals
  
  # Add a class of 5, which will be the NAs
  raster_vals[is.na(raster_vals)]<-5
  
  # Count the number of pixels in each class
  raster::freq(raster_vals) %>% 
    as_tibble() %>% 
    mutate(mission = raster_list$mission,
           date = raster_list$date) %>% 
    select(mission, date, class = value, count)
}

# Convert the `prism` plot objects into a single data frame, including the HUC info
get_prism_data_at_huc_centers <- function(huc_latlong_table, prism_var, prism_dates, prism_dir) {
  
  # Extracting data using the prism package, gives you a plot for some reason...
  prism_plot_info <- extract_prism_plot_at_location(
    lat = huc_latlong_table$latitude,
    lon = huc_latlong_table$longitude,
    prism_var, prism_dates, prism_dir)
  
  # Pull out the data itself and add the HUC info back in.
  prism_data <- prism_plot_info$data %>% 
    left_join(huc_latlong_table, by = c('latitude', 'longitude'))
  
  return(prism_data)
}

# For a given lat/long, use `prism` fxns to extract timeseries. Note
# that the `prism` pkg functions return a plot object and data must be
# extracted from that separately.
extract_prism_plot_at_location <- function(lat, lon, prism_var, prism_dates, prism_dir) {
  
  # Set the prism archive location
  prism_set_dl_dir(prism_dir)
  
  # Use prism helper fxns to list relevant files for the variable
  to_slice <- prism_archive_subset(prism_var, "daily", dates = prism_dates)
  
  # Convert sf point to a vector, `c(longitude, latitude)`
  lon_lat_vector <- c(lon, lat)
  # NAs result unless I round to only 1 decimal
  # round(digits=1)
  
  # User prism helper function to slice the files into a plot obj
  var_pd_plot <- pd_plot_slice(to_slice, lon_lat_vector)
  
  # Add a column to the time series data 
  var_pd_plot$data <- var_pd_plot$data %>% 
    mutate(var = prism_var,
           longitude = lon,
           latitude = lat) %>% 
    select(longitude, latitude, variable = var, date, value = data)
  
  return(var_pd_plot)
}

# Summarize daily meteo values per grid cell into a daily value per huc
summarize_meteo_data_by_huc <- function(meteo_data) {
  meteo_data %>%
    split(.$variable) %>% 
    purrr::map(~{
      cur_var <- unique(.x$variable)
      if(cur_var == 'tmean') {
        # If the variable is `tmean`, just take the mean of all the cells per huc
        .x %>% 
          filter(variable == cur_var) %>% 
          group_by(huc, variable, date) %>% 
          summarize(value_huc = mean(value, na.rm=TRUE), .groups="keep") %>% 
          ungroup() 
      } else if(cur_var == 'ppt') {
        # If the variable is `ppt`, then we need to sum the precipitation values
        # of each cell based on the fraction of the cell that is in the HUC
        .x %>% 
          filter(variable == cur_var) %>% 
          mutate(value_adj = value*huc_frac) %>% 
          group_by(huc, variable, date) %>% 
          summarize(value_huc = sum(value_adj, na.rm=TRUE), .groups="keep") %>% 
          ungroup() 
      } else {
        stop(sprintf('`summarize_meteo_data_by_huc()` needs to be updated to include `%s`', cur_var))
      }
    }) %>% bind_rows() %>% 
    # Add some helpful columns for dates
    mutate(year = year(date),
           # Round *down* to decade (e.g. 1989 = 1980s, 1992 = 1990s)
           decade = sprintf('%ss', year - (year %% 10)),
           month = month(date)) %>% 
    mutate(season = month_to_season(month)) %>% 
    relocate(date, value_huc, .after = season)
}

# Pass in a vector of month numbers (1 thru 12) and get a 
# vector of the same size with the season back.
month_to_season <- function(month_num) {
  # Create vector with season as the value and month number as the name
  month_season <- setNames(rep(c("Winter", "Spring", "Summer", "Fall"), each = 3), c(12, 1:11))
  
  # Arrange so that DEC is in the 12th spot
  month_season <- month_season[order(as.numeric(names(month_season)))] 
    
  # Pull out the season based on month number as the indices
  season_out <- month_season[month_num]
  names(season_out) <- NULL # Drop the names attribute
  
  return(season_out)
}
