
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
