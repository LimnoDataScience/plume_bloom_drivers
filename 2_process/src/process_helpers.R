
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
