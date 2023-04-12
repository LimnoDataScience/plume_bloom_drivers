
stacked_netcdf_to_raster_list <- function(nc_file) {
  nc <- nc_open(nc_file)
  nc_atts <- attributes(nc$var)$names # List dates
  
  # For each date, extract the values and save as a list of rasters
  out_raster_list <- purrr::map(nc_atts, function(date) {
    var_atts <- ncatt_get(nc, date)
    list_name <- sprintf('%s_%s', var_atts$mission, var_atts$date)
    list(
      raster_vals = raster(ncvar_get(nc, date))
    ) %>% setNames(nm = list_name)
  }) %>% unlist()
  
  nc_close(nc)
  return(out_raster_list)
}
