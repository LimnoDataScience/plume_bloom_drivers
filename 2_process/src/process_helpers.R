
# Per file, convert class values 0 thru 4 to sediment 
# binary value (sediment classes are 2 thru 4 and would become 1)
classified_raster_to_sed_presence <- function(in_file, out_file) {
  
  # Load the tif file
  rast_by_class <- terra::rast(in_file)
  
  # if(nrow(terra::freq(rast_by_class)) > 1) browser()
  
  # Convert from classes 0:4 to sediment/not sediment binary
  rast_sed_bin <- subst(subst(rast_by_class, from=1, to=0), from=2:4, to=1)
  
  save_terraqs(rast_sed_bin, out_file)
  return(out_file)
}

sum_sed_presence <- function(in_files, out_file) {
 
  # Sum the values across all rasters, keep overwriting the
  # same object to avoid too much in memory at once.
  rast_sum <- load_terraqs(in_files[1]) # Had to start with a populated raster
  for(fn in tail(in_files, -1)) {
    raster_now <- load_terraqs(fn)
    rast_sum <- sum(c(rast_sum, raster_now))
  }
  
  save_terraqs(rast_sum, out_file)
  return(out_file)
}

# Read in and process the manually generated bloom observation spreadsheet from Kait Reinl
clean_bloom_history <- function(file_in) {
  
  # Initially load the excel spreadsheet in order to get columns to use after 
  # skipping first two data rows (row 1 = header above column names, skip it)
  obs_blooms_raw <- read_excel(file_in, skip = 1, sheet = 'BloomHistory')
  
  # Read without the 'circa' observations from 1968 & 1995 (random header + 
  # col names row + the two old observations = 4 to skip)
  obs_blooms_clean <- read_excel(file_in, 
                           skip = 4, sheet = 'BloomHistory',
                           col_names = names(obs_blooms_raw),
                           na = "n/a") %>% 
    # Remove spaces and headers between the secondary tables 
    filter(!is.na(Year),
           !Year %in% c("Inland lake reports", 
                        "Benthic bloom reports",
                        "Green water (no surface scum)")) %>% 
    # Rather than reformat the Year column, just delete and
    # add back using the `Start Date` column
    select(-Year) %>% 
    mutate(Year = year(`Start Date`), .before = `Start Date`) %>% 
    # Change the dates to `Date` class rather than `POSIXct`
    mutate(`Start Date` = as.Date(`Start Date`),
           `End Date` = as.Date(`End Date`)) %>% 
    # Remove any data that doesn't have a location listed
    filter(!is.na(Longitude), !is.na(Latitude)) %>% 
    # Harmonize the 'verified' column - just leave T/F for whether it was verified or not (NA = FALSE)
    mutate(Verified_cyanos = grepl('^(v|V)erified', `Verified cyanos with microscope?`), 
           .after = `Water Body Name`)
  
  return(obs_blooms_clean)
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

# There is a specific way you need to load/save terra
# raster objects in order to share between targets.
# See more at https://github.com/rspatial/terra/issues/987
save_terraqs <- function(terra_obj, qs_file) qs::qsave(terra::wrap(terra_obj), qs_file)
load_terraqs <- function(qs_file) terra::unwrap(qs::qread(qs_file))
# Use the above to load any targets with the suffix `_terraqs`
