
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
  tar_target(p2_lake_superior_watershed_filt, {
    # Transform Lake Superior grid shape before using in filter
    p1_lake_superior_box_sf_transf <- p1_lake_superior_box_sf %>% 
      st_transform(crs = st_crs(p1_lake_superior_watershed_sf))
    
    # Filter to only subwatersheds within 5 miles of the AOI bbox
    p1_lake_superior_watershed_sf %>%
      st_filter(p1_lake_superior_box_sf_transf, 
                .predicate = st_is_within_distance, 
                dist = 1609*5)
  }),
  
  # Collapse subwatersheds into a single shape
  tar_target(p2_lake_superior_watershed_dissolved, 
             p2_lake_superior_watershed_filt %>% 
               st_union() %>% st_as_sf()),
  
  # Create a grid of 10km cells across the AOI watersheds (PRISM data come in 
  # 4 km but that resolution might be too fine to process for now)
  tar_target(p2_lake_superior_watershed_grid_all, 
             p2_lake_superior_watershed_dissolved %>% 
               # Cellsize is in meters because of the projection we are in
               st_make_grid(cellsize=10000) %>% 
               st_as_sf()),
  
  # Subset to grid cells that intersect the HUC watersheds and calculate
  # the fraction of cell that overlaps the watershed polygon. That fraction
  # will be used to calculate contributing precip later.
  tar_target(p2_lake_superior_watershed_grid_sf, {
    huc10_sf_transf <- st_transform(p1_huc10_nwis_sites, crs=st_crs(p1_lake_superior_watershed_sf))
    grids_over_hucs <- p2_lake_superior_watershed_grid_all %>% 
      st_filter(huc10_sf_transf, .predicate = st_intersects)
    # For each HUC, identify the cells that intersect with them
    # and create grid cell sf with the fractions and HUC as a column.
    grids_over_hucs_info <- huc10_sf_transf %>% 
      split(.$huc10) %>% 
      purrr::map(~{
        grid_cells <- grids_over_hucs %>% st_filter(.x, .predicate = st_intersects)
        grid_cells_adjs <- st_intersection(grid_cells, .x)
        grid_cell_frac <- units::drop_units(st_area(grid_cells_adjs)/st_area(grid_cells))
        grid_cells %>% mutate(huc = .x$huc10, huc_frac = grid_cell_frac)
      }) %>% bind_rows() %>% rename(geometry = x)
    return(grids_over_hucs_info)
  }),
  
  # Convert cell polygons to cell centroids then filter to keep only those 
  # with centroids that intersect the watershed shape
  tar_target(p2_lake_superior_watershed_grid_centers_sf,
             # Get the center of each cell and filter
             p2_lake_superior_watershed_grid_sf %>% 
               st_centroid()),
  
  # Convert cell centroids to CRS=4326 so that we can extract the matching 
  # PRISM data for each cell.
  tar_target(p2_lake_superior_watershed_grid_centers_tbl, 
             p2_lake_superior_watershed_grid_centers_sf %>% 
               st_transform(crs=4326) %>% 
               st_coordinates() %>% 
               as_tibble() %>% 
               setNames(c('longitude', 'latitude')) %>% 
               mutate(cell_no = row_number()) %>% 
               # Add corresponding huc and huc_frac columns
               bind_cols(st_drop_geometry(p2_lake_superior_watershed_grid_centers_sf))),
  
  
  # Get the prism data from the files for each of the lat/longs, variables, and 
  # dates. Note that this is a lengthy step because of using the `cross` pattern.
  # Over 3 hrs with 20 works, 2 variables, 20 date batches, and 37 grid cells 
  tar_target(p2_prism_data, {
    # Make this target dependent on the prism files so that
    # it will build if they change.
    p1_prism_files
    get_prism_data_at_huc_centers(
      huc_latlong_table = p2_lake_superior_watershed_grid_centers_tbl,
      prism_var = p1_prism_vars,
      prism_dates = p1_prism_download_batches$date,
      prism_dir = p1_prism_dir)
  }, 
  pattern = cross(p2_lake_superior_watershed_grid_centers_tbl, p1_prism_vars,
                  p1_prism_download_batches),
  # Define this target as one that can be done in parallel when `tar_make_clustermq(workers = X)` 
  # is called. The default for other targets in this pipeline is NOT to parallelize.
  # Note that on Lindsay's computer, 20 workers seems to be the best option.
  deployment = "worker", 
  # Management parallel computing worker data storage to speed up performance
  storage = "worker", retrieval = "worker"),
  
  # Summarize the data per HUC
  tar_target(p2_prism_data_huc, summarize_meteo_data_by_huc(p2_prism_data))
  
  # If you downloaded the CSV file of all pre-processed PRISM data, uncomment
  # this target and comment out the one above instead. Make sure you already
  # placed the file in `2_process/in/` before building the pipeline.
  # tar_target(p2_prism_data_huc, read_csv('2_process/in/prism_data_huc.csv'))
  
)
