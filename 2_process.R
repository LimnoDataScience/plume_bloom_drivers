
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
  
  ##### Process classified tifs from HydroShare #####
  
  # Convert classified rasters to binary - 0 for not a sediment class, 1 for sediment class
  tar_target(p2_sedpresence_terraqs, {
    classified_raster_to_sed_presence(
      in_file = p1_hs_sedclass_tif_info$tif_fn,
      out_file = sprintf('2_process/tmp/%s', gsub('.tif', '.qs', basename(p1_hs_sedclass_tif_info$tif_fn))))
  },
    pattern = map(p1_hs_sedclass_tif_info), 
    format = 'file'),
  
  # PER MISSION-GROUP, sum each of the raster files cell values of binary 
  # sediment presence to create a heatmap. Note the full mission files were
  # two big to do this, so need to batch and then recombine at the end.
  
  # To take advantage of tarchetypes batching, had to first separate the 
  # Sentinel/Landsat targets. Since there are only two, not a big deal to manual split :)
  tar_target(p2_terraqs_grp, tibble(terraqs_fn = p2_sedpresence_terraqs) %>%
               mutate(terraqs_fn_hash = tools::md5sum(terraqs_fn), # Use hash to rebuild downstream targets if the files change
                      mission = ifelse(grepl('Sentinel', terraqs_fn),
                                       yes = 'Sentinel', no = 'Landsat'))),
  tarchetypes::tar_group_count(p2_terraqs_grp_ct_sentinel, 
                               filter(p2_terraqs_grp, mission == "Sentinel"), 
                               count = 50),
  tarchetypes::tar_group_count(p2_terraqs_grp_ct_landsat, 
                               filter(p2_terraqs_grp, mission == "Landsat"), 
                               count = 50),
  
  # Now map over Sentinel batches
  tar_target(p2_sediment_heatmap_sentinel_batch_terraqs, 
             sum_sed_presence(p2_terraqs_grp_ct_sentinel$terraqs_fn,
                              sprintf('2_process/tmp/sediment_heatmap_%s_grp%02d.qs', 
                                      unique(p2_terraqs_grp_ct_sentinel$mission),
                                      unique(p2_terraqs_grp_ct_sentinel$tar_group))),
             pattern = map(p2_terraqs_grp_ct_sentinel),
             format='file'),
  # Repeat the same command but combine the group heatmaps into a single Sentinel one
  tar_target(p2_sediment_heatmap_sentinel_terraqs,
             sum_sed_presence(p2_sediment_heatmap_sentinel_batch_terraqs,
                              unique(gsub('tmp', 'out', gsub(
                                '_grp([0-9]+)', '', 
                                p2_sediment_heatmap_sentinel_batch_terraqs)))),
             format='file'),
  
  # Now map over Landsat batches
  tar_target(p2_sediment_heatmap_landsat_batch_terraqs, 
             sum_sed_presence(p2_terraqs_grp_ct_landsat$terraqs_fn,
                              sprintf('2_process/tmp/sediment_heatmap_%s_grp%02d.qs', 
                                      unique(p2_terraqs_grp_ct_landsat$mission),
                                      unique(p2_terraqs_grp_ct_landsat$tar_group))),
             pattern = map(p2_terraqs_grp_ct_landsat),
             format='file'),
  # Repeat the same command but combine the group heatmaps into a single Landsat one
  tar_target(p2_sediment_heatmap_landsat_terraqs,
             sum_sed_presence(p2_sediment_heatmap_landsat_batch_terraqs,
                              unique(gsub('tmp', 'out', gsub(
                                '_grp([0-9]+)', '', 
                                p2_sediment_heatmap_landsat_batch_terraqs)))),
             format='file'),
  
  ##### Load and process observed blooms spreadsheet #####
  
  tar_target(p2_obs_blooms_details, clean_bloom_history(p1_obs_blooms_xlsx)),
  tar_target(p2_obs_blooms_sf, {
    p2_obs_blooms_details %>% 
      select(Year, `Start Date`, `End Date`, Latitude, Longitude, Verified_cyanos) %>% 
      st_as_sf(coords = c('Longitude', 'Latitude'), crs=4326) %>% 
      # Remove observations outside of our AOI
      st_crop(p1_lake_superior_box_sf) %>% 
      # Transform to match other sf object projections
      st_transform(crs = st_crs(p1_lake_superior_watershed_sf))
  }),
  
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
