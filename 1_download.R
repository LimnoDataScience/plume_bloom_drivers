
source('1_download/src/download_helpers.R')

p1_download <- list(
  
  ##### Handle authentication with Google Drive #####
  
  # Authenticate! Note that existing auth won't matter 
  # because targets builds in a new session every time.
  tar_target(p1_gd_config_yml, 'gd_config.yml', format='file'),
  tar_target(p1_gd_config, yaml::yaml.load_file(p1_gd_config_yml)),
  tar_target(p1_authenticated_user, gd_auth(p1_gd_config$gd_email),
             # Re-authenticate to be certain the user is still authenticated.
             cue = tar_cue_age(p1_authenticated_user, 
                               as.difftime(3, units = "hours"))),
  
  ##### Download the files from Google Drive #####
  
  # List the files available in this specified folder
  tar_target(p1_gd_id_netcdfs, as_id('1g3spZxtTP2tq7TzHaCZqK7Nn1HXB9rKq')),
  tar_target(p1_gd_netcdfs, {
    # Add a dependency on p1_authenticated_user target so that this 
    # builds AFTER the target for authenticated to GH has been run.
    message(sprintf('Attempting to list files using permissions for %s', 
                    p1_authenticated_user$emailAddress))
    drive_ls(p1_gd_id_netcdfs)
  }),
  
  # Download the raster stacks as netcdf files
  tar_target(p1_netcdfs, {
    # Add a dependency on p1_authenticated_user target so that this 
    # builds AFTER the target for authenticated to GH has been run.
    p1_authenticated_user
    
    files_saved_info <- drive_download(
      p1_gd_netcdfs$id, 
      path = sprintf('1_download/out/%s', p1_gd_netcdfs$name),
      overwrite = TRUE)
    return(files_saved_info$local_path)
  }, format = 'file',
  pattern = map(p1_gd_netcdfs)),
  
  # Download the observed blooms dataset
  tar_target(p1_obs_blooms_xlsx, {
    # Add a dependency on p1_authenticated_user target so that this 
    # builds AFTER the target for authenticated to GH has been run.
    p1_authenticated_user
    
    files_saved_info <- drive_download(
      as_id('1JPheDfzusaOWRS4Dew9KTnCRAqJSQykV'), 
      path = '1_download/out/lake_sup_bloom_history.xlsx',
      overwrite = TRUE)
    return(files_saved_info$local_path)
  }, format = 'file'),
  
  ##### Load spatial data for Lake Superior watershed & AOI #####
  
  tar_target(p1_lake_superior_box_sf, {
    # Pulled the bounding box for our Lake Superior AOI:
    # https://github.com/rossyndicate/Superior-Plume-Bloom/blob/efa1bdc644611ee97c2e1e0c3bf0cfc4a7ca1955/eePlumB/A_PrepAOI/TileAOI.Rmd#L31-L52
    sup_box <- tibble(ymin = 46.5, ymax = 47.3,  xmin = -92.2,xmax = -90.1)
    
    tibble(
      lat = c(sup_box$ymin, sup_box$ymax, sup_box$ymax, sup_box$ymin, sup_box$ymin),
      lon = c(sup_box$xmin, sup_box$xmin, sup_box$xmax, sup_box$xmin, sup_box$xmax)) %>% 
      st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
      st_bbox() %>% st_as_sfc()
  }),
  tar_target(p1_lake_superior_grid_sf, 
    # Now make the grid using that box. To do this, I borrowed code from:
    # https://github.com/rossyndicate/Superior-Plume-Bloom/blob/efa1bdc644611ee97c2e1e0c3bf0cfc4a7ca1955/eePlumB/A_PrepAOI/TileAOI.Rmd#L31-L52
    st_make_grid(p1_lake_superior_box_sf, 
                 cellsize = c(0.55, 0.3)) # units are degrees
  ),
    
  tar_target(p1_lake_superior_grid_centers, 
    # Get the center of each cell and then convert to a table
    p1_lake_superior_grid_sf %>% 
      st_centroid() %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      setNames(c('longitude', 'latitude')) %>% 
      mutate(cell_no = row_number())),

  tar_target(p1_lake_superior_watershed_shp, '1_download/in/LakeSuperiorWatershed.shp', format="file"),
  tar_target(p1_lake_superior_watershed_sf, st_read(p1_lake_superior_watershed_shp)),
  
  ##### Download the HUCs per site outlet #####
  
  # Manual table for which sites to include and their names
  tar_target(p1_nwis_sites, 
             tibble(river = c('Nemadji', 'Bois Brule', 'Siskiwit', 'St. Louis'),
                    nwis_site = c('04024454', '04026005', '04026160', '04024000'))),
  
  # Find lat/long per site and then download associated HUC8. Note that we want 
  # HUC10s, but `nhdplusTools` won't allow you to get HUC10s from site ids alone.
  tar_target(p1_nwis_sites_sf, 
             dataRetrieval::readNWISsite(p1_nwis_sites$nwis_site) %>% 
               st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs=4326)),
  tar_target(p1_huc08_nwis_sites, 
             get_huc(id = unique(p1_nwis_sites_sf$huc_cd), type='huc08')),
  
  # Use the HUC8 shape to pull the appropriate HUC10s, then filter to just those
  # that contain the NWIS site point. 
  tar_target(p1_huc10_nwis_sites, 
             p1_huc08_nwis_sites %>% 
               split(.$id) %>% 
               purrr::map(~get_huc(AOI = .x, type='huc10') %>% 
               # TODO: Not sure about routing at this time. It could be that 
               # some feed into the next one and more should be included.
               st_filter(p1_nwis_sites_sf, .predicate = st_contains)) %>% 
               bind_rows() %>% 
               distinct()),
  
  ##### Download the PRISM meteo data #####
  
  tar_target(p1_prism_dir, '1_download/prism_data'),
  tar_target(p1_prism_vars, c('tmean', 'ppt')),
  tar_target(p1_prism_dates, seq(from = as.Date("1981-01-01"), 
                                 to = as.Date("2022-09-30"), by = "days")),
  
  # Group the dates so that we can query individually and
  # therefore rebuild only dates that don't work, but not
  # store thousands of dynamic branches
  tar_group_count(p1_prism_download_batches, 
                  tibble(date = p1_prism_dates),
                  count = 20),
  
  tar_target(p1_prism_files, {
    # Set the directory where the prism files will go
    prism_set_dl_dir(p1_prism_dir)
    
    # Download each date for the current variable from PRISM
    get_prism_dailys(
      type = p1_prism_vars,
      dates = p1_prism_download_batches$date,
      keepZip=FALSE
    )
    
    # In order to track files and changes, list the files saved in
    # the folder as the output here. This works since each subfolder
    # is named with the variable and date so adding dates or vars
    # will result in changes here. 
    var_files <- list.files(p1_prism_dir, pattern = p1_prism_vars)
    return(tibble(prism_var = p1_prism_vars,
                  prism_files = var_files))
  }, 
  pattern = cross(p1_prism_vars, p1_prism_download_batches),
  # Sometimes there is a temporary timeout when pulling a date. Retrying 
  # has usually fixed it. To handle this more automatically, use `error="null"`
  # so that this target will move on and build all branches BUT will
  # not considered "complete" and thus will try to rebuild the branch that
  # errored the next time the pipeline is built.
  error = "null")
  
  # If you download the zip of all the pre-downloaded prism data, uncomment
  # this target and comment out the one above instead. Make sure you 
  # unzip the files and place them in `1_download/prism_data/`
  # tar_target(p1_prism_files, 
  #            list.files('1_download/prism_data'))
  
)
