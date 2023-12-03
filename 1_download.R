
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
  
  # Download the observed blooms dataset
  tar_target(p1_gd_id_obs_blooms, as_id('1JPheDfzusaOWRS4Dew9KTnCRAqJSQykV')),
  tar_target(p1_obs_blooms_gd_hash, drive_get(p1_gd_id_obs_blooms) %>% 
               pluck('drive_resource', 1, 'md5Checksum'),
             # Always ping GD and get the hash of this file in case it changes
             cue = tar_cue('always')),
  tar_target(p1_obs_blooms_xlsx, {
    # Add a dependency on p1_authenticated_user target so that this 
    # builds AFTER the target for authenticated to GD has been run.
    p1_authenticated_user
    
    # Depend on the file hash so that this rebuilds if the GD file changes
    p1_obs_blooms_gd_hash
    
    files_saved_info <- drive_download(
      p1_gd_id_obs_blooms, 
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
                    nwis_site = c('04024430', '04025500', '04026160', '04024000'))),
  
  # Also create a manual data frame of bbox corners into the lake for each river outlet
  # Note that the St. Louis outlet bbox is the same as the Nemadji
  tar_target(p1_river_outlet_bbox_tbl, 
             tibble(river = c('Nemadji', 'Bois Brule', 'Siskiwit', 'St. Louis'),
                    xmax = c(-91.892330, -91.570010, -91.082328, -91.892330),
                    xmin = c(-92.090796, -91.690019, -91.208000, -92.090796),
                    ymax = c(46.764212, 46.846760, 46.916154, 46.764212),
                    ymin = c(46.670194, 46.729790, 46.839908, 46.670194))),
  
  # Find lat/long per site and then download associated HUC8. Note that we want 
  # HUC10s, but `nhdplusTools` won't allow you to get HUC10s from site ids alone.
  tar_target(p1_nwis_sites_sf, 
             dataRetrieval::readNWISsite(p1_nwis_sites$nwis_site) %>% 
               st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs=4326)),
  
  # Use the NWIS sites to find the matching HUC10s
  tar_target(p1_huc10_nwis_sites, 
             p1_nwis_sites_sf %>% 
               split(.$site_no) %>% 
               # TODO: Not sure about routing at this time. We may want to include
               # more of the HUC10s that route into this one.
               purrr::map(~get_huc(AOI = .x, type='huc10') %>% 
                            select(huc10, name, areasqkm)) %>% 
               bind_rows(.id = "nwis_site")),
  
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
  error = "null"),
  
  # If you download the zip of all the pre-downloaded prism data, uncomment
  # this target and comment out the one above instead. Make sure you 
  # unzip the files and place them in `1_download/prism_data/`
  # tar_target(p1_prism_files, 
  #            list.files('1_download/prism_data'))
  
  ##### Download NWIS discharge data #####
  
  tar_target(p1_nwis_Q, 
             readNWISdv(siteNumber = p1_nwis_sites$nwis_site, 
                        startDate = min(p1_prism_dates),
                        endDate = max(p1_prism_dates),
                        parameterCd = '00060') %>% 
               renameNWISColumns() %>% 
               select(nwis_site = site_no, date = Date, Q = Flow)),
  
  ##### Download rasters of classified sediment data from HydroShare #####
  
  # For now, manually downloaded zips and placed in the `1_download/in` folder
  # https://www.hydroshare.org/resource/17cd38e9ac7845c29b0f45dab15e7073/
  # Might be able to switch to using HSClientR in the future once the item
  # is not private but auth doesn't work right now and is "forbidden"
  # HSClientR::hs_access('17cd38e9ac7845c29b0f45dab15e7073')
  tar_target(p1_hs_sedclass_tifzips_dir, '1_download/in/tifzips', format='file'),
  tar_target(p1_hs_sedclass_tifzips, list.files(p1_hs_sedclass_tifzips_dir, full.names = TRUE)),
  # Next target only here to map over previous target in order for branching in `p1_hs_sedclass_tif` to take place
  tar_target(p1_hs_sedclass_tif_zip, p1_hs_sedclass_tifzips, pattern=map(p1_hs_sedclass_tifzips), format='file'),
  tar_target(p1_hs_sedclass_tifs, 
             unzip_tifs(p1_hs_sedclass_tif_zip, '1_download/out/sediment_tifs'), 
             pattern = map(p1_hs_sedclass_tif_zip),
             format = 'file'),
  # Could not get this to branch over the `format='file'` without making this a pattern and 
  # I want to collapse the list, so adding a hash column instead.
  tar_target(p1_hs_sedclass_tif_info, tibble(tif_fn = unname(p1_hs_sedclass_tifs)) %>% 
               mutate(tif_fn_hash = tools::md5sum(tif_fn)))
)
