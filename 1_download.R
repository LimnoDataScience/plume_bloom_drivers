
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
  
  ##### Download the GEE imagery mission-dates from Google Drive #####
  
  # List the files available in this specified folder
  tar_target(p1_gd_id_missiondates, as_id('1UEEVBlvX7P4H2dtNoX1oj44-Xeyg6x01')),
  tar_target(p1_gd_missiondates_csv, {
    # Add a dependency on p1_authenticated_user target so that this 
    # builds AFTER the target for authenticated to GH has been run.
    message(sprintf('Attempting to download a file using permissions for %s', 
                    p1_authenticated_user$emailAddress))
    gd_file_info <- drive_get(p1_gd_id_missiondates)
    local_file_info <- drive_download(
      p1_gd_id_missiondates,
      path = sprintf('1_download/out/%s', gd_file_info$name),
      overwrite=TRUE)
    return(local_file_info$local_path)
  }, format = "file"),

  ##### Download the PRISM meteo data #####
  
  tar_target(p1_prism_vars, c('tmean', 'ppt')),
  
  tar_target(p1_prism_files, {
    # Download each date for the current variable from PRISM
    dir_out <- '1_download/prism_data'
    prism_set_dl_dir(dir_out)
    get_prism_dailys(
      type = p1_prism_vars,
      dates = p2_prism_dates[year(p2_prism_dates) >= 2022],
      keepZip=FALSE
    )
    # In order to track files and changes, list the files saved in
    # the folder as the output here. This works since each subfolder
    # is named with the variable and date so adding dates or vars
    # will result in changes here. 
    var_files <- list.files(dir_out, pattern = p1_prism_vars)
    return(tibble(prism_var = p1_prism_vars,
                  prism_files = var_files))
  }, pattern = map(p1_prism_vars))
  
)
