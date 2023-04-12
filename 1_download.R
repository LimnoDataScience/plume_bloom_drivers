
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
  pattern = map(p1_gd_netcdfs))

)
