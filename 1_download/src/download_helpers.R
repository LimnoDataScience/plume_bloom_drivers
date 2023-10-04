
gd_auth <- function(email_to_use) {
  
  # Login
  drive_auth(email = email_to_use)
  
  # Check login
  user_logged_in <- drive_user()
  
  if(is.null(user_logged_in)) {
    stop('\n
  You need to initialize your login email outside of the 
  current targets build. Run `googledrive::drive_auth()`, 
  follow the prompts to authorize the email you added, 
  and then try `tar_make()` again.')
  } else {
    message(sprintf('You are authenticated with `%s`. ', 
                    user_logged_in$emailAddress))
  } 
  
  return(user_logged_in)
}

# Unzip the very nested tif zips from HydroShare
# Structure is MISSION.zip > MISSION > MISSION.zip
# With some metadata files in the nested folder.
unzip_tifs <- function(in_zip, out_dir, overwrite=TRUE) {
  
  # Unzip the first layer
  tmp <- tempfile()
  zip::unzip(zipfile = in_zip, exdir = tmp, overwrite=TRUE)
  
  # Create the out directory if it doesn't exist yet
  if(!dir.exists(out_dir)) dir.create(out_dir)
  
  # Identify files and unzip the nested zip file
  zip_nested <- list.files(tmp, pattern = '.zip', recursive = T, full.names = T)
  
  # For the Sentinel zipfiles, there is even one more layer of zip files grouped
  # by year, so the following is necessary in order to get ALL of them unzipped.
  zips_extracted_tifs_all <- purrr::map(zip_nested, function(zip_fn) {
    zips_extracted_all <- zip::zip_list(zip_fn)$filename
    zips_extracted_tifs <- zips_extracted_all[!grepl('__MACOSX/', zips_extracted_all)]
    zip::unzip(zipfile = zip_fn, exdir = out_dir, files = zips_extracted_tifs, overwrite=overwrite)
    return(zips_extracted_tifs)
  }) %>% reduce(c)
  
  return(zips_extracted_tifs_all)
}
