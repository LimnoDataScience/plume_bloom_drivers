
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
