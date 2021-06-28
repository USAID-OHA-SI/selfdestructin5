# PURPOSE: Write pngs and csvs to google drive folders
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-06-28
# NOTES: Run after completion of 01_Create_MD_tables.R & 02_Create_MD_tables_vlc.R

# LOCALS & SETUP ============================================================================

  # Where does this stuff live on the drive?
  # These will need to be retrieved each quarter
  # TODO: CREATE a FUNCTION TO CREATE FOLDERS / FETCH ID BASED ON fy / period / MSD release (i or c)
    gdrive_ou_c <- "102iefMh1DURa_4nxwE7MVy0Tz_ANOf3i"
    gdrive_asia_c <- "1vXnqhFm37KvqocidccSgJk0xnaPKkEpW"
    gdrive_afr_c <- "1KPggkgZ_4TkUClC113bWa2Rtx_gyfPtL"
    gdrive_wh_c <- "1z94UO8-ztxKQblpLeRj_BVcqRdJFwZ22"
    gdrive_glbl_c <- "1lLsxKKVU3LzUB1A6Fml7cKHAU4RQynfZ"

    gdrive_keyind_data <- "1WdYd0nOJo5B4GKQ65qAbxNBBBhQ2JMwT"
    gdrive_mmd_data <- "1IyP-guJehnYOGoU48-BQdbV9xX4_xlwI"

# POST TO GOOGLE DRIVE ----------------------------------------------------
  
  # TODO: CREATE a FUNCTION TO FETCH FILES FROM EACH LEVEL OF FOLDERS
  # AND MAP TO THE APPROPRIATE DRIVE CALL. BASICALLY, A MAP OVER THE LOCAL
    
  # Upload to Google drive now
    local_files <- list.files("Images/OU", full.names = T)
    local_files_asia <- list.files("Images/Regional/Asia", full.names = T)
    local_files_afr <- list.files("Images/Regional/WAR", full.names = T)
    local_files_wh <- list.files("Images/Regional/WesternHemi", full.names = T)
    local_files_glb <- list.files("Images/Global/", full.names = T)
   
  # Need to ensure this is fetching the latest batch of .csvs   
    local_files_rawdata <- list.files("Dataout/", pattern = "_RAW.csv$", full.names = T)
    local_files_mmd_rawdata <- list.files("Dataout/MMD_VLC", pattern = "_RAW.csv$", full.names = T)
    
  # Write to Google drive folder (either i or c depending on MSD release)    
  # OU level  
    walk(local_files,
         ~drive_upload(.x,
                       path = as_id(gdrive_ou_c),
                       name = basename(.x))
    )
   
  # Asia Regional 
    walk(local_files_asia,
         ~drive_upload(.x,
                       path = as_id(gdrive_asia_c),
                       name = basename(.x))
    )
    
    
  # WAR
    walk(local_files_afr,
         ~drive_upload(.x,
                       path = as_id(gdrive_afr_c),
                       name = basename(.x))
    )
    
  # WESTERN HEMI
    walk(local_files_wh,
         ~drive_upload(.x,
                       path = as_id(gdrive_wh_c),
                       name = basename(.x))
    )
    
  #GLOBAL
    walk(local_files_glb,
         ~drive_upload(.x,
                       path = as_id(gdrive_glbl_c),
                       name = basename(.x))
    )
    
# Data posting
  # Key Indicators
    walk(local_files_rawdata,
         ~drive_upload(.x,
                       path = as_id(gdrive_keyind_data),
                       name = basename(.x))
    )
    
  # MMD VLC Data
    walk(local_files_mmd_rawdata,
         ~drive_upload(.x,
                       path = as_id(gdrive_mmd_data),
                       name = basename(.x))
    )    
