# Helper functions for MD tables

# Functions  
# Returns the current quarter
  get_qtr <- function(period){
    #Return an erorr if the input time variable does not contain quarters
    qtr <- stringr::str_extract({{period}}, "(Q1|Q2|Q3|Q4)")
  }

# Apply achievement colors
# Requires a period variable and an achievement variable

# To check if columns already exists?
#any(names(agency) %in% c("APR", "achievement"))

  calc_achv <- function(df, ach_var, curr_qtr, add_color = TRUE){
    
    # pry need a check here for whether an ach var exists or not 
    # any(names(sd) %in% c("APR", "achievement")) -- stop if not
    
    df_achv <- df %>% 
      mutate(get_qtr = stringr::str_extract({{curr_qtr}}, "(Q1|Q2|Q3|Q4)") %>% 
               gsub("Q", "", .) %>% as.numeric(),
             qtr_goal = ifelse(indicator == "TX_CURR", 1, 1*(get_qtr/4)),
             achv_color = case_when(is.na({{ach_var}}) ~ NA_character_,
                                    {{ach_var}} <= qtr_goal - .25 ~ old_rose_light,
                                    {{ach_var}} <= qtr_goal - .1 ~ burnt_sienna_light,
                                    {{ach_var}} <= qtr_goal + .1 ~ "#5BB5D5",
                                    TRUE ~ trolley_grey_light)) %>% 
      select(-qtr_goal)
    
    return(df_achv)
  }
  

  # Create a rounded gap value based on remaining value and number of quarters left in year 
  # Should this dynamically calculate quarters remaining based on qtr input above?
  gap_calc <- function(x, y) {
    ifelse(y > 0.000, round(x / y, 0), x)
  }
  
  
  # Basical achievement calculation assuming denominator is greater then 0
  denom_share <- function(x, y) {
    ifelse(y > 0.000, (x / y), NA_real_)
  }
  
  # Function to calculate APR growth based on lagged variables
  # Should the function create the lags for you as well?
  q2q_compare <- function(x, y) {
    ifelse(x > 0.000, (x / y) - 1, NA_real_)
  }
  
  # Column names that will be used
  
  
  # Function takes 3 inputs and glues them together with HTML formatting
  # Used to format achievement, results and targets that have been concatenated together 
  fmt_aprs <- function(x){
    name <- word(x, 1)
    results <- word(x, 2)
    target <- word(x, -1)
    glue::glue(
      "<div style='line-height:10px'><span style='font-weight:regular;color:grey60k;font-variant:small-caps;font-size:14px'>{name}</div>
          <div><span style='font-weight:regular;color:grey60k;font-size:10px'>{results} /
          <span style ='font-weight:regular;color:grey60k;font-size:10px'>{target}</span></div>"
  
  
    )
  }
  
  
  fmt_curr_fy <- function(x) {
    name <- word(x, 1)
    name2 <- word(x, 2)
    glue::glue(
      "<div style='line-height:10px'><span style='font-weight:regular;font-variant:small-caps;font-size:14px'>{name}</span></div>
          <div><span style='font-weight:regular;color:grey90k;font-size:10px'>{name2}</span></div>"
    )
  }
  
  
# Function to source only parts of a files
# From https://stackoverflow.com/questions/12214963/source-only-part-of-a-file
  
  # Sources only certain parts of another r script. Useful pre-loading select chunks.
  source_parts <- function(file, start, end, ...) {
    file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
    file.lines.collapsed <- paste(file.lines, collapse='\n')
    source(textConnection(file.lines.collapsed), ...)
  }

  
  
  

# CREATE A CUSTOM COLOR PALETTE FOR TABLES --------------------------------
  
  pal <- function(x) {
    f_low <- scales::col_numeric(
      palette = c(old_rose, old_rose_light),
      domain = c(0, 0.25)
    )
    f_med <- scales::col_numeric(
      palette = c(burnt_sienna_light),
      domain = c(0.25, 0.4)
    )
    f_ok <- scales::col_numeric(
      palette = c(scooter_light),
      domain = c(0.4, 0.6)
    )
    f_great <- scales::col_numeric(
      palette = c(trolley_grey_light),
      domain = c(0.6, 100)
    )
    case_when(
      x >0 & x <0.25 ~ f_low(x),
      x >= 0.25 & x < 0.4 ~ f_med(x),
      x >= 0.40 & x < 0.6 ~ f_ok(x),
      x >= 0.60 ~ f_great(x),
      TRUE ~ "#ffffff"
    )
  }
  
  
  pal_tx <- function(x) {
    f_low <- scales::col_numeric(
      palette = c(old_rose, old_rose_light),
      domain = c(0, 0.75)
    )
    f_med <- scales::col_numeric(
      palette = c(burnt_sienna_light),
      domain = c(0.75, 0.9)
    )
    f_ok <- scales::col_numeric(
      palette = c(scooter_light),
      domain = c(0.9, 1.1)
    )
    f_great <- scales::col_numeric(
      palette = c(trolley_grey_light),
      domain = c(1.1, 100)
    )
    case_when(
      x >0 & x <0.75 ~ f_low(x),
      x >= 0.75 & x < 0.9 ~ f_med(x),
      x >= 0.90 & x < 1.1 ~ f_ok(x),
      x >= 1.1 ~ f_great(x),
      TRUE ~ "#ffffff"
    )
  }
  
  

# EXPAND SEMI-ANNUAL-Indicators -------------------------------------------


  