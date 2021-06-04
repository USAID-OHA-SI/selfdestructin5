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
  
  indic_def <- 
    tibble::tribble(
      ~indic_category,    ~indicator,                                           ~indicator_plain,
      "prevention",    "PrEP_NEW", "Newly enrolled on antiretroviral pre-exposure prophylaxis",
      "prevention",   "VMMC_CIRC",    "Voluntary medical male circumcision for hiv prevention",
      "testing",     "HTS_TST",                  "Received HIV testing service and results",
      "testing", "HTS_TST_POS",         "Received HIV testing service and positive results",
      "treatment",      "TX_NEW",                  "Newly enrolled on antiretroviral therapy",
      "treatment",     "TX_CURR",                "Currently receiving antiretroviral therapy"
    ) 
  




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
