#' Label aggregation level
#' 
#' @description
#' Labels the level of aggregation using regiona, agency or operating unit
#' Used to apply a filter column to the single data frame returned from table operations
#' 
#' @param df MSD or genie data frame that has been ran through collapse_base_tbl()
#' @param type Level of aggregation to be labeled
#' 
#' @export 
#' 
#' @family MDB helper functions
#' 
#' 
# Function to label aggreagation level based on input
label_aggregation <- function(df, type = "OU") {
  
  if(!type %in% c("Regional", "Agency", "OU")) {
    stop("Please select the type of aggregation label to apply: OU, Regional or Agency")
  }  
  
  if (type == "Agency") {
    df %>% 
      dplyr::mutate(agg_type = "Agency", 
                    operatingunit = "Global") 
  } else if (type == "Regional") {
    df %>% 
      dplyr::mutate(agg_type = "Region-Country",
                    operatingunit = paste(operatingunit, country, sep = "-")) %>% 
      dplyr::select(-country)
  } else {
    df %>% dplyr::mutate(agg_type = "OU")
  }
}
