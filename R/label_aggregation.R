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
#' 
# Function to label aggreagation level based on input
label_aggregation <- function(df, type = "ou") {
  
  if(!type %in% c("regional", "agency", "ou")) {
    stop("Please select the type of aggregation label to apply: ou, regional or agency")
  }  
  
  if (type == "agency") {
    df %>% 
      dplyr::mutate(agg_type = "Agency", 
                    operatingunit = "Global") 
  } else if (type == "regional") {
    df %>% 
      dplyr::mutate(agg_type = "Region-Country",
                    operatingunit = paste(operatingunit, countryname, sep = "-")) %>% 
      dplyr::select(-countryname)
  } else {
    df %>% dplyr::mutate(agg_type = "OU")
  }
}
