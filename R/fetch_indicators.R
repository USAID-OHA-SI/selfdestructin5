#' Fetches indicators used to make tables, accounting for quarter of MSD and semi-annual indicators
#' 
#' MDB indicators are stored on google drive. This helper function fetches a desired tab.
#' Used to create filters, sort factor levels, and apply plain labels to indicators.
#' 
#' @param df data frame on which indicators are to be fetched
#' @param tab which tab to fetch (main or treatment) from googlesheet
#' @return dataframe of indicators 
#'  
#' @export
#' 
#'@examples
#' \dontrun{
#'  fetch_indicators(ou_im, tab = "main")
#'  fetch_indicators(ou_im, tab = "treatment")}
#'   
#'   
fetch_indicators <- function(df, tab = "main") {
  
  # Determine what quarter you are fetching data from
  pd <- df %>% 
    ICPIutilities::identifypd() %>% 
    stringr::str_extract(".{2}$")
  cat("Fetching ", crayon::yellow(pd), " indicators\n")
  
  if(!tab %in% c("main", "treatment")) {
    stop("Please select the indicator crosswalk you would like to use: main or treatment")
  }
  
  # Pull in indicator list from google drive
  if (tab == "treatment") {
    indic_list <- googlesheets4::read_sheet("1Xv9QQp6AkDdKxSYGRQ8UYJidBE8LzMnEzjIlYEur83A", sheet = "indicator_tx")
  } else  {
    indic_list <- googlesheets4::read_sheet("1Xv9QQp6AkDdKxSYGRQ8UYJidBE8LzMnEzjIlYEur83A", sheet = "indicator_cw")
  } 
  
  # Extract the correct vector of indicators for the tables
  if (pd %in% c("Q1", "Q3")) {
    indics <- indic_list %>% 
      dplyr::filter(frequency != "semi-annual") 
  } else {
    indics <- indic_list 
  }
  cat(crayon::yellow(pd), " indicators fetched ", crayon::yellow(indics$indicator), "\n")
  
  return(indics)
}