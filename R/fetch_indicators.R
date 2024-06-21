#' Fetches indicators used to make tables, accounting for quarter of MSD and semi-annual indicators
#' 
#' MDB indicators are stored on google drive. This helper function fetches a desired tab.
#' Used to create filters, sort factor levels, and apply plain labels to indicators.
#' 
#' @param pd period used to pull quarterly or quaterly plus semi-annual indicators
#' @param tab which table is being created, main or treatment
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
<<<<<<< HEAD
fetch_indicators <- function(pd = meta, tab = "main") {

  if (!tab %in% c("main", "treatment")) {
    stop("Please select the indicator crosswalk you would like to use: main or treatment")
  }

  # Determine the appropriate filter based on `tab` and `pd$curr_pd`
  indics <- switch(
    tab,
    main = {
      if (pd$curr_pd %in% c(1, 3)) {
        mdb_tbl %>% filter(mdb_table == "main", frequency == "quarterly")
      } else {
        mdb_tbl %>% filter(mdb_table == "main")
      }
    },
    treatment = mdb_tbl %>% filter(mdb_table == "treatment"),
    stop("Invalid tab specified")
  )
  # 
  # # Pull in indicator list from stored data set mdb_tbl
  # if (tab == "main" & pd$curr_pd %in% c(1, 3)) {
  #   indics <- mdb_tbl %>% filter(mdb_table == "main", frequency == "quarterly")
  # } else if (tab == "main" & pd$curr_pd %in% c(2, 4)) {
  #   indics <- mdb_tbl %>% filter(mdb_table == "main")
  # } else {
  #   indics <- mdb_tbl %>% filter(mdb_table == "treatment")
  # }
  lst <- paste(indics$indicator, collapse = ", ")
  cli::cli_inform(glue::glue("Quarter:{crayon::yellow(pd$curr_pd)} indicators fetched {crayon::yellow(lst)} \n"))

=======
fetch_indicators <- function(df, tab = "main") {
  
  # Determine what quarter you are fetching data from
  pd <- df %>% 
    gophr::identifypd() %>% 
    stringr::str_extract(".{2}$")
  
  #cat("Fetching ", crayon::yellow(pd), " indicators\n")
  cli::cli_inform(glue::glue("Fetching {crayon::yellow(pd)} indicators\n"))
  
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
 
  #concatenate indicator list into a chracter for return message
  lst <- paste(indics$indicator, collapse = ", ")
  cli::cli_inform(glue::glue("{crayon::yellow(pd)} indicators fetched {crayon::yellow(lst)} \n"))
  
>>>>>>> origin/main
  return(indics)
}
