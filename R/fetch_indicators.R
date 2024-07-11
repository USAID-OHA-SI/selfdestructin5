#' Fetches indicators used to make tables, accounting for quarter of MSD and semi-annual indicators
#' 
#' MDB indicators are stored on google drive. This helper function fetches a desired tab.
#' Used to create filters, sort factor levels, and apply plain labels to indicators.
#' 
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
fetch_indicators <- function(tab = "main") {

  if (!tab %in% c("main", "treatment")) {
    stop("Please select the indicator crosswalk you would like to use: main or treatment")
  }
  
  pd <- fetch_metadata()

  # Determine the appropriate filter based on `tab` and `pd$curr_pd`
  indics <- switch(
    tab,
    main = {
      if (pd$curr_qtr %in% c(1, 3)) {
        mdb_tbl %>% dplyr::filter(mdb_table == "main", frequency == "quarterly")
      } else {
        mdb_tbl %>% dplyr::filter(mdb_table == "main")
      }
    },
    treatment = mdb_tbl %>% dplyr::filter(mdb_table == "treatment"),
    stop("Invalid tab specified")
  )

  # Pull in indicator list from stored data set mdb_tbl
  # if (tab == "main" & pd$curr_qtr %in% c(1, 3)) {
  #   indics <- mdb_tbl %>% filter(mdb_table == "main", frequency == "quarterly")
  # } else if (tab == "main" & pd$curr_qtr %in% c(2, 4)) {
  #   indics <- mdb_tbl %>% filter(mdb_table == "main")
  # } else {
  #   indics <- mdb_tbl %>% filter(mdb_table == "treatment")
  # }
  lst <- paste(indics$indicator, collapse = ", ")
  cli::cli_inform(glue::glue("Quarter:{crayon::yellow(pd$curr_pd)} indicators fetched {crayon::yellow(lst)} \n"))

  return(indics)
}
