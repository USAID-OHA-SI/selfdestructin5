#' Create time objects for gt tables
#' 
#' @description 
#' Wrapper function to create numerous time objects that are called throughout the table creation process.
#' 
#' @export
#' @importFrom usethis ui_code_block
#' @importFrom usethis ui_todo
#' 
#' 
#' @examples
#' \dontrun{
#' make_time_metadata()
#' # paste clipboard to session and run }
#' 
make_time_metadata <- function(df = ou_im){
  
  usethis::ui_code_block(
    c(
    "pd <- gophr::identifypd(df)",
    "present_fy <- paste(pd %>% substr(., 1, 4), 'Cumulative')",
    "present_qtr <- paste(pd, 'Results')",
    "present_results <- paste(pd %>% substr(., 1, 4), 'Results')",
    "past_fy <- paste0('FY', identifypd(ou_im, 'year', TRUE) %>% substr(3, 4))",
    "fy_end <- pd %>% substr(., 3, 4) %>% as.numeric() + 2000",
    "fy_beg <- fy_end - 1 ",
    "min_pd <- paste0('FY', substr(fy_beg, 3, 4), 'Q4')"
    )
  )
  
  usethis::ui_todo("Copy, paste and run this code in your R session to create the time metadata objects.")

}

