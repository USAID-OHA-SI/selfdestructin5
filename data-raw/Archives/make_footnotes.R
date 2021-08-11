#' Create recommended table footnotes
#' 
#' @description 
#' Wrapper function to create footnotes that are called throughout the table creation process.
#' 
#' @param msd_source key metadata from the MSD used in the table creation
#' 
#' @export
#' @importFrom usethis ui_code_block
#' @importFrom usethis ui_todo
#' 
#' 
#' @examples
#' \dontrun{
#' make_footnotes()
#' # paste clipboard to session and run }
#' 
make_footnotes <- function(msd_source){
  
  if(!exists("msd_source")){
    usethis::ui_oops("Please create the msd_source object using:") 
    usethis::ui_code_block(c('msd_source <- pd %>% msd_period(period = .)'))

  } else {
    usethis::ui_code_block(
      c(
        "authors <- glue::glue('Created by Core Analytics Cluster on', Sys.Date(), 'using', '{msd_source}')",
        "caveats <- c('Certain mechanisms have been omitted. See the Known Issues Tracker for full list of mechanisms omitted.')",
        "dedup_note <- c('ALL OTHER AGENCIES based on aggregates excluding de-duplication.')",
        "change_note <- c('Number reflects percentage change from the same quarter in the previous year.')",
        "delta_note <- c('Number reflects the change between current and most recent quarter.')",
        "vlc_note <- c('Viral Load Covererage = TX_PVLS_D / TX_CURR_2_period_lag')"
      )
    )
    usethis::ui_todo("Copy, paste and run this code in your R session to create the footnotes objects.")
  }
}

