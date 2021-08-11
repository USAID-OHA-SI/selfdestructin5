#' Create fontawesome performance arrow
#' 
#' @description 
#' Creates an svg object based on the direction of change of a performance variable.
#' The svg code can be embedded in a data frame and passed to gt() for prettifying a table.
#' Appears to work well when applied with purrr::map()
#' 
#' 
#' @param change_dir argument indicating the direction of change 
#' @return gt html() code for an svg
#' 
#' @param change_dir variable indicating direction of change or not applicable
#' @export
#' 
#' @family MDB helper functions
#' 
#' @examples
#' \dontrun{
#'  rank_chg("increase") %>% htmltools::html_print()
#'  rank_chg("decrease") %>% htmltools::html_print()
#'  df %>% mutate(chg_dir = purrr::map(present_z_direction, rank_chg))
#'  }
#' 
#' 
make_chg_shape <- function(change_dir){
  
  alpha = 0.75
  # Extra if statement to catch NA cells
  if(is.na(change_dir)){
    logo_out <- fontawesome::fa("circle", fill = glitr::trolley_grey_light, fill_opacity = alpha)
  } else {
    if (change_dir == "increase") {
      logo_out <- fontawesome::fa("arrow-circle-up", fill = glitr::genoa_light, fill_opacity = alpha)
    } else if (change_dir == "decrease"){
      logo_out <- fontawesome::fa("arrow-circle-down", fill = glitr::old_rose_light, fill_opacity = alpha)
    } else if (change_dir == "not applicable" | is.na(change_dir)) {
      logo_out <- fontawesome::fa("circle", fill = glitr::trolley_grey_light, fill_opacity = alpha)
    } 
  }
  logo_out %>% as.character() %>% gt::html()
}