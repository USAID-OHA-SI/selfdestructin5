#' Generate colored circle as an svg 
#' 
#' Description
#' Function to create and color achievement circles.
#' The circles are placed next to the most recent achievement level in the main MDB table.
#' 
#' 
#' Returns a html formatting circle that can be embedded in gt object
#' @param x color hex code created by adorn_achievement function
#' 
#' @returns return gt html() code for an svg
#' @export
#' 
#' @family MDB helper functions
#' 
#'@examples
#' \dontrun{
#'  achv_circle(glitr::scooter) %>% htmltools::html_print()
#'  df %>% mutate(achv_color = achv_circle(achv_color)) }

make_achv_shape <- function(x){
  if(is.na(x)){
    fontawesome::fa("circle", fill = "white", height = "2em", prefer_type = "solid") %>% as.character() %>% gt::html()
  } else {
    fontawesome::fa("circle", fill = x, height = "2em", prefer_type = "solid") %>% as.character() %>% gt::html()
  }
}