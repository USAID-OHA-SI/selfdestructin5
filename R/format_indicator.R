#' Formats PEPFAR indicators based on [fetch_indicator()] list
#' 
#' @description 
#' Formats a string containing the main PEPFAR abbrevation and plain text.
#' The PEPFAR abbreviation is listed first and the plain text in a line below.
#' Used to create the upper portion of the MDB tables.
#' 
#' @param x PEPFAR indicator name as an abbreviation and the full definition
#' @export
#' 
#' @return html formatted string that can be rendered in gt
#' 
#' 
#' @examples
#' \dontrun{
#'  format_indicator("HTS_TST Received HIV testing service and results") %>% 
#'  gt::html() %>%  htmltools::html_print()
#'  df %>% mutate(indicator2 = format_indicator(indicator2)) }
#'  
#'  
format_indicator <- function(x){
  name <- stringr::word(x, 1)
  name2 <- stringr::word(x, 2, -1)
  glue::glue(
    "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
        <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
  )
}
