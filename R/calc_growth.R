#' Calculate growth between two metrics that are positive
#' 
#' Helper function for calculating growth.
#' Returns NA_real_ if the 
#' @param x numerator to calculate growth - most recent metric
#' @param y denominator to calculate growth - lagged metric
#' 
#'@examples
#' \dontrun{
#'  calc_growth(110, 100) #should give 10%
#'  df %>% mutate(z_change = calc_growth(results_cumulative, dplyr::lag(results_cumulative, n = 4)))}
#' 
calc_growth <- function(x, y) {
  ifelse(x > 0.000, (x / y) - 1, NA_real_)
}