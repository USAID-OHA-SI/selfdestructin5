# utils.R

# Create an environment to store the metadata
pkg_env <- new.env()

#' Set metadata
#' 
#' @description 
#' Sets the metadata for the package, which can be used across various functions.
#' 
#' @param meta A list containing metadata information.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'  metadata <- gophr::get_metadata(file_path)
#'  set_metadata(metadata)
#' }
set_metadata <- function(meta) {
  if (!is.list(meta)) {
    stop("Metadata should be a list.")
  }
  pkg_env$meta <- meta
  invisible(NULL)
}

#' Fetch metadata
#' 
#' @description 
#' Retrieves the metadata set for the package.
#' 
#' @return A list containing metadata information.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'  meta <- get_metadata()
#' }
fetch_metadata <- function() {
  if (!exists("meta", envir = pkg_env)) {
    stop("Metadata has not been set. Please use set_metadata() to set it.")
  }
  return(pkg_env$meta)
}
              