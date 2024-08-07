% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{set_metadata}
\alias{set_metadata}
\title{Set metadata}
\usage{
set_metadata(meta)
}
\arguments{
\item{meta}{A list containing metadata information.}
}
\description{
Sets the metadata for the package, which can be used across various functions.
}
\examples{
\dontrun{
 metadata <- gophr::get_metadata(file_path)
 set_metadata(metadata)
}
}
