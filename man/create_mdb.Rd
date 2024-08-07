% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_mdb.R
\name{create_mdb}
\alias{create_mdb}
\title{Wrapper function to create a MDB table}
\usage{
create_mdb(df, ou, type = "main", legend = NULL, legend_height = 20)
}
\arguments{
\item{df}{data frame resulting from running \code{\link[=reshape_mdb_df]{reshape_mdb_df()}} or \code{\link[=reshape_mdb_tx_df]{reshape_mdb_tx_df()}}}

\item{ou}{operating unit for which table is to be returned}

\item{type}{type of table to be created, main or treatment}

\item{legend}{can pass a legend to subtitle if desired, default is NULL}

\item{legend_height}{adjusts the height of the preset legend}
}
\value{
mdb_gt a gt object formatted as the main or treatment table
}
\description{
Creates a MDB table for a specified OU or list of OUS.
Can be called in a purrr statement for batch creating
}
\examples{
\dontrun{
 create_mdb(mdb_tbl, "Global")
 create_mdb(mdb_tbl_tx, "Zambia", type = "treatment")
 
 # Batch produce tables
 
 ou_batch <- mdb_tbl_tx \%>\% 
 filter(agg_type == "OU") \%>\% 
 distinct(operatingunit) \%>\% pull()
 
 purrr::map(ou_batch, ~create_mdb(mdb_tbl, ou = .x))
 
 
 # More advanced example selecting single indicator across numerous OUS
 
 create_mdb(mdb_tbl_tx \%>\% filter(indicator == "VLC"), 
 c("Malawi", "Zambia"), type = "treatment") \%>\% 
 cols_unhide(operatingunit)}


}
