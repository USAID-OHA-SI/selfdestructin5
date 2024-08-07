% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape_mdb_tx_df.R
\name{reshape_mdb_tx_df}
\alias{reshape_mdb_tx_df}
\title{Prepare filtered data frame of treatment indicators for MDB formatting}
\usage{
reshape_mdb_tx_df(df)
}
\arguments{
\item{df}{dataframe that is the result of running make_mdb_tx_df()}
}
\value{
data frame that is pivoted wide for passing to gt() call
}
\description{
\code{reshape_mdb_tx_df} takes the output from \code{\link[=make_mdb_tx_df]{make_mdb_tx_df()}} and creates a gt ready data frame.
The transformed data frame retains the most recent quarter and previous year performance.
Viral load coverage and viral load suppression percentages are calculated along with TX MMD shares.
The resulting table can be passed directly to the \code{\link[=mdb_treatment_theme]{mdb_treatment_theme()}} to create a MDB table.
Helper functions format certain columns as svgs to be rendered in the gt call.
}
\details{
Create a wide formatted and sorted table for treatment indicators
}
\examples{
\dontrun{
 mdb_df <- make_mdb_tx_df(ou_im, resolve_issues = F)
 mdb_tbl <- reshape_mdb_tx_df(mdb_df)
 }




}
\seealso{
Other data frame munging: 
\code{\link{collapse_base_tbl}()},
\code{\link{collapse_vlc_tbl}()},
\code{\link{make_mdb_df}()},
\code{\link{make_mdb_tx_df}()},
\code{\link{reshape_mdb_df}()}
}
\concept{data frame munging}
