% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_mdb_df.R
\name{make_mdb_df}
\alias{make_mdb_df}
\title{Create a base table of MDB indicators stacked long}
\usage{
make_mdb_df(df, resolve_issues = T)
}
\arguments{
\item{df}{data frame from which MDB tables will be constructed for core indicators}

\item{resolve_issues}{logical that fetches troublesome mechs and omits them from df}
}
\value{
data frame
}
\description{
Makes a single table of core MDB indicators for all OUs, countries and USAID.
Uses \code{\link[=fetch_indicators]{fetch_indicators()}} to filter indicators and resolves known issues as a default.
Output from this function feeds into \code{\link[=reshape_mdb_df]{reshape_mdb_df()}}
}
\examples{
\dontrun{
 mdb_df <- make_mdb_df(ou_im, resolve_issues = F)}
 
 
}
\seealso{
\code{\link[=reshape_mdb_df]{reshape_mdb_df()}} to reshape into gt ready data frame;
\code{\link[=fetch_indicators]{fetch_indicators()}} to filter indicators

Other data frame munging: 
\code{\link{collapse_base_tbl}()},
\code{\link{collapse_vlc_tbl}()},
\code{\link{make_mdb_tx_df}()},
\code{\link{reshape_mdb_df}()},
\code{\link{reshape_mdb_tx_df}()}
}
\concept{data frame munging}
