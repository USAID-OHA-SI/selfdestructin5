% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_helpers.R
\name{bold_column}
\alias{bold_column}
\title{Bold columns inside gt objects
Helper function to quickly make columns within table bold}
\usage{
bold_column(gt_obj, col, wt = 700)
}
\arguments{
\item{gt_obj}{gt object to be bolded}

\item{col}{column or columns to be bolded}

\item{wt}{weight of boldness can be lighter, normal, bold, or bolder or 0-1000}
}
\value{
a modified gt object
}
\description{
Bold columns inside gt objects
Helper function to quickly make columns within table bold
}
\examples{
\dontrun{
 mtcars \%>\% 
 gt(groupname_col  = "cyl") \%>\% 
 bold_column(c(mpg, hp, drat, carb), wt = "bolder")
 }
}
