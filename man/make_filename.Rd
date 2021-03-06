% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{make_filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{Integer indicating the year desired}
}
\value{
Char vector Filename in the format "accident_<year>.csv.bz2"
}
\description{
Function to create a string to be used as a filename in the format "accident_<year>.csv.bz2".
}
\note{
Using anything other than an integer for the year paramenter will result in an error.
}
\examples{
 \dontrun{
  make_filename(99)
  make_filename(2018)
 }

}
