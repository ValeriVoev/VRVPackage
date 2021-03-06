% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Number of observations (cases) by month and year from the Traffic Safety Administration's
Fatality Analysis Reporting System annual datasets}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of year values in \%Y format, e.g., \code{2014}, or \code{2013:2015}}
}
\value{
A tibble (in wide format) containing the number of cases for each month and year
}
\description{
This function constructs a tibble (in wide format) with the number of cases by
month and year from a set of annual datasets.
}
\note{
The data for each year should be in format "accident_<year>.csv.bz2". The input should be provided
(or coercible into) as an integer vector in \%Y format (e.g., \code{2013:2015}). This function
uses \code{\link{fars_read_years}} to extract the month and year variables from the Traffic Safety Administration's
Fatality Analysis Reporting System annual datasets.
}
\examples{
fars_summarize_years(2015)

}
