% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_city.R
\name{get_city}
\alias{get_city}
\title{Get City Function}
\usage{
get_city(string)
}
\arguments{
\item{string}{The search string to find the city}
}
\value{
A string in the format "YY.YYYYYY\%2CXXX.XXXXX" where X and Y are latitude and longitude coordinates
}
\description{
This function allows you to search by city name and returns the latitude and
longitude formatted for use in further API requests
}
