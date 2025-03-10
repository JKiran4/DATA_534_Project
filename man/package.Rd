\name{TripadvisoR}
\alias{TripadvisoR}
\docType{package}
\title{Tripadvisor RESTful API Wrapper}
\author{Kiran John, Foster Lockerbie, Seamus Riordan-Short}
\description{
This package contains functions to get useful information from the Tripadvisor
API. See examples below for various functions available.
}
\examples{
results <- top_ten("sushi", "restaurants", "kelowna")
latlong <- get_city("vancouver")
df <- get_location_info("location_id")
nearby_search()
}
