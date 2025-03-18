\name{TripadvisoR}
\alias{TripadvisoR}
\docType{package}
\title{Tripadvisor API Wrapper in R}
\author{Kiran John, Foster Lockerbie, Seamus Riordan-Short}
\description{
An all-inclusive R package for retrieving Tripadvisor information. The package offers a suite of functions for retrieving information from the Tripadvisor API. 
    Note that users must acquire a free API key from Tripadvisor before running any functions - users are able to make 5,000 free monthly calls before being charged. 
    See the examples below for the various functions available.
}
\examples{
api_key <- "YOUR_API_KEY"
results <- top_ten(string = "sushi", type = "restaurants", city = "Kelowna")
latlong <- get_city(string = "Vancouver")
information <- get_location_info(location_id = 1996323)
nearby_locations <- nearby_search(location_name = "YAMA Kelowna", type = "restaurants")
}
