\name{TripadvisoR}
\alias{TripadvisoR}
\title{Tripadvisor RESTful API Wrapper}
\author{Kiran John, Foster Lockerbie, Seamus Riordan-Short}
\description{
This function gets useful information from the Tripadvisor API. See examples below
for various functions available.
}
\examples{
results <- top_ten("sushi", "restaurants", "kelowna")
latlong <- get_city("vancouver")
df <- get_location_info("location_id")
nearby_search()
}
