
source("getcity_function.R")

library(httr)
library(jsonlite)


#api_key <- "AA79C765B0DE44CD91518F3456F2A6A8"


nearby_search <- function(location_name, category, api_key = api_key) {
  
  latlong <- get_city(location_name)
  
  if (latlong == "") {
    return("Error: Could not retrive lat/long for this location")
  }
  
  latlong_decoded <- URLdecode(latlong)
  
  latlong_split <- strsplit(latlong_decoded, ",")[[1]]
  lat <- as.numeric(latlong_split[1])
  lon <- as.numeric(latlong_split[2])
  
  latLong <- paste0(lat, ",", lon)
  
  params <- list(
    key = api_key,
    latLong = latLong,
    category = category
  )
  
  response <- GET("https://api.content.tripadvisor.com/api/v1/location/nearby_search", query = params)
  
  if (status_code(response) == 200) {
    
    content_data <- content(response, as = "parsed")
    
    nearby_locations <- content_data$data
    
    if (length(nearby_locations) > 0) {
      results <- data.frame(
        location_id <- sapply(nearby_locations, function(loc) loc$location_id),
        name = sapply(nearby_locations, function(loc) loc$name),
        distance = sapply(nearby_locations, function(loc) loc$distance),
        bearing = sapply(nearby_locations, function(loc) loc$bearing)
      )
      return(results)
    } else {
      return("No nearby locations found.")
    }
    
  } else {
    return(paste("Error:", status_code(response)))
  }
}

nearby_locations <- nearby_search(lat = 42.3455, lon = -71.10767, category = "hotels", api_key = api_key)