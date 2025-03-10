#' @name nearby_search
#' 
#' @title Get Nearby Locations Function
#' 
#' @description This function allows you to search by place name and category of attraction (e.g. restaurant) and 
#' returns 10 nearby attractions of that category and their distance from the location
#'
#' @param location_name The search string to find the city
#' @param category The type of location to retrieve
#' 
#' @return A tidy dataframe with 10 locations of chosen category and their location_id, name, distance and bearing
#' nearby_search()
#' 
#' @import httr
#' @import jsonlite
#' 
#' @export

nearby_search <- function(location_name, type = NA) {
  
  latlong <- TripadvisoR::get_city(location_name)  
  
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


