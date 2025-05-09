#' @name get_city
#'
#' @title Get City Function
#'
#' @description This function allows you to search by city name and returns the latitude and
#' longitude formatted for use in further API requests
#'
#' @param string The search string to find the city
#'
#' @return A string in the format "YY.YYYYYY%2CXXX.XXXXX" where X and Y are latitude and longitude coordinates
#'
#' @import httr
#'
#' @export

get_city <- function(string) {

  api_key <- Sys.getenv("API_KEY")

  if (length(string) > 1) {
    return ("Invalid input, pass only 1 search string argument")
  }

  params <- list(
    key = api_key,
    searchQuery = string,
    category = "geo",
    language = "en"
  )

  response <- GET("https://api.content.tripadvisor.com/api/v1/location/search", query = params)

  if (status_code(response) != 200) {
    return(paste("Error:", status_code(response)))
  }

  content_data <- content(response, as = "parsed")
  location_id <- content_data$data[[1]]$location_id

  loc_url <- paste0("https://api.content.tripadvisor.com/api/v1/location/", location_id, "/details")

  params2 <- list(
    key = api_key,
    language = "en",
    currency = "CAD"
  )

  response2 <- GET(loc_url, query = params2)
  content_data2 <- content(response2, as = "parsed")
  latlong <- paste0(content_data2$latitude, "%2C", content_data2$longitude)

  return(latlong)

}
