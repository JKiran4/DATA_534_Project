#' @name top_ten
#'
#' @title Top Ten Function
#'
#' @description This function returns IDs and names on the top ten best matches of businesses
#' based on a search string and optional business type and location filters
#'
#' @param string The search string for the query
#' @param type Optional field to limit the business type to one of "hotels",
#' "attractions", "restaurants", or "geos".
#' @param city Optional field that will limit search to a specific geo location
#'
#' @return 10 obs. of 4 variables: id, name, country, address
#'
#' @import httr
#'
#' @export

top_ten <- function(string, type = NA, city = NA) {

  options <- c("hotels", "attractions", "restaurants", "geos", NA)

  if (!(type %in% options)) {
    print("Invalid business type input, defaulting to no type filter. Must be one of 'hotels', 'attractions', 'restaurants', or 'geos'.")
    type = NA
  }

  if (is.na(city)) {
    params <- list(
      key = api_key,
      searchQuery = string,
      category = type,
      language = "en"
    )
  }

  else {
    params <- list(
      key = api_key,
      searchQuery = string,
      category = type,
      language = "en",
      latLong = get_city(city)
    )
  }

  response <- GET("https://api.content.tripadvisor.com/api/v1/location/search", query = params)

  if (status_code(response) == 200) {

    content_data <- content(response, as = "parsed")

    results <- data.frame(
      id = character(),
      name = character(),
      country = character(),
      address = character(),
      stringsAsFactors = FALSE
    )

    if (length(content_data$data) == 0) { return("No results!") }

    for (i in 1:length(content_data$data)) {
      results[i, "id"] <- content_data$data[[i]]$location_id
      results[i, "name"] <- content_data$data[[i]]$name
      results[i, "country"] <- content_data$data[[i]]$address_obj$country
      results[i, "address"] <- content_data$data[[i]]$address_obj$address_string
    }

    return(results)

  } else {
    return(paste("Error:", status_code(response)))
  }

}
