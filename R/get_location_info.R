#' @name get_location_info
#' Get Location Info Function
#'
#' This function allows the user to input a location ID
#' which will then provide information for the specified location
#' Users can optionally set the currency for results (default CAD)
#'
#' @param location_id The location_id to search
#' @return A tidy dataframe with information for the inputted location_id
#' @export
#' @examples
#' get_location_info()

library(httr)

get_location_info <- function(location_id) {

  params <- list(
    key = "AA79C765B0DE44CD91518F3456F2A6A8",
    language = "en",
    currency = "CAD"
  )

  loc_url <- paste0("https://api.content.tripadvisor.com/api/v1/location/", location_id, "/details")
  response <- GET(loc_url, query = params)
  loc_data <- content(response, as = "parsed")

  name <- ifelse(!is.null(loc_data$name), loc_data$name, NA)
  description <- ifelse(!is.null(loc_data$description), loc_data$description, NA)
  category <- ifelse(!is.null(loc_data$category$localized_name), loc_data$category$localized_name, NA)
  web_url <- ifelse(!is.null(loc_data$web_url), loc_data$web_url, NA)
  address <- ifelse(!is.null(loc_data$address_obj$address_string), loc_data$address_obj$address_string, NA)
  latitude <- ifelse(!is.null(loc_data$latitude), loc_data$latitude, NA)
  longitude <- ifelse(!is.null(loc_data$longitude), loc_data$longitude, NA)
  phone <- ifelse(!is.null(loc_data$phone), loc_data$phone, NA)
  rating <- ifelse(!is.null(loc_data$rating), loc_data$rating, NA)
  num_reviews <- ifelse(!is.null(loc_data$num_reviews), loc_data$num_reviews, NA)
  count_1_star <- ifelse(!is.null(loc_data$review_rating_count$`1`), loc_data$review_rating_count$`1`, NA)
  count_2_star <- ifelse(!is.null(loc_data$review_rating_count$`2`), loc_data$review_rating_count$`2`, NA)
  count_3_star <- ifelse(!is.null(loc_data$review_rating_count$`3`), loc_data$review_rating_count$`3`, NA)
  count_4_star <- ifelse(!is.null(loc_data$review_rating_count$`4`), loc_data$review_rating_count$`4`, NA)
  count_5_star <- ifelse(!is.null(loc_data$review_rating_count$`5`), loc_data$review_rating_count$`5`, NA)
  price_level <- ifelse(!is.null(loc_data$price_level), loc_data$price_level, NA)
  ranking <- ifelse(!is.null(loc_data$ranking_data$ranking_string), loc_data$ranking_data$ranking_string, NA)
  awards <- ifelse(!is.null(loc_data$awards$award_type), loc_data$awards$award_type, NA)

  info_df <- data.frame(
    "Name" = name,
    "Description" = description,
    "Category" = category,
    "Trip Advisor URL" = web_url,
    "Address" = address,
    "Latitude" = latitude,
    "Longitude" = longitude,
    "Phone" = phone,
    "Rating" = rating,
    "Number of Reviews" = num_reviews,
    "Count 1 Star Ratings" = count_1_star,
    "Count 2 Star Ratings" = count_2_star,
    "Count 3 Star Ratings" = count_3_star,
    "Count 4 Star Ratings" = count_4_star,
    "Count 5 Star Ratings" = count_5_star,
    "Price Level" = price_level,
    "Ranking" = ranking,
    "Awards" = awards
  )

  return(info_df)

}
