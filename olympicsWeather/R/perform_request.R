#' @export
library(dplyr)
library(httr2)
perform_request <- function(lat, lon) {
  url <- "https://api.open-meteo.com/v1/forecast"
  table_reponse <-
    request(url) |>
    req_url_query(latitude=lat,longitude=lon, hourly= c("temperature_2m","apparent_temperature","precipitation_probability","precipitation"), .multi = "comma"
    ) |>
    req_perform() |>
    resp_body_json() |>
    as_tibble()
  return(table_reponse)
}
