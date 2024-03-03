library(devtools)
library(httr2)
library(jsonlite)
library(tibble)
library(purrr)
library(usethis)
library(testthat)
library(dplyr)


#7
perform_request <- function(lat, lon) {
  library(jsonlite)
  library(tibble)
  library(httr2)
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
rep <- perform_request(48.85, 2.35)

#8

unnest_response <- function(rep) {
  library(tibble)
  unnested_table <- tibble(date_heure = unlist(rep$hourly[1][[1]]),
                           temperature_celsius=unlist(rep$hourly[2][[1]]),
                           temperature_ressentie_celsius=unlist(rep$hourly[3][[1]]),
                           precipitation_proba=unlist(rep$hourly[4][[1]]),
                           precipitation = unlist(rep$hourly[5][[1]]))

  col_names <- c(
    "date_heure",
    "temperature_celsius",
    "temperature_ressentie_celsius",
    "precipitation_proba",
    "precipitation"
  )
  names(unnested_table) <- col_names
  return(unnested_table)
}

rep <- perform_request(48.85, 2.35)
unnested_table <- unnest_response(rep)

unnested_table

#9
usethis::use_test("unnest_response")

#10
library(tidygeocoder)
address_to_gps <- function(adresse) {
  library(tidygeocoder)
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)

  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)

  resultat_geocodage <- df_adresse |>
    geocode(addr, method = 'arcgis')

  df_adresse <- resultat_geocodage

  return(df_adresse)
}

resultat_final <- address_to_gps("17 rue du champ neuf, Le Rheu, 35650, FRANCE")
print(resultat_final)

#11
get_gps_coordinate <- function(adresse){
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)
  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)

  df_resultat <- address_to_gps(df_adresse)

  return(c(lat = df_resultat$lat, lon = df_resultat$long))
}

resultat_final2 <- get_gps_coordinate(adresse = "17 rue du champ neuf, Le Rheu, 35650, FRANCEE")
print(resultat_final2)

#12
get_forecast.numeric <- function(xy) {
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("L'argument xy doit être un vecteur numérique de taille 2.")
  }
    rep <- perform_request(xy[1], xy[2])

  unnested_table <- unnest_response(rep)

  return(unnested_table)
}

result <- get_forecast.numeric(c(44.7, 2.35))
print(result)

#13

get_forecast.character <- function(address) {
  library(tidygeocoder)
  if (!is.character(address) || length(address) != 1) {
    stop("L'argument address doit être un caractère de taille 1.")
  }

  coords <- address_to_gps(address)
  gps_coords <- get_gps_coordinate(coords$addr)
  return(gps_coords)
}
resultat_final <- get_forecast.character("stade de France")
print(resultat_final)

#fonction finale
#' Obtient les prévisions météorologiques à partir d'une adresse ou de coordonnées géographiques.
#'
#' Cette fonction permet d'obtenir les prévisions météorologiques en fonction de l'entrée fournie, qui peut être une adresse de type character ou des coordonnées géographiques de type numeric de taille 2.
#' Si une adresse est fournie, elle est convertie en coordonnées géographiques à l'aide de la fonction get_forecast.character. Si des coordonnées géographiques sont fournies, elles sont utilisées directement.
#' Ensuite, les prévisions météorologiques sont obtenues à partir des coordonnées géographiques à l'aide de la fonction get_forecast.numeric.
#'
#' @param x Une adresse de type character ou des coordonnées géographiques de type numeric de taille 2.
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#' @return Un tibble contenant les prévisions météorologiques.
#' @examples
#' # Obtention des prévisions météorologiques pour une adresse
#' resultat_final <- get_forecast("17 rue du champ neuf, Le Rheu, 35650, FRANCE")
#' print(resultat_final)
#'
#' # Obtention des prévisions météorologiques pour des coordonnées géographiques
#' result <- get_forecast(c(48.1, -1.8))
#' print(result)
#' @export
get_forecast <- function(x) {
  if (is.character(x)) {
    coords <- get_forecast.character(x)
  } else if (is.numeric(x) && length(x) == 2) {
    coords <- x
  } else {
    stop("L'argument doit être une adresse de type character ou des coordonnées géographiques de type numeric de taille 2.")
  }

  result <- get_forecast.numeric(coords)

  return(result)
}

resultat_final <- get_forecast("Saint-Félicien")
view(resultat_final)

result <- get_forecast(c(48.1, -1.8))
view(result)





