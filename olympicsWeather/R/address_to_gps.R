#' @export
library(tidygeocoder)
address_to_gps <- function(adresse) {
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)

  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)

  resultat_geocodage <- df_adresse |>
    geocode(addr, method = 'arcgis')

  df_adresse <- resultat_geocodage

  return(df_adresse)
}
