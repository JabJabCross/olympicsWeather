#' @export
get_gps_coordinate <- function(adresse){
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)
  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)

  df_resultat <- address_to_gps(df_adresse)

  return(c(lat = df_resultat$lat, lon = df_resultat$long))
}
