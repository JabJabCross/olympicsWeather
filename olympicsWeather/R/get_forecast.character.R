#' @export
get_forecast.character <- function(address) {
  if (!is.character(address) || length(address) != 1) {
    stop("L'argument address doit être un caractère de taille 1.")
  }

  coords <- address_to_gps(address)
  gps_coords <- get_gps_coordinate(coords$addr)
  return(gps_coords)
}
