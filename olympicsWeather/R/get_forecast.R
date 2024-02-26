#' Obtient les prévisions météorologiques à partir d'une adresse ou de coordonnées géographiques.
#'
#' Cette fonction permet d'obtenir les prévisions météorologiques en fonction de l'entrée fournie, qui peut être une adresse de type character ou des coordonnées géographiques de type numeric de taille 2.
#' Si une adresse est fournie, elle est convertie en coordonnées géographiques à l'aide de la fonction get_forecast.character. Si des coordonnées géographiques sont fournies, elles sont utilisées directement.
#' Ensuite, les prévisions météorologiques sont obtenues à partir des coordonnées géographiques à l'aide de la fonction get_forecast.numeric.
#'
#' @param x Une adresse de type character ou des coordonnées géographiques de type numeric de taille 2.
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

