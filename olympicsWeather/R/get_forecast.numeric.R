#' @export
get_forecast.numeric <- function(xy) {
  if (!is.numeric(xy) || length(xy) != 2) {
    stop("L'argument xy doit être un vecteur numérique de taille 2.")
  }
  rep <- perform_request(xy[1], xy[2])

  unnested_table <- unnest_response(rep)

  return(unnested_table)
}

