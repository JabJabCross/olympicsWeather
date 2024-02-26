#' @export
unnest_response <- function(rep) {
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
