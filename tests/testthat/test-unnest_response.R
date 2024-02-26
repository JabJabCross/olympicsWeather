test_data <- list(
  hourly = list(
    time = c("2024-02-24T00:00", "2024-02-24T01:00"),
    temperature_2m = c(5, 4.6),
    apparent_temperature = c(1.7, 1.5),
    precipitation_probability = c(0, 0),
    precipitation = c(0, 0)))


test_that("La fonction renvoie le bon nombre de lignes", {
  result <- unnest_response(test_data)
  expect_equal(nrow(result), 2)
})

test_that("Les valeurs de la colonne temperature correspondent aux valeurs proposées en entrée", {
  result <- unnest_response(test_data)
  expected_values <- c(5, 4.6)
  actual_values <- unlist(result$temperature_celsius)
  expect_equal(actual_values, expected_values)
})

test_that("Les noms des colonnes sont corrects", {
  result <- unnest_response(test_data)
  expected_columns <- c("date_heure", "temperature_celsius",
                        "temperature_ressentie_celsius", "precipitation_proba",
                        "precipitation")
  expect_equal(colnames(result), expected_columns)
})

test_that("Le nombre de colonnes en sortie est correct", {
  result <- unnest_response(test_data)
  expected_num_columns <- 5
  expect_equal(ncol(result), expected_num_columns)
})

