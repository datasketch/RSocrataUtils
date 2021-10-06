test_that("multiplication works", {

  url <- "https://data.cityofchicago.org/"
  chicago <- soc_list(url)

  fire_data <- soc_search_dataset("fire", socrata)

  dataset_id <- "28km-gtjn"
  soc_resource_url("28km-gtjn", chicago)

  meta <- soc_dataset_meta("28km-gtjn", chicago)

})
