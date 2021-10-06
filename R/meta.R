

#' @export
soc_dataset_meta <- function(dataset_id, socrata = NULL){

  # socrata can by a soc list or a url
  if(!"soc_list" %in% class(socrata)){
    socrata <- soc_list(socrata)
  }
  sl <- dplyr::as_tibble(socrata)


  dataset_id_in <- dataset_id
  data_meta <- sl %>% dplyr::filter(dataset_id == dataset_id_in)

  dist <- data_meta$distribution[[1]] %>% filter(mediaType == "application/json")
  columns <- dist$describedBy
  cols <- jsonlite::read_json(columns, simplifyVector = TRUE)
  dic <- cols

  data_meta <- purrr::transpose(data_meta)[[1]]
  data_meta$dictionary <- dic
  data_meta
}
