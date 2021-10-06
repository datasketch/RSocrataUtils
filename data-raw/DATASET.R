## code to prepare `DATASET` dataset goes here



library(tidyverse)
library(RSocrata)
library(jsonlite)

url <- "https://datos.gov.co"
soc_download_list(url, "data-raw/datosgovco")


url <- "https://data.cityofchicago.org/"
chicago <- soc_list(url)



all_datasets <- ls.socrata("https://datos.gov.co")

# flatten dataframe

al <- jsonlite::flatten(all_datasets)

# convert NULLs to flat vector
al2 <- al %>% map(function(col){
  y <- map(col, function(x){
    if(is.null(x)) return(NA)
    x
  })
  if(length(unlist(y)) == length(col)) return(unlist(y))
  y
}) %>% as_tibble()

write_rds(al)


# select all non list
al_non_list <- al2 %>% select(!where(is.list))

# select all list
al_list <- al2 %>% select(where(is.list))


write_csv(al_non_list, "tmp/dataset.csv")

xx <- read_csv("tmp/dataset.csv", col_types = cols(.default = "c"))

#jsonlite::write_json(all_datasets, "tmp/datasets.datos.gov.co.json")

dists <- al_list$distribution




attributes(all_datasets)

str(all_datasets)
map(al, class)

ct <- all_datasets$contactPoint

#jsonlite::write_json(all_datasets, "tmp/datasets.datos.gov.co.json")



pubs1 <- all_datasets$publisher[[1]]
attributes(pubs1)


write_csv(all_datasets, "tmp/datasets_datosgovco.csv")



usethis::use_data("DATASET")
