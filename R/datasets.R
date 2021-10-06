
#' @export
soc_list <- function(url){

  all_datasets <- ls.socrata(url)

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

  class(al2) <- c(class(al2), "soc_list")
  al2

}


#' @export
soc_list_save <- function(x, name, format = "rds"){
  if(!"soc_list" %in% class(x)){
    al2 <- soc_list(x)
  }
  if(format == "rds") {
    write_rds(al2, paste0(name, ".", format))
  } else if (format == "csv"){
    al_non_list <- al2 %>% select(!where(is.list))
    write_csv(al_non_list, path)
  }else{
    stop("format must be rds or csv")
  }

}
