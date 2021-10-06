


#' @export
soc_resource_url <- function(dataset_id, socrata){
  if(!"soc_list" %in% class(socrata)){
    socrata <- soc_list(socrata)
  }
  sl <- dplyr::as_tibble(socrata)

  dataset_id_in <- dataset_id
  sl %>% dplyr::filter(dataset_id == dataset_id_in) %>%
    dplyr::pull(resource_url)
}


#' @export
soc_get_distinct <- function(var, table = "secop1"){
  valid_var(var, table)
  q <- paste0(select_query(table = table), soc_distinct(var))
  q <- URLencode(q)
  x <- jsonlite::fromJSON(q)
  x[,1]
}

soc_distinct <- function(var){
  glue::glue("distinct {var}")
}


#' @export
soc_search_list <- function(l, logical = "or", table = "secop1"){
  q <- soc_query_list(l, logical = logical, table = table)
  read.socrata(q)
}

soc_query_list <- function(l, logical = "or", table = "secop1"){
  paste0(where_query(table = table), list_query_str(l, logical = logical))
}

list_query_str <- function(l, logical = "or", match = "in"){
  val_vars <- unlist(lapply(l, function(x){
    if(!all(c("var", "val", "match") %in% names(x)))
      stop("l must be named: var, val and match", names(x))
    valid_match(x$match)
    do.call(paste0("soc_", x$match), list(x$var, toupper(x$val)))
  }))
  paste0(val_vars, collapse = paste0(" ", logical, " "))
}



#' @export
soc_search <- function(var, val, match = "in", table = "secop1"){
  q <- soc_query(var, toupper(val), match = match, table = table)
  read.socrata(q)
}


soc_query <- function(var, val, match = "in", table = "secop1"){
  valid_var(var, table)
  valid_match(match)
  search <- do.call(paste0("soc_",match), list(var, val))
  paste0(where_query(table = table), search)
}


soc_in <- function(var, values){
  vals <- paste0("'",paste0(values,collapse = "','"),"'")
  glue::glue("upper({var}) in({vals})")
}

soc_contains <- function(var, value){
  glue::glue("contains(upper({var}),'{value}')")
}

soc_equals <- function(var, value){
  glue::glue("upper({var})=='{value}'")
}


valid_match <- function(match){
  if(!match %in% c("in", "equals", "contains"))
    stop("Match must be one of in, equals, contains")
}

valid_var <- function(var, table){
  vars <- secop_dics(table = table)[,"fieldName"]
  if(!var %in% vars) stop("Var not in ", table, " dictionary.")
}

where_query <- function(table = "secop1"){
  url <- soc_resource_url(dataset_id)
  paste0(url,"?$where=")
}

select_query <- function(dataset_id){
  url <- soc_resource_url(dataset_id)
  paste0(url,"?$select=")
}





